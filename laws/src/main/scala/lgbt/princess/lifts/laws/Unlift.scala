package lgbt.princess.lifts.laws

import cats.{Eq, Functor, Monad, Monoid, Show}
import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.syntax.functor._
import cats.syntax.show._

/** Un-lifts the higher-kinded type `G` to the higher-kinded type `F`. */
trait Unlift[F[_], G[_]] {
  def functor: Functor[F]

  /**
   * Un-lifts `G[A]` to `F[A]`, if possible.
   *
   * @return
   *   a [[Unlift.Result `Result`]] containing [[scala.Right `Right`]] of the value if it was
   *   successfully un-lifted, or [[scala.Left `Left`]] of a [[Unlift.Failure `Failure`]] otherwise
   */
  def unlift[A](value: G[A]): Unlift.Result[F, A]
}

object Unlift {

  /** A description of why a value could not be un-lifted. */
  final case class Failure(description: String)

  object Failure {
    implicit val eqInstance: Eq[Failure] = Eq.by(_.description)
  }

  /**
   * The result of an attempt to un-lift a value, containing either the un-lifted value or a
   * description of why it could not be un-lifted.
   */
  type Result[F[_], A] = EitherT[F, Failure, A]

  /** @return the successful result of un-lifting a value, as a [[`Result`]] */
  def success[F[_]: Functor, A](value: F[A]): Result[F, A] =
    EitherT.liftF(value)

  implicit def id[F[_]](implicit F: Functor[F]): Unlift[F, F] =
    new Unlift[F, F] {
      val functor: Functor[F] = F
      def unlift[A](value: F[A]): Result[F, A] =
        success(value)
    }

  implicit def optionT[F[_]](implicit F: Functor[F]): Unlift[F, OptionT[F, *]] =
    new Unlift[F, OptionT[F, *]] {
      val functor: Functor[F] = F
      def unlift[A](value: OptionT[F, A]): Result[F, A] =
        value.toRight(Failure("OptionT.empty"))
    }

  implicit def eitherT[F[_], L: Show](implicit F: Functor[F]): Unlift[F, EitherT[F, L, *]] =
    new Unlift[F, EitherT[F, L, *]] {
      val functor: Functor[F] = F
      def unlift[A](value: EitherT[F, L, A]): Result[F, A] =
        value.leftMap(left => Failure(s"Left(${left.show})"))
    }

  implicit def iorT[F[_], L: Show](implicit F: Functor[F]): Unlift[F, IorT[F, L, *]] =
    new Unlift[F, IorT[F, L, *]] {
      val functor: Functor[F] = F
      def unlift[A](value: IorT[F, L, A]): Result[F, A] =
        value.toEither.leftMap(left => Failure(s"Ior.Left(${left.show})"))
    }

  implicit def kleisli[F[_], A: Monoid](implicit F: Functor[F]): Unlift[F, Kleisli[F, A, *]] =
    new Unlift[F, Kleisli[F, A, *]] {
      val functor: Functor[F] = F
      def unlift[B](value: Kleisli[F, A, B]): Result[F, B] =
        success(value.run(Monoid[A].empty))
    }

  implicit def stateT[F[_], S: Monoid](implicit F: Monad[F]): Unlift[F, StateT[F, S, *]] =
    new Unlift[F, StateT[F, S, *]] {
      val functor: Functor[F] = F
      def unlift[A](value: StateT[F, S, A]): Result[F, A] =
        success(value.run(Monoid[S].empty).map(_._2))
    }

  implicit def writerT[F[_], L](implicit F: Functor[F]): Unlift[F, WriterT[F, L, *]] =
    new Unlift[F, WriterT[F, L, *]] {
      val functor: Functor[F] = F
      def unlift[A](value: WriterT[F, L, A]): Result[F, A] =
        success(value.value)
    }
}
