package lgbt.princess.lifts.laws

import cats.{Eq, Functor, Monad, Monoid, Show}
import cats.data.{EitherT, IorT, Kleisli, OptionT, RWST, StateT, WriterT}
import cats.syntax.functor._
import cats.syntax.show._
import lgbt.princess.lifts.Identity

/** Un-lifts the higher-kinded type `From` to the higher-kinded type `To`. */
trait Unlift[From[_], To[_]] {
  def functor: Functor[To]

  /**
   * Un-lifts `G[A]` to `F[A]`, if possible.
   *
   * @return
   *   a [[Unlift.Result `Result`]] containing [[scala.Right `Right`]] of the value if it was
   *   successfully un-lifted, or [[scala.Left `Left`]] of a [[Unlift.Failure `Failure`]] otherwise
   */
  def unlift[A](value: From[A]): Unlift.Result[To, A]

  /** @return an instance that un-lifts `From` to `To` and then `To` to `Last` */
  final def andThen[Last[_]](that: Unlift[To, Last]): Unlift[From, Last] =
    Unlift.Composed(this, that)

  /** @return an instance that un-lifts `First` to `From` and then `From` to `To` */
  final def compose[First[_]](that: Unlift[First, From]): Unlift[First, To] =
    Unlift.Composed(that, this)
}

object Unlift {
  private[this] final class Composed[From[_], Middle[_], To[_]] private (
      inner: Unlift[From, Middle],
      outer: Unlift[Middle, To]
  ) extends Unlift[From, To] {
    implicit def functor: Functor[To] = outer.functor
    def unlift[A](value: From[A]): Result[To, A] =
      outer
        .unlift(inner.unlift(value).value)
        .subflatMap(identity)
  }

  private object Composed {
    def apply[From[_], Middle[_], To[_]](
        inner: Unlift[From, Middle],
        outer: Unlift[Middle, To]
    ): Unlift[From, To] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[Unlift[From, To]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[Unlift[From, To]]
      else new Composed(inner, outer)
  }

  type Derived[Wrapper[_[_], _], To[_]] = Unlift[Wrapper[To, *], To]

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
    new Unlift[F, F] with Identity {
      val functor: Functor[F] = F
      def unlift[A](value: F[A]): Result[F, A] = success(value)
    }

  implicit def optionT[From[_], To[_]](implicit
      From: Functor[From],
      outer: Unlift[From, To]
  ): Unlift[OptionT[From, *], To] =
    outer.compose {
      new Unlift[OptionT[From, *], From] {
        val functor: Functor[From] = From
        def unlift[A](value: OptionT[From, A]): Result[From, A] =
          value.toRight(Failure("OptionT.empty"))
      }
    }

  implicit def eitherT[From[_], To[_], L: Show](implicit
      From: Functor[From],
      outer: Unlift[From, To]
  ): Unlift[EitherT[From, L, *], To] =
    outer.compose {
      new Unlift[EitherT[From, L, *], From] {
        val functor: Functor[From] = From
        def unlift[A](value: EitherT[From, L, A]): Result[From, A] =
          value.leftMap(left => Failure(s"Left(${left.show})"))
      }
    }

  implicit def iorT[From[_], To[_], L: Show](implicit
      From: Functor[From],
      outer: Unlift[From, To]
  ): Unlift[IorT[From, L, *], To] =
    outer.compose {
      new Unlift[IorT[From, L, *], From] {
        val functor: Functor[From] = From
        def unlift[A](value: IorT[From, L, A]): Result[From, A] =
          value.toEither.leftMap(left => Failure(s"Ior.Left(${left.show})"))
      }
    }

  implicit def kleisli[From[_], To[_], A: Monoid](implicit
      From: Functor[From],
      outer: Unlift[From, To]
  ): Unlift[Kleisli[From, A, *], To] =
    outer.compose {
      new Unlift[Kleisli[From, A, *], From] {
        val functor: Functor[From] = From
        def unlift[B](value: Kleisli[From, A, B]): Result[From, B] =
          success(value.run(Monoid[A].empty))
      }
    }

  implicit def writerT[From[_], To[_], L](implicit
      From: Functor[From],
      outer: Unlift[From, To]
  ): Unlift[WriterT[From, L, *], To] =
    outer.compose {
      new Unlift[WriterT[From, L, *], From] {
        val functor: Functor[From] = From
        def unlift[A](value: WriterT[From, L, A]): Result[From, A] =
          success(value.value)
      }
    }

  implicit def stateT[From[_], To[_], S: Monoid](implicit
      From: Monad[From],
      outer: Unlift[From, To]
  ): Unlift[StateT[From, S, *], To] =
    outer.compose {
      new Unlift[StateT[From, S, *], From] {
        val functor: Functor[From] = From
        def unlift[A](value: StateT[From, S, A]): Result[From, A] =
          success(value.run(Monoid[S].empty).map(_._2))
      }
    }

  implicit def rwst[From[_], To[_], E: Monoid, L, S: Monoid](implicit
      From: Monad[From],
      outer: Unlift[From, To]
  ): Unlift[RWST[From, E, L, S, *], To] =
    outer.compose {
      new Unlift[RWST[From, E, L, S, *], From] {
        val functor: Functor[From] = From
        def unlift[A](value: RWST[From, E, L, S, A]): Result[From, A] =
          success(value.run(Monoid[E].empty, Monoid[S].empty).map(_._3))
      }
    }
}
