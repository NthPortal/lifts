package lgbt.princess.lifts.laws

import cats.{Eq, Functor, Monad, Monoid, Show}
import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.syntax.functor._
import cats.syntax.show._
import lgbt.princess.lifts.Identity

/** Un-lifts the higher-kinded type `G` to the higher-kinded type `F`. */
trait Unlift[G[_], F[_]] {
  def functor: Functor[F]

  /**
   * Un-lifts `G[A]` to `F[A]`, if possible.
   *
   * @return
   *   a [[Unlift.Result `Result`]] containing [[scala.Right `Right`]] of the value if it was
   *   successfully un-lifted, or [[scala.Left `Left`]] of a [[Unlift.Failure `Failure`]] otherwise
   */
  def unlift[A](value: G[A]): Unlift.Result[F, A]

  /** @return an instance that un-lifts `G` to `F` and then `F` to `E` */
  final def andThen[E[_]](that: Unlift[F, E]): Unlift[G, E] =
    Unlift.Composed(this, that)

  /** @return an instance that un-lifts `H` to `G` and then `G` to `F` */
  final def compose[H[_]](that: Unlift[H, G]): Unlift[H, F] =
    Unlift.Composed(that, this)
}

object Unlift {
  private[this] final class Composed[H[_], G[_], F[_]] private (
      inner: Unlift[H, G],
      outer: Unlift[G, F]
  ) extends Unlift[H, F] {
    implicit def functor: Functor[F] = outer.functor
    def unlift[A](value: H[A]): Result[F, A] =
      outer
        .unlift(inner.unlift(value).value)
        .subflatMap(identity)
  }

  private object Composed {
    def apply[H[_], G[_], F[_]](inner: Unlift[H, G], outer: Unlift[G, F]): Unlift[H, F] =
      if (outer.isInstanceOf[Identity]) inner.asInstanceOf[Unlift[H, F]]
      else if (inner.isInstanceOf[Identity]) outer.asInstanceOf[Unlift[H, F]]
      else new Composed(inner, outer)
  }

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

  implicit def optionT[G[_], F[_]](implicit
      G: Functor[G],
      outer: Unlift[G, F]
  ): Unlift[OptionT[G, *], F] =
    outer.compose {
      new Unlift[OptionT[G, *], G] {
        val functor: Functor[G] = G
        def unlift[A](value: OptionT[G, A]): Result[G, A] =
          value.toRight(Failure("OptionT.empty"))
      }
    }

  implicit def eitherT[G[_], F[_], L: Show](implicit
      G: Functor[G],
      outer: Unlift[G, F]
  ): Unlift[EitherT[G, L, *], F] =
    outer.compose {
      new Unlift[EitherT[G, L, *], G] {
        val functor: Functor[G] = G
        def unlift[A](value: EitherT[G, L, A]): Result[G, A] =
          value.leftMap(left => Failure(s"Left(${left.show})"))
      }
    }

  implicit def iorT[G[_], F[_], L: Show](implicit
      G: Functor[G],
      outer: Unlift[G, F]
  ): Unlift[IorT[G, L, *], F] =
    outer.compose {
      new Unlift[IorT[G, L, *], G] {
        val functor: Functor[G] = G
        def unlift[A](value: IorT[G, L, A]): Result[G, A] =
          value.toEither.leftMap(left => Failure(s"Ior.Left(${left.show})"))
      }
    }

  implicit def kleisli[G[_], F[_], A: Monoid](implicit
      G: Functor[G],
      outer: Unlift[G, F]
  ): Unlift[Kleisli[G, A, *], F] =
    outer.compose {
      new Unlift[Kleisli[G, A, *], G] {
        val functor: Functor[G] = G
        def unlift[B](value: Kleisli[G, A, B]): Result[G, B] =
          success(value.run(Monoid[A].empty))
      }
    }

  implicit def stateT[G[_], F[_], S: Monoid](implicit
      G: Monad[G],
      outer: Unlift[G, F]
  ): Unlift[StateT[G, S, *], F] =
    outer.compose {
      new Unlift[StateT[G, S, *], G] {
        val functor: Functor[G] = G
        def unlift[A](value: StateT[G, S, A]): Result[G, A] =
          success(value.run(Monoid[S].empty).map(_._2))
      }
    }

  implicit def writerT[G[_], F[_], L](implicit
      G: Functor[G],
      outer: Unlift[G, F]
  ): Unlift[WriterT[G, L, *], F] =
    outer.compose {
      new Unlift[WriterT[G, L, *], G] {
        val functor: Functor[G] = G
        def unlift[A](value: WriterT[G, L, A]): Result[G, A] =
          success(value.value)
      }
    }
}
