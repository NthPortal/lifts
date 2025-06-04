package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, WriterT}
import cats.kernel.Monoid
import cats.{Applicative, Functor, ~>}

trait LiftKind2[In1[_], Out1[_], In2[_], Out2[_]] extends MapK[In1, Out1, In2, Out2] {
  def liftValueInput: LiftValue[In1, In2]
  def liftValueOutput: LiftValue[Out1, Out2]

  def andThen[In3[_], Out3[_]](
      that: LiftKind2[In2, Out2, In3, Out3]
  ): LiftKind2[In1, Out1, In3, Out3] =
    LiftKind2.Composed(this, that)

  def compose[In0[_], Out0[_]](
      that: LiftKind2[In0, Out0, In1, Out1]
  ): LiftKind2[In0, Out0, In2, Out2] =
    LiftKind2.Composed(that, this)
}

object LiftKind2 {

  private[this] final class Composed[In1[_], Out1[_], In2[_], Out2[_], In3[_], Out3[_]] private (
      inner: LiftKind2[In1, Out1, In2, Out2],
      outer: LiftKind2[In2, Out2, In3, Out3],
  ) extends LiftKind2[In1, Out1, In3, Out3] {
    val liftValueInput: LiftValue[In1, In3] =
      inner.liftValueInput.andThen(outer.liftValueInput)
    val liftValueOutput: LiftValue[Out1, Out3] =
      inner.liftValueOutput.andThen(outer.liftValueOutput)
    def mapK[A](value: In3[A])(f: In1 ~> Out1): Out3[A] =
      outer.mapK(value)(inner.liftFunctionK(f))
    override def liftFunctionK(f: In1 ~> Out1): In3 ~> Out3 =
      outer.liftFunctionK(inner.liftFunctionK(f))
  }

  private object Composed {
    def apply[In1[_], Out1[_], In2[_], Out2[_], In3[_], Out3[_]](
        inner: LiftKind2[In1, Out1, In2, Out2],
        outer: LiftKind2[In2, Out2, In3, Out3],
    ): LiftKind2[In1, Out1, In3, Out3] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftKind2[In1, Out1, In3, Out3]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftKind2[In1, Out1, In3, Out3]]
      else new Composed(inner, outer)
  }

  type Derived[In[_], Out[_], Wrapper[_[_], _]] =
    LiftKind2[In, Out, Wrapper[In, *], Wrapper[Out, *]]

  def apply[In1[_], Out1[_], In2[_], Out2[_]](implicit
      ev: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, In2, Out2] = ev

  implicit def id[In[_], Out[_]]: LiftKind2[In, Out, In, Out] =
    new LiftKind2[In, Out, In, Out] with Identity {
      val liftValueInput: LiftValue[In, In] = LiftValue.id
      val liftValueOutput: LiftValue[Out, Out] = LiftValue.id
      def mapK[A](value: In[A])(f: In ~> Out): Out[A] = f(value)
      override def liftFunctionK(f: In ~> Out): In ~> Out = f
    }

  implicit def optionT[In1[_], Out1[_], In2[_]: Functor, Out2[_]: Functor](implicit
      inner: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, OptionT[In2, *], OptionT[Out2, *]] =
    inner.andThen {
      new LiftKind2[In2, Out2, OptionT[In2, *], OptionT[Out2, *]] {
        val liftValueInput: LiftValue[In2, OptionT[In2, *]] = LiftValue.optionT
        val liftValueOutput: LiftValue[Out2, OptionT[Out2, *]] = LiftValue.optionT
        def mapK[A](value: OptionT[In2, A])(f: In2 ~> Out2): OptionT[Out2, A] =
          value.mapK(f)
      }
    }

  implicit def eitherT[In1[_], Out1[_], In2[_]: Functor, Out2[_]: Functor, L](implicit
      inner: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, EitherT[In2, L, *], EitherT[Out2, L, *]] =
    inner.andThen {
      new LiftKind2[In2, Out2, EitherT[In2, L, *], EitherT[Out2, L, *]] {
        val liftValueInput: LiftValue[In2, EitherT[In2, L, *]] = LiftValue.eitherT
        val liftValueOutput: LiftValue[Out2, EitherT[Out2, L, *]] = LiftValue.eitherT
        def mapK[A](value: EitherT[In2, L, A])(f: In2 ~> Out2): EitherT[Out2, L, A] =
          value.mapK(f)
      }
    }

  implicit def iorT[In1[_], Out1[_], In2[_]: Functor, Out2[_]: Functor, L](implicit
      inner: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, IorT[In2, L, *], IorT[Out2, L, *]] =
    inner.andThen {
      new LiftKind2[In2, Out2, IorT[In2, L, *], IorT[Out2, L, *]] {
        val liftValueInput: LiftValue[In2, IorT[In2, L, *]] = LiftValue.iorT
        val liftValueOutput: LiftValue[Out2, IorT[Out2, L, *]] = LiftValue.iorT
        def mapK[A](value: IorT[In2, L, A])(f: In2 ~> Out2): IorT[Out2, L, A] =
          value.mapK(f)
      }
    }

  implicit def kleisli[In1[_], Out1[_], In2[_], Out2[_], A](implicit
      inner: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, Kleisli[In2, A, *], Kleisli[Out2, A, *]] =
    inner.andThen {
      new LiftKind2[In2, Out2, Kleisli[In2, A, *], Kleisli[Out2, A, *]] {
        val liftValueInput: LiftValue[In2, Kleisli[In2, A, *]] = LiftValue.kleisli
        val liftValueOutput: LiftValue[Out2, Kleisli[Out2, A, *]] = LiftValue.kleisli
        def mapK[B](value: Kleisli[In2, A, B])(f: In2 ~> Out2): Kleisli[Out2, A, B] =
          value.mapK(f)
      }
    }

  implicit def writerT[In1[_], Out1[_], In2[_]: Applicative, Out2[_]: Applicative, L: Monoid](
      implicit inner: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, WriterT[In2, L, *], WriterT[Out2, L, *]] =
    inner.andThen {
      new LiftKind2[In2, Out2, WriterT[In2, L, *], WriterT[Out2, L, *]] {
        val liftValueInput: LiftValue[In2, WriterT[In2, L, *]] = LiftValue.writerT
        val liftValueOutput: LiftValue[Out2, WriterT[Out2, L, *]] = LiftValue.writerT
        def mapK[A](value: WriterT[In2, L, A])(f: In2 ~> Out2): WriterT[Out2, L, A] =
          value.mapK(f)
      }
    }
}
