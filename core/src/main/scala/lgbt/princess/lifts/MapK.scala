package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Functor, ~>}

trait MapK[In1[_], Out1[_], In2[_], Out2[_]] {
  def mapK[A](value: In2[A])(f: In1 ~> Out1): Out2[A]

  def liftFunctionK(f: In1 ~> Out1): In2 ~> Out2 =
    new (In2 ~> Out2) {
      def apply[A](fa: In2[A]): Out2[A] = mapK(fa)(f)
    }

  def andThen[In3[_], Out3[_]](that: MapK[In2, Out2, In3, Out3]): MapK[In1, Out1, In3, Out3] =
    MapK.Composed(this, that)

  def compose[In0[_], Out0[_]](that: MapK[In0, Out0, In1, Out1]): MapK[In0, Out0, In2, Out2] =
    MapK.Composed(that, this)
}

object MapK {

  type Derived[In[_], Out[_], Wrapper[_[_], _]] =
    MapK[In, Out, Wrapper[In, *], Wrapper[Out, *]]

  def apply[In1[_], Out1[_], In2[_], Out2[_]](implicit
      ev: MapK[In1, Out1, In2, Out2]
  ): MapK[In1, Out1, In2, Out2] = ev

  private[lifts] trait Identity[In[_], Out[_]] extends MapK[In, Out, In, Out] {
    def mapK[A](value: In[A])(f: In ~> Out): Out[A] = f(value)
    override def liftFunctionK(f: In ~> Out): In ~> Out = f
  }

  private[this] val _identity =
    new Identity[({ type L[_] = Any })#L, ({ type L[_] = Any })#L] {}

  private[this] final class Composed[In1[_], Out1[_], In2[_], Out2[_], In3[_], Out3[_]] private (
      inner: MapK[In1, Out1, In2, Out2],
      outer: MapK[In2, Out2, In3, Out3],
  ) extends MapK[In1, Out1, In3, Out3] {
    def mapK[A](value: In3[A])(f: In1 ~> Out1): Out3[A] =
      outer.mapK(value)(inner.liftFunctionK(f))
    override def liftFunctionK(f: In1 ~> Out1): In3 ~> Out3 =
      outer.liftFunctionK(inner.liftFunctionK(f))
  }

  private object Composed {
    def apply[In1[_], Out1[_], In2[_], Out2[_], In3[_], Out3[_]](
        inner: MapK[In1, Out1, In2, Out2],
        outer: MapK[In2, Out2, In3, Out3],
    ): MapK[In1, Out1, In3, Out3] = {
      if (inner.isInstanceOf[Identity[In1, Out1]]) outer.asInstanceOf[MapK[In1, Out1, In3, Out3]]
      else if (outer.isInstanceOf[Identity[In2, Out2]])
        inner.asInstanceOf[MapK[In1, Out1, In3, Out3]]
      else new Composed(inner, outer)
    }
  }

  implicit def id[In[_], Out[_]]: MapK[In, Out, In, Out] =
    _identity.asInstanceOf[Identity[In, Out]]

  implicit def eitherT[In1[_], Out1[_], In2[_], Out2[_], L](implicit
      inner: MapK[In1, Out1, In2, Out2]
  ): MapK[In1, Out1, EitherT[In2, L, *], EitherT[Out2, L, *]] =
    inner.andThen {
      new MapK[In2, Out2, EitherT[In2, L, *], EitherT[Out2, L, *]] {
        def mapK[A](value: EitherT[In2, L, A])(f: In2 ~> Out2): EitherT[Out2, L, A] =
          value.mapK(f)
      }
    }

  implicit def iorT[In1[_], Out1[_], In2[_], Out2[_], L](implicit
      inner: MapK[In1, Out1, In2, Out2]
  ): MapK[In1, Out1, IorT[In2, L, *], IorT[Out2, L, *]] =
    inner.andThen {
      new MapK[In2, Out2, IorT[In2, L, *], IorT[Out2, L, *]] {
        def mapK[A](value: IorT[In2, L, A])(f: In2 ~> Out2): IorT[Out2, L, A] =
          value.mapK(f)
      }
    }

  implicit def kleisli[In1[_], Out1[_], In2[_], Out2[_], A](implicit
      inner: MapK[In1, Out1, In2, Out2]
  ): MapK[In1, Out1, Kleisli[In2, A, *], Kleisli[Out2, A, *]] =
    inner.andThen {
      new MapK[In2, Out2, Kleisli[In2, A, *], Kleisli[Out2, A, *]] {
        def mapK[B](value: Kleisli[In2, A, B])(f: In2 ~> Out2): Kleisli[Out2, A, B] =
          value.mapK(f)
      }
    }

  implicit def optionT[In1[_], Out1[_], In2[_], Out2[_]](implicit
      inner: MapK[In1, Out1, In2, Out2]
  ): MapK[In1, Out1, OptionT[In2, *], OptionT[Out2, *]] =
    inner.andThen {
      new MapK[In2, Out2, OptionT[In2, *], OptionT[Out2, *]] {
        def mapK[A](value: OptionT[In2, A])(f: In2 ~> Out2): OptionT[Out2, A] =
          value.mapK(f)
      }
    }

  implicit def writerT[In1[_], Out1[_], In2[_], Out2[_], L](implicit
      inner: MapK[In1, Out1, In2, Out2]
  ): MapK[In1, Out1, WriterT[In2, L, *], WriterT[Out2, L, *]] =
    inner.andThen {
      new MapK[In2, Out2, WriterT[In2, L, *], WriterT[Out2, L, *]] {
        def mapK[A](value: WriterT[In2, L, A])(f: In2 ~> Out2): WriterT[Out2, L, A] =
          value.mapK(f)
      }
    }

  object MonadMorphismOnly {
    implicit def stateT[In1[_], Out1[_], In2[_]: Functor, Out2[_], S](implicit
        inner: MapK[In1, Out1, In2, Out2]
    ): MapK[In1, Out1, StateT[In2, S, *], StateT[Out2, S, *]] =
      inner.andThen {
        new MapK[In2, Out2, StateT[In2, S, *], StateT[Out2, S, *]] {
          def mapK[A](value: StateT[In2, S, A])(f: In2 ~> Out2): StateT[Out2, S, A] =
            value.mapK(f)
        }
      }
  }
}
