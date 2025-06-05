package lgbt.princess.lifts.instances

import cats.data.OptionT
import cats.effect.IO
import fs2.Stream
import lgbt.princess.lifts._
import lgbt.princess.lifts.instances.fs2support._

object Fs2ImplicitSummoningTest {

  def `Stream instances for IO`(): Unit = {
    LiftValue[IO, Stream[IO, *]]
    LiftScope[IO, Stream[IO, *]]
    LiftKind[IO, Stream[IO, *]]
    MapK[IO, OptionT[IO, *], Stream[IO, *], Stream[OptionT[IO, *], *]]
    LiftScopeAlt[IO, Stream[IO, *]]
    LiftKind1[IO, Stream[IO, *]]
    LiftKind2[IO, OptionT[IO, *], Stream[IO, *], Stream[OptionT[IO, *], *]]
  }

  def `Stream instances for abstract types`[F[_], G[_]](): Unit = {
    LiftValue[F, Stream[F, *]]
    LiftScope[F, Stream[F, *]]
    LiftKind[F, Stream[F, *]]
    MapK[F, G, Stream[F, *], Stream[G, *]]
    LiftScopeAlt[F, Stream[F, *]]
    LiftKind1[F, Stream[F, *]]
    LiftKind2[F, G, Stream[F, *], Stream[G, *]]
  }

  def `LiftKind1 from LiftKind2 for Stream`[F[_], G[_]]()(implicit
      lk2: LiftKind2[F, F, Stream[G, *], Stream[G, *]]
  ): Unit = {
    LiftKind1[F, Stream[G, *]]
  }
}
