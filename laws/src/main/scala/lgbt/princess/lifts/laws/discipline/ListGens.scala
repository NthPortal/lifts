package lgbt.princess.lifts.laws.discipline

import cats.~>
import org.scalacheck.Gen

private[discipline] object ListGens {
  private[this] val smallPositiveInt: Gen[Int] = Gen.oneOf(1 to 5)

  private[this] val genListDrop: Gen[List ~> List] =
    for (count <- smallPositiveInt)
    yield new (List ~> List) {
      def apply[A](list: List[A]): List[A] = list.drop(count)
      override def toString: String = s"List#drop($count)"
    }

  private[this] val genListTake: Gen[List ~> List] =
    for (count <- smallPositiveInt)
    yield new (List ~> List) {
      def apply[A](list: List[A]): List[A] = list.take(count)
      override def toString: String = s"List#take($count)"
    }

  private[this] val genListSlice: Gen[List ~> List] =
    for {
      start <- smallPositiveInt
      offset <- smallPositiveInt
    } yield new (List ~> List) {
      def apply[A](list: List[A]): List[A] =
        list.slice(start, start + offset)
      override def toString: String = s"List#slice($start, $offset)"
    }

  private[this] val genListFilterEvenOrOddIndices: Gen[List ~> List] =
    for (remainder <- Gen.oneOf(0, 1))
    yield new (List ~> List) {
      def apply[A](list: List[A]): List[A] =
        list.view.zipWithIndex.filter(_._2 % 2 == remainder).map(_._1).toList
      override def toString: String =
        s"List#filter(<index is ${if (remainder == 0) "even" else "odd"}>)"
    }

  val genFunctionKListList: Gen[List ~> List] =
    Gen.oneOf(
      genListDrop,
      genListTake,
      genListSlice,
      genListFilterEvenOrOddIndices
    )
}
