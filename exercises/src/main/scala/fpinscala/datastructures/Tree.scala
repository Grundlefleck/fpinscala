package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)((acc, extraSize) => acc + extraSize)

  def maximum(t: Tree[Int]): Int = fold[Int, Int](t)(a => a)((elem, acc) => elem.max(acc))

  def depth[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)((l, r) => 1 + (l max r))

  def map[A, B](t: Tree[A])(f: (A) => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}

object TreeTest {
  def main(args: Array[String]): Unit = {
    def testSize(tree: Tree[_]): Unit = {
      println(s"Size: ${Tree.size(tree)}, Tree: ${tree}")
    }

    def testMax(tree: Tree[Int]): Unit = {
      println(s"Max: ${Tree.maximum(tree)}, Tree: ${tree}")
    }

    def testDepth(tree: Tree[Int]): Unit = {
      println(s"Depth: ${Tree.depth(tree)}, Tree: ${tree}")
    }

    def testMap(tree: Tree[Int]): Unit = {
      println(s"Times 2: ${Tree.map(tree)(_ * 2)}, Tree: ${tree}")
    }



    testMap(Leaf[Int](1))

    testMap(Branch(Leaf(1),
                     Branch(Leaf(4), Leaf(3))))
    testMap(Branch(
                Branch(
                  Leaf(1),
                  Leaf(2)),
                Branch(
                  Leaf(4),
                  Branch(
                    Leaf(3),
                    Branch(
                      Leaf(6),
                      Leaf(7))))))
  }
}
