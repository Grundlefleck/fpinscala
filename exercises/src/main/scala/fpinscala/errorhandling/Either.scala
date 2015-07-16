package fpinscala.errorhandling



import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e) => Left(e)
   case Right(e) => Right(f(e))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(e) => Left(e)
   case Right(e) => f(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(e) => Right(e)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   this flatMap { aa =>
     b map { bb => f(aa, bb)}
   }

   for {
     aa <- this
     bb <- b
   } yield f(aa, bb)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h :: t =>
        val eitherOfHead: Either[E, B] = f(h)
        val eitherOfTail: Either[E, List[B]] = traverse(t)(f)
        eitherOfHead.map2(eitherOfTail)(_ :: _)
    }
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    traverse(es)(e => e)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}


object Tester {
  def main(args: Array[String]): Unit = {
    def testTraverse[A, B, C](a: List[A], f: A => Either[B, C]): Unit = {
      printf("Traversing: %n    %s%nResults in: %n    %s%n%n".format(a, Either.traverse(a)(f)))
    }

    def stringToEither: (String) => Either[Throwable, Int] = (i) => {
      try {
        Right(i.toInt)
      } catch {
        case t: Exception => Left(t)
      }
    }

    testTraverse[String, Throwable, Int](List("1", "2", "3"), stringToEither)
    testTraverse[String, Throwable, Int](List(), stringToEither)
    testTraverse[String, Throwable, Int](List("1", "2", "bob"), stringToEither)
    testTraverse[String, Throwable, Int](List("1", "2", "bob", "sue"), stringToEither)
  }
}
