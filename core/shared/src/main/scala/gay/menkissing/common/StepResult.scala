package gay.menkissing.common

import cats.*

// Why?
// Because I find it confusing the difference between tailRecM's use of either (Right is done, Left is continue)
// and the normal monadic use of either (Left is done, Right is continue)
// so i am wrapping the instance with better named types

type StepResult[+A] = StepResult.Type[A]

object Done:
  def apply[A](v: A): StepResult[A] = StepResult.makeDone(v)
object Continue:
  def apply[A](v: A): StepResult[A] = StepResult.makeContinue(v)

object StepResult:
  opaque type Type[+B] <: Either[B, B] = Either[B, B]
  def makeDone[A](v: A): Type[A] = Left(v)
  def makeContinue[A](v: A): Type[A] = Right(v)

  // ???????????????
  // should be ok even though we are making an invalid instance of the other summoned typeclasses?
  // god is dead
  given monadForStepResult: Monad[Type] =
    Monad[Either[Any, _]].asInstanceOf[Monad[Type]]
