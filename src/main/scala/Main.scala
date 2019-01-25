object Main extends App {
  trait PieT

  final case class Atom(s: Symbol) extends PieT {
    type AtomT = this.type
    def apply(s: Symbol): Atom#AtomT = {
      Atom(s)
    }
    override def toString: String = s.toString
  }

  trait Pair[L <: PieT, R <: PieT] extends PieT {
    def left: L = this match {
      case Cons(left, _) => left
    }
    def right: R = this match {
      case Cons(_, right) => right
    }
  }
  final case class Cons[L <: PieT, R <: PieT](l: L, r: R) extends Pair[L, R]
  final def Car[T <: PieT](pair: Pair[T, _ <: PieT]): T = pair.left
  final def Cdr[T <: PieT](pair: Pair[_ <: PieT, T]): T = pair.right

  trait Nat extends PieT {
    @scala.annotation.tailrec
    final def toInt(agg: Int = 0): Int = this match {
      case Zero => agg
      case add1(lessone) => lessone.toInt(agg + 1)
    }
    final def + (that: Nat): Nat = IterNat(this, that, add1)
  }
  object Nat extends Nat{
    def apply(i: Int): Nat = {
      def recurse(n: Nat): Nat = n.toInt() match {
        case int if int == i => n: Nat
        case _ => recurse(add1(n))
      }
      recurse(Zero)
    }
  }

  final object Zero extends Nat
  final case class add1(n: Nat) extends Nat
  final def WhichNat[R <: PieT](target: Nat, base: R, step: Nat => R): R = target match {
    case Zero => base
    case add1(lessone) => step(lessone)
  }
  final def IterNat[R <: PieT](target: Nat, base: R, step: R => R): R = target match {
    case Zero => base
    case add1(lessone) => step(IterNat(lessone, base, step))
  }

  val One = add1(Zero)
  val Two = add1(One)
  val Three = add1(Two)
  val Seventeen = Nat(17)

  // if a claim is of type U, we use a type declaration
  final type Pear = Pair[Nat, Nat]
  final type PearMaker = (Nat, Nat) => Pear

  final def ElimPear(pear: Pear, maker: PearMaker): Pear = maker(Car(pear), Cdr(pear))

  val p: Pear = Cons(One, Two)
  val v: Pair[Nat, PieT] = Cons(One, Nat)
  val x: Pair[Atom, Atom] = Cons(Atom('Hello), Atom('World))

  println(One + Two) // should be 3
  println(WhichNat(Nat(5), Zero, Nat(6) + _)) //p49: should be 10
  println(ElimPear(Cons(Three, Seventeen), Cons(_, _))) //p58: should be Cons(3, 17)

}
