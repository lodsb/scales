/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-11-07 :: 17:46
    >>  Origin:
    >>
  +3>>
    >>  Copyright (c) 2013:
    >>
    >>     |             |     |
    >>     |    ,---.,---|,---.|---.
    >>     |    |   ||   |`---.|   |
    >>     `---'`---'`---'`---'`---'
    >>                    // Niklas Klügel
    >>
  +4>>
    >>  Made in Bavaria by fat little elves - since 1983.
 */

// what about octaves? should be included?
// better to see Pitches as contour step?
sealed abstract class Movement[A <: Pitched](from: A, to: A)
case class UpMovement(from: Pitch, to: Pitch) extends Movement(from, to)
case class DownMovement(from: Pitch, to: Pitch) extends Movement(from, to)
case class StraightMovement(from: Pitch, to: Pitch) extends Movement(from, to)


//object

// better p and down movement

/*
case class MovementWithOctave(from: Pitch, to: Pitch) {
  // hackish, how to resolve this?
  def direction: Int = (from.number + (from.octave*1000)).compare(to.number + (to.octave*1000))
}
*/


//contour is given a movementfunctionthingy to convert to some movement description
import collection.generic.CanBuildFrom
import collection.immutable.VectorBuilder
import collection.IndexedSeqLike
import collection.mutable.Builder

object Contour {
  def apply[P <: Pitched](Ps: P*) = this.fromSeq(Ps.toVector)
  //def apply[P <: PitchBase](seq: Seq[P]) = PitchedVector.fromSeq(this.makeDistinct(seq).toVector)

  protected[Contour] def makeDistinct[P <: Pitched](seq: Vector[P]) : Vector[P] = {
    var ret = Vector[P]()

    seq.foreach {
      x =>
        if (!ret.contains(x)) {
          ret = ret :+ x
        }
    }

    ret
  }

  def fromSeq[P <: Pitched](buf: IndexedSeq[P]): Contour[P] =
    new Contour[P](makeDistinct(buf.toVector))


  def newBuilder[P <: Pitched]: Builder[P, Contour[P]] =
    new VectorBuilder mapResult fromSeq

  implicit def canBuildFrom[P <: Pitched,From]:
  CanBuildFrom[Contour[_], P, Contour[P]] =
    new CanBuildFrom[Contour[_], P, Contour[P]] {
      def apply(): Builder[P, Contour[P]] = Contour.newBuilder
      def apply(from: Contour[_]): Builder[P, Contour[P]] =
        Contour.newBuilder
    }
}

class Contour[P <: Pitched] protected (buf: Vector[P])
  extends IndexedSeq[P]
  with IndexedSeqLike[P, Contour[P]] {

  val buffer = processVector(buf)

  def apply(idx: Int): P = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    buffer(idx)
  }

  def length = buffer.length
  protected def processVector(buf: Vector[P]): Vector[P] = Contour.makeDistinct(buf)

  override protected[this] def newBuilder: Builder[P, Contour[P]] = Contour.newBuilder
}
