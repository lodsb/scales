import collection.{immutable, IndexedSeqLike}
import scala.collection.immutable.{BitSet}
import scala.collection.mutable.{ArrayBuffer,ListBuffer, Builder}
import scala.collection.generic._
import scala.collection.immutable.VectorBuilder

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-11-06 :: 00:22
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


// transpose -> no octave: shift, octave: rotate
// num steps

// scale == idx to tuning
// pitch & chord == idx to scale

//TODO: fix seq-like

class Scale protected (private val buffer: BitSet,
                       private val cyclicOctaveSteps: Option[Int],
                       private val root: Int)
  //extends IndexedSeq[Boolean]
  /*with IndexedSeqLike[Boolean, Scale] */{

  /*
  override protected[this] def newBuilder: Builder[Boolean, Scale] =
    Scale.newBuilder
*/

  def isInScale(note: Int): Boolean = {
    // positive and negative indices...
    var idx = note
    var ret = false

    if(cyclicOctaveSteps.isDefined) {
      val cycle = cyclicOctaveSteps.get
      idx = idx % cycle

      if(idx < 0) {
        idx = cycle - idx
      }

    }

    try {
      ret = buffer(idx)
    } catch {
      case _: Throwable =>
    }

    ret
  }

  /**
   * applies a pitch to the scale, returning the absolute index on a tuning
   * @param p
   * @return
   */
  def apply(p: Pitch) : ScaledAndPitched = {
    var ret : ScaledAndPitched = UndefinedScaledPitch

    val ithNoteAndOctave = this(p.number)

    if(ithNoteAndOctave.isDefined) {
      val ithOctave = ithNoteAndOctave.get._2
      val ithNote = ithNoteAndOctave.get._1

      var note = ithNote + this.root

      if(cyclicOctaveSteps.isDefined) {
        note = note + ( (p.octave + ithOctave)* cyclicOctaveSteps.get)
      }

      ret = ScaledPitch(note)
    }

    ret
  }

  /**
   * Returns the ith note in the scale and possible carry from octave shift
   * @param i
   * @return
   */
  def apply(i: Int) : Option[(Int,Int)] = {
    var idx = i
    var ret : Option[(Int, Int)] = None
    var octaveShift = 0

    if(cyclicOctaveSteps.isDefined) {
      val cycle = cyclicOctaveSteps.get
      idx = idx % cycle
      octaveShift = idx / cycle
    }

    val noteList = buffer.toList

    if (idx < ( noteList.length - 1 ) ) {
      ret = Some((noteList(idx), octaveShift))
    }

    ret
  }

  def degrees : List[Int] = BitSetOps.bitDistances(this.buffer)

  def snapToScale(p: Pitch) : PitchBase = {
    val nextPitchNumber = BitSetOps.findNearestSetBit(this.buffer, p.number)

    if (nextPitchNumber.isDefined) {
      Pitch(nextPitchNumber.get, p.octave)
    } else {
      UndefinedPitch
    }
  }

  def length : Int = {
    var ret = buffer.size

    if (cyclicOctaveSteps.isDefined) {
      ret = cyclicOctaveSteps.get
    }

    ret
  }

  def apply(c: Contour) : ConcreteContour = {
    null
  }

  def transpose(offset: Int) : Scale = {

    var transposedBitSet : BitSet = BitSet.empty
    var currentRoot = this.root

    // if cyclic, transposition results in a rotation, otherwise it is a shift with the same root
    if(cyclicOctaveSteps.isDefined) {
      transposedBitSet = BitSetOps.rotate(this.buffer, offset, this.length)
      currentRoot = currentRoot + offset
    } else {
      transposedBitSet = BitSetOps.shift(this.buffer, offset)
    }

    Scale(transposedBitSet, this.cyclicOctaveSteps, currentRoot )
  }


}

object Scale {
  def apply(code: Int, cyclicOctaveSteps: Option[Int] = Some(12), root: Int = 0) : Scale = {
    new Scale(BitSetOps.int2BitSet(code), cyclicOctaveSteps, root)
  }

  def apply(bitSet: scala.collection.immutable.BitSet, cyclicOctaveSteps: Option[Int], root: Int) : Scale = {
    new Scale(bitSet, cyclicOctaveSteps, root)
  }

}

  /*

object Scale {

  def apply[Base](bases: Base*) = fromSeq(bases)

  def fromSeq[Base](buf: Seq[Base]): Scale[Base] = {
    var array = new ArrayBuffer[Base](buf.size)
    for (i <- 0 until buf.size) array += buf(i)
    new Scale[Base](array)
  }

  def newBuilder[Base]: Builder[Base, Scale[Base]] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom[Base,From]: CanBuildFrom[Scale[_], Base, Scale[Base]] =
    new CanBuildFrom[Scale[_], Base, Scale[Base]] {
      def apply(): Builder[Base, Scale[Base]] = newBuilder
      def apply(from: Scale[_]): Builder[Base, Scale[Base]] = newBuilder
    }



}
    */