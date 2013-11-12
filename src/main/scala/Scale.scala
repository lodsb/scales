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
                       private val root: Int) extends Ordered[Scale] {

  //extends IndexedSeq[Boolean]
  /*with IndexedSeqLike[Boolean, Scale] */

  /*
  override protected[this] def newBuilder: Builder[Boolean, Scale] =
    Scale.newBuilder
*/

  private val valueOfBitSet = BitSetOps.bitSet2Int(this.buffer)

  private def ifCyclicElse[A](ifBlock: Int => A)(elseBlock: => A): A = {
    if(cyclicOctaveSteps.isDefined) {
      ifBlock(cyclicOctaveSteps.get)
    } else {
      elseBlock
    }
  }

  def classOf: Scale = {
    // rotate back = 0-class
    val newBitset = ifCyclicElse({cycle =>
      BitSetOps.rotate(this.buffer, -this.root, cycle)
    })({
      BitSetOps.shift(this.buffer, -this.root)
    })

    Scale(newBitset, cyclicOctaveSteps, 0)
  }

  // interval spectrum
  def intervalSpectrum : List[Int] = {
    ifCyclicElse(cycle => BitSetOps.cyclicAutoCorrelation(this.buffer, cycle ))(Nil)
  }

  def isInScale(sp: ScaledPitch): Boolean = {
    // positive and negative indices...
    val note = sp.note
    var ret = false

    val index  = ifCyclicElse(
    { cycle: Int =>
      var idx = note % cycle

      if(idx < 0) {
        idx = cycle - idx
      }
      idx
    })(note)

    try {
      ret = buffer(index)
    } catch {
      case _: Throwable =>
    }

    ret
  }

  // apply for Movement, so we can have AD Scales

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

  //inverse
  def apply(sp: ScaledPitch) : PitchBase = {
    var ret: PitchBase = UndefinedPitch

    val noteList = buffer.toList
    val note = ifCyclicElse(cycle => sp.note % cycle)(sp.note)
    val octave=ifCyclicElse(cycle => sp.note / cycle)(0)


    val index = noteList.indexOf(note)

    if( index > -1) {
      ret = Pitch(note, octave)
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
      idx = i % cycle
      octaveShift = i / cycle
    }

    // the number of notes in a scale = scale steps in one octave
    octaveShift = octaveShift + (idx / buffer.size)
    idx = idx % buffer.size

    val noteList = buffer.toList

    if (idx <  noteList.length  ) {
      ret = Some((noteList(idx), octaveShift))
    }

    ret
  }

  def degrees : List[Int] = BitSetOps.bitDistances(this.buffer)

  // this doesnt make sense! a pitch is _ALWAYS_ snapped to scale
  // should be function for ScaledPitch -> snap to this pitch scale...
  // but it is better to invert scaledpitch and then apply it to another scale
  /*
  def snapToScale(p: Pitch) : PitchBase = {
    val nextPitchNumber = BitSetOps.findNearestSetBit(this.buffer, p.number)

    if (nextPitchNumber.isDefined) {
      Pitch(nextPitchNumber.get, p.octave)
    } else {
      UndefinedPitch
    }
  }
  */

  def length : Int = {
    var ret = buffer.size

    if (cyclicOctaveSteps.isDefined) {
      ret = cyclicOctaveSteps.get
    }

    ret
  }

  // TODO fixme...
  /*
  def apply(c: Contour) : ConcreteContour = {
    null
  }
  */

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

  def compare(that: Scale): Int = this.valueOfBitSet.compare(that.valueOfBitSet)
}

object Scale {
  def apply(code: Int, cyclicOctaveSteps: Option[Int] = Some(12), root: Int = 0) : Scale = {
    new Scale(BitSetOps.int2BitSet(code), cyclicOctaveSteps, root)
  }

  def apply(bitSet: scala.collection.immutable.BitSet, cyclicOctaveSteps: Option[Int], root: Int) : Scale = {
    new Scale(bitSet, cyclicOctaveSteps, root)
  }

  def apply(s: String): Option[Scale] = {
    var ret : Option[Scale] = None

    val id = ScaleLib.id(s)

    if (id.isDefined) {
      ret = Some(this.apply(id.get))
    }

    ret
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