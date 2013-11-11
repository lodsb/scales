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

//put comparison here, should be a generic type then
sealed trait Pitched


/** ***************
  * Pitches
  */
abstract class PitchBase extends Ordered[PitchBase] with Pitched {

  // comparison for note value -> basic pitch classes
  def compare(that: PitchBase): Int = {
    this.number.compare(that.number)
  }

  def compareWithOctave(that: PitchBase): Int = {
    // quick hack
    scala.math.min(scala.math.max(this.number.compare(that.number) + 10*this.octave.compare(that.octave),-1),1)
  }

  def number: Int

  def octave: Int

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[PitchBase] && this.compareWithOctave(that.asInstanceOf[PitchBase])==0
  }

  override def toString = "Pitch(number="+this.number+", octave="+this.octave+")"

}
// this describes an abstract Pitch (no tuning and scale)
class Pitch(override val number: Int, override val octave: Int=5) extends PitchBase
object Pitch {
  def apply(number: Int, octave: Int=5) = new Pitch(number, octave)
}


//undefined pitch
object UndefinedPitch extends PitchBase {
  override def number = Int.MinValue
  override def octave = Int.MinValue
}

case class Tonic(override val octave: Int=5) extends Pitch(0)

case class Supertonic(override val octave: Int=5) extends Pitch(1)

case class Mediant(override val octave: Int=5) extends Pitch(2)

case class Subdominant(override val octave: Int=5) extends Pitch(3)

case class Dominant(override val octave: Int=5) extends Pitch(4)

case class Submediant(override val octave: Int=5) extends Pitch(5)

case class Subtonic(override val octave: Int=5) extends Pitch(6)


trait ScaledAndPitched extends Pitched
case class ScaledPitch(note: Int) extends ScaledAndPitched
object UndefinedScaledPitch extends ScaledAndPitched

trait TunedPitch extends Pitched
case class ConcretePitch(frequency: Double) extends TunedPitch
object UndefinedTunedPitch
