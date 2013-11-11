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

  def number: Int

  def octave: Int

}
// this describes an abstract Pitch (no tuning and scale)
case class Pitch(override val number: Int, override val octave: Int=5) extends PitchBase
//undefined pitch
object UndefinedPitch extends PitchBase {
  override def number = Int.MinValue
  override def octave = Int.MinValue
}

case class Tonic(override val octave: Int=5) extends PitchBase {
  override def number = 1
}

case class Supertonic(override val octave: Int=5) extends PitchBase {
  override def number = 2
}

case class Mediant(override val octave: Int=5) extends PitchBase {
  override def number = 3
}

case class Subdominant(override val octave: Int=5) extends PitchBase {
  override def number = 4
}

case class Dominant(override val octave: Int=5) extends PitchBase {
  override def number = 5
}

case class Subominant(override val octave: Int=5) extends PitchBase {
  override def number = 6
}

case class Subtonic(override val octave: Int=5) extends PitchBase {
  override def number = 7
}


trait ScaledAndPitched extends Pitched
case class ScaledPitch(note: Int) extends ScaledAndPitched
object UndefinedScaledPitch extends ScaledAndPitched

trait TunedPitch extends Pitched
case class ConcretePitch(frequency: Double) extends TunedPitch
object UndefinedTunedPitch
