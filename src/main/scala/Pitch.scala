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

trait Pitched

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

trait ScaledAndPitched extends Pitched
case class ScaledPitch(note: Int) extends ScaledAndPitched
object UndefinedScaledPitch extends ScaledAndPitched

//case class ConcretePitch() extends Pitch {} // must be comparable -> movement

trait TunedPitch extends Pitched
case class ConcretePitch(frequency: Double) extends TunedPitch
object UndefinedTunedPitch
