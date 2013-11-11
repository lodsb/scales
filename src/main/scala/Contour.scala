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
class Contour[A <: Pitched] {
  // is indexed seq, transpose idx, transpose all of class etc...
  // also supports chords?

}

class ConcreteContour {

}
