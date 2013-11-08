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
case class Movement(from: Pitch, to: Pitch) {
  def direction: Int = from.compare(to)
}

class Contour {
  // is indexed seq, transpose idx, transpose all of class etc...
  // also supports chords?

}

class ConcreteContour {

}
