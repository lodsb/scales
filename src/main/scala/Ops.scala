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

object Ops {
  trait TransposeOp[From, With, To] {
    def apply(fst: From, snd: With): To
  }

  implicit object PitchIntTranspose extends TransposeOp[Pitch, Int, Pitch] {
    def apply(fst: Pitch, snd: Int): Pitch = {
      Pitch(fst.number+snd, fst.octave)
    }
  }

  implicit object ScaleIntTranspose extends TransposeOp[Scale, Int, Scale] {
    def apply(fst: Scale, snd: Int): Scale = {
      fst.transpose(snd)
    }
  }

  implicit object PitchPitchTranspose extends TransposeOp[Pitch, Pitch, Pitch] {
    def apply(fst: Pitch, snd: Pitch): Pitch = {
      Pitch(fst.number+snd.number, fst.octave+snd.number)
    }
  }

  //TODO: Sequences/chords...

  def transpose[From, With, To](fst: From, snd: With)(implicit top: TransposeOp[From, With, To]): To = {
    top(fst, snd)
  }

}
