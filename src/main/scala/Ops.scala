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

object Conversions {
  implicit def int2Chroma(i: Int) : Chromatic = Chromatic(i)
  implicit def chroma2Int(c: Chromatic) : Int = c.value
}

object Transformation {
  import Conversions._

  trait TransposeOp[From, With, To] {
    def apply(fst: From, snd: With): To
  }

  implicit object PitchIntTranspose extends TransposeOp[Pitch, Chromatic, Pitch] {
    def apply(fst: Pitch, snd: Chromatic): Pitch = {
      Pitch(fst.number+snd, fst.octave)
    }
  }

  implicit object ScaleIntTranspose extends TransposeOp[Scale, Chromatic, Scale] {
    def apply(fst: Scale, snd: Chromatic): Scale = {
      fst.transpose(snd)
    }
  }

  implicit object PitchPitchTranspose extends TransposeOp[Pitch, Pitch, Pitch] {
    def apply(fst: Pitch, snd: Pitch): Pitch = {
      Pitch(fst.number+snd.number, fst.octave+snd.number)
    }
  }

  //TODO: Sequences/chords...
  // is indexed seq, transpose idx, transpose all of class etc...
  // should be the same for chord and contour

  def transpose[From, With, To](fst: From, snd: With)(implicit top: TransposeOp[From, With, To]): To = {
    top(fst, snd)
  }

  def octaveShift(p: Pitch, offset: Int) = {
    val octave = scala.math.max(p.octave + offset, 0)
    Pitch(p.number, octave)
  }
}

object Algebra {
  // eqivalence classes for pitches (d'uh), chords and scales
 // guess scale from pitchset/chord

}
