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
  implicit def int2Pitch(i: Int) : Pitch = Pitch(i)
  implicit def chroma2Int(c: Chromatic) : Int = c.number
}

object Transformations {
  import Conversions._

  trait TransposeOp[From, With, To] {
    def apply(fst: From, snd: With): To
  }

  implicit object PitchChromaTranspose extends TransposeOp[Pitch, Chromatic, Pitch] {
    def apply(fst: Pitch, snd: Chromatic): Pitch = {
      Pitch(fst.number+snd, fst.octave)
    }
  }

  implicit object ScaleChromaTranspose extends TransposeOp[Scale, Chromatic, Scale] {
    def apply(fst: Scale, snd: Chromatic): Scale = {
      fst.transpose(snd)
    }
  }

  implicit object ScaledPitchChromaTranspose extends TransposeOp[ScaledPitch, Chromatic, ScaledPitch] {
    def apply(fst: ScaledPitch, snd: Chromatic): ScaledPitch = {
      ScaledPitch(fst.note+snd.number)
    }
  }

  implicit object PitchPitchTranspose extends TransposeOp[Pitch, Pitch, Pitch] {
    def apply(fst: Pitch, snd: Pitch): Pitch = {
      Pitch(fst.number+snd.number, fst.octave+snd.number)
    }
  }
  /*
 implicit object Dummy extends TransposeOp[AnyRef, AnyRef, AnyRef] {
   def apply(fst: AnyRef, snd: AnyRef): AnyRef = {
     null.asInstanceOf[AnyRef]
   }
 }  */
/*
 implicit object ChordPitchPitchTranspose extends TransposeOp[Chord[Pitch], Pitch, Chord[Pitch]] {
   def apply(fst: Chord[Pitch], snd: Pitch): Chord[Pitch] = {
     fst.map(x => PitchPitchTranspose(x,snd))
   }
 }

 implicit object ChordPitchChromaTranspose extends TransposeOp[Chord[Pitch], Chromatic, Chord[Pitch]] {
   def apply(fst: Chord[Pitch], snd: Chromatic): Chord[Pitch] = {
     fst.map(x => PitchChromaTranspose(x,snd))
   }
 }

 implicit object ChordScaledPitchChromaTranspose extends TransposeOp[Chord[ScaledPitch], Chromatic, Chord[ScaledPitch]] {
   def apply(fst: Chord[ScaledPitch], snd: Chromatic): Chord[ScaledPitch] = {
     fst.map(x => ScaledPitchChromaTranspose(x,snd))
   }
 }
  */

  def transpose[From, With, To](fst: From, snd: With)(implicit top: TransposeOp[From, With, To]): To = {
    top(fst, snd)
  }

  def transpose[From <: Pitched[_], With, To<:Pitched[_]](fst: Chord[From], snd: With)
                                                   (implicit top: TransposeOp[From, With, To]): Chord[To] = {
    fst.map { x => top(x, snd) }
  }

  def transpose[From <: Pitched[_], With, To<:Pitched[_]](fst: Contour[Chord[From]], snd: With)
                                                   (implicit top: TransposeOp[From, With, To]): Contour[Chord[To]] = {
    fst.map { x => transpose(x, snd) }
  }

  def octaveShift(p: Pitch, offset: Int) = {
    val octave = scala.math.max(p.octave + offset, 0)
    Pitch(p.number, octave)
  }

  def chordify(p: Pitch, f: Function[Pitch, Chord[Pitch]]) : Chord[Pitch] = {
    f(p)
  }

  def chordify(p: Contour[Pitch], f: Function[Pitch, Chord[Pitch]]) : Contour[Chord[Pitch]] = {
    p.map {x => f(x)}
  }



}

object Algebra {
  // eqivalence classes for pitches (d'uh), chords and scales
 // guess scale from pitchset/chord

}
