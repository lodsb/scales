import scala.collection.immutable.BitSet

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-11-07 :: 22:00
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


object BitSetOps {

  def rotate(b: BitSet, offset: Int, bits: Int): BitSet = {
    b.map {
      x => if (offset >= 0) {
        (x + offset) % bits
      } else {
        (bits - (x + offset)) % bits
      }
    }
  }

  def shift(b: BitSet, offset: Int) = {
    var ret = BitSet.empty

    b.foreach { x=>
      if(x + offset >= 0) {
        ret = ret + (x + offset)
      }
    }

    ret
  }

  def cardinality(b: BitSet) = {
    b.foldLeft(0){(x,y) => x+1}
  }

  def nextSetBitInclude(b: BitSet, from: Int) : Option[Int] = {
    findNearestSetBit(b, from, (from, bitPos) => {from <= bitPos} )
  }

  def nextSetBitExclude(b: BitSet, from: Int) : Option[Int] = {
    findNearestSetBit(b, from, (from, bitPos) => {from < bitPos} )
  }

  def findNearestSetBit(b: BitSet, from: Int,
                        constraint: (Int, Int) => Boolean = (from,pos) => true) : Option[Int] = {
    var ret: Option[Int] = None
    var minDistance: Option[Int] = None

    b.foreach({
      bitNr =>
        val distance = scala.math.abs(bitNr - from)

        if ( (minDistance == None || (distance < minDistance.get)) && constraint(from, bitNr) ) {
          minDistance = Some(distance)
          ret = Some(bitNr)
        }
    })

    ret
  }

  def bitDistances(b: BitSet) : List[Int] = {
    b.foldLeft(List((0,0))){(x,y) => println(x+" " + y); (y-x.head._2, y)::x}.reverse.map{x => x._1}.filter(_!=0)
  }

  def int2BitSet(n: BigInt): BitSet = {
    var b = BitSet.empty

    for(idx <- 0 to n.bitLength) {
      if(n.testBit(idx)) {
        b = b + idx
      }
    }

    b
  }

  def bitSet2Int(b: BitSet) : BigInt = {
    var ret = BigInt(0)

    b.foreach { x=>
      ret.setBit(x)
    }

    ret
  }


}
