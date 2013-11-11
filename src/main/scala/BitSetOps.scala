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
    //b.foldLeft(0){(x,y) => x+1} // err... size?
    b.size
  }

  private def bigIterator(start: BigInt, end: BigInt, step: BigInt = 1) =
    Iterator.iterate(start)(_ + step).takeWhile(_ <= end)

  // G-System
  def equivalenceClasses(noBits: Int = 12) : List[BigInt] = {
    // brute force approach
    // highly stupid forward method, SLOW

    var instances = Map[BigInt, BigInt]()
    var classes = List[BigInt]()

    val maxVal: BigInt = (BigInt(2).pow(noBits)-1)
    val iterator = bigIterator(0, maxVal)

    iterator.foreach {
      idx =>

      if (!instances.contains(idx)) {
        // found new class
        instances = instances + (idx -> idx)
        classes = idx +: classes

        // generate all instances of the class via rotation
        // not using int mult here because of BigInt
        val eqClass = this.int2BitSet(idx)


        for(i <- 1 to noBits) {
          val rot = this.rotate(eqClass, i, noBits)
          val rotVal = this.bitSet2Int(rot)

          instances = instances + (rotVal -> rotVal)
        }

      }
    }

    classes
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

  def cyclicAutoCorrelation(b: BitSet,  noBits: Int) : List[Int] = {
    var ret : List[Int] = Nil

    for(rots <- 1 to noBits-1) {
      val rotated = this.rotate(b, rots, noBits);

      println(rotated)
      println("--")
      println(rotated&b)

      val correlation = (rotated & b).size

      ret = ret :+ correlation
    }

    ret
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

    val base = BigInt(2)
    b.foreach { x=>
      ret = ret + base.pow(x)
    }

    ret
  }


}
