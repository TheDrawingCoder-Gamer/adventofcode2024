package gay.menkissing.advent

import gay.menkissing.advent.Problem
import gay.menkissing.common.debugTiming

import scala.collection.mutable as mut
import scala.io.Source

object Day9 extends Problem[String, Long]:
  
  val input = FileIO.getContentsOf("day9.txt").trim

  override def parse(str: String): String = str

  def unpackData(str: String): Disk = {
    var freakyNum: Long = 0
    Disk(str.zipWithIndex.flatMap { (s, i) =>
      if (i % 2 == 0) {
        Iterator.fill(s.asDigit)((i / 2).toLong)
      } else {
        Iterator.fill(s.asDigit)(-1L)
      }
    })
  }

  def unpackP2(str: String): Vector[Chunk] = {
    str.zipWithIndex.map { (s, i) =>
      if (i % 2 == 0) File(s.asDigit, i / 2) else Free(s.asDigit)
    }.toVector
  }
  

  sealed trait Chunk
  
  case class File(size: Int, id: Long) extends Chunk
  case class Free(size: Int) extends Chunk

  case class Disk(disk: Seq[Long]) {
    override def toString: String = {
      disk.map {
        case -1 => '.'
        case c =>
          if (c < 10) {
            ('0' + c).toChar
          } else if (c < (10 + 26)) {
            ('A' + (c - 10)).toChar
          } else if (c < (10 + 26 + 26)) {
            ('a' + (c - 10 - 26)).toChar
          } else {
            '?'
          }
      }.mkString("")
    }

    def checksum: Long = {
      disk.zipWithIndex.filterNot(_._1 == -1).foldLeft(0L) { (acc, r) =>
        acc + (r._1 * r._2)
      }
    }
  
    def part1: Disk = {
      val sorted = disk.toArray
      val countFilled = sorted.count(_ != -1)
  
      while (sorted.indexWhere(_ == -1) < countFilled) {
        val lastIdx = sorted.lastIndexWhere(_ != -1)
        val lastBlock = sorted(lastIdx)
        sorted.update(lastIdx, -1)
        val startIdx = sorted.indexWhere(_ == -1)
        sorted.update(startIdx, lastBlock)
      }
      Disk(sorted.toSeq)
    }
  
  }




  def flattenChunkList(chunks: Iterable[Chunk]): Disk = {
    Disk(chunks.flatMap {
      case File(size, id) => Iterator.fill(size)(id)
      case Free(size) => Iterator.fill(size)(-1L)
    }.toList)
  }


  def calculateP2(chunks: Vector[Chunk]): Vector[Chunk] = {
    val data = chunks.iterator.to(mut.ArrayBuffer.iterableFactory[Chunk])
    var curId = chunks.findLast(_.isInstanceOf[File]).get.asInstanceOf[File].id + 1
  
    while (curId >= 0) {
      val curIdx = data.lastIndexWhere {
        case File(size, id) => id < curId
        case _ => false
      }
      if (curIdx >= 0) {
        val curFile = data(curIdx).asInstanceOf[File]
        curId = curFile.id
        val freespace = data.indexWhere {
          case Free(size) => size >= curFile.size
          case _ => false
        }
        if (freespace < curIdx && freespace > 0) {
          val curFreespace = data(freespace).asInstanceOf[Free]
          data(freespace) = curFile
          data(curIdx) = Free(curFile.size)
          if (curFile.size < curFreespace.size) {
            data.insert(freespace + 1, Free(curFreespace.size - curFile.size))
          }
        }
      } else {
        curId = -1
      }
    }
  
    data.toVector
  
  }

  override def part1(input: String): Long =
    val disk = unpackData(input)
    disk.part1.checksum

  override def part2(input: String): Long =
    val chunks = unpackP2(input)
    flattenChunkList(calculateP2(chunks)).checksum
