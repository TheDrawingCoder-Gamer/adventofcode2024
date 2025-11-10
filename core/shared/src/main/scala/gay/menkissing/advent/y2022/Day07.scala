package gay.menkissing.advent
package y2022

import scala.collection.mutable
import scala.io.Source
import scala.util.chaining.*
import gay.menkissing.common.*
import cats.Show

object Day07 extends Problem:
  type Input = Seq[Command]
  type Output = Int

  lazy val input = FileIO.getInput(2022, 7)
  enum Command:
    case Chdir(to: String)
    case List(files: Seq[AFile])

  enum AFile(name: String):
    case File(name: String, size: Int) extends AFile(name)
    case Dir(name: String) extends AFile(name)

  def parse(input: String): Seq[Command] =
    val daSeq = mutable.ListBuffer[Command]()
    val lines = input.linesIterator.toList
    var i = 0
    while i < lines.length do
      val line = lines(i)
      i += 1
      line match
        case s"$$ cd $dir" => daSeq += Command.Chdir(dir)
        case s"$$ ls"      =>
          val files = lines.drop(i).takeWhile(it => !it.startsWith("$"))
          i += files.length
          daSeq += Command.List:
            files.map:
              case s"dir $dir"    => AFile.Dir(dir)
              case s"$size $name" => AFile.File(name, size.toInt)
    // drop cd /
    daSeq.toSeq.drop(1)

  def buildDirs(commands: Seq[Command]): FSDir =
    val root: FSDir = FSDir(0, "/", mutable.ListBuffer())

    var workingOn: FSDir = root
    // sanity loss
    val parents = mutable.ListBuffer[FSDir]()
    commands.foreach:
      case Command.Chdir("..") =>
        workingOn = parents.head
        parents.dropInPlace(1)
      case Command.Chdir(to) =>
        val node = workingOn.children.find(_.name == to)
        node.foreach:
          case it: FSDir =>
            parents.prepend(workingOn)
            workingOn = it
          case _ => !!!
        node.getOrElse:
          val newOne = FSDir(0, to, mutable.ListBuffer())
          workingOn.children += newOne
          parents.prepend(workingOn)
          workingOn = newOne
      case Command.List(files) =>
        workingOn.children ++= files.iterator.map:
          case AFile.Dir(name)        => FSDir(0, name, mutable.ListBuffer())
          case AFile.File(name, size) => FSFFile(size, name)

    root

  sealed trait FSFile:
    def size: Int
    val name: String
    def show: String

  case class FSFFile(size: Int, name: String) extends FSFile:
    override def show: String = s"- $name (file, size=$size)"
  class FSDir
    (
      var size: Int,
      val name: String,
      val children: mutable.ListBuffer[FSFile]
    ) extends FSFile:
    override def toString: String = s"dir $name"
    override def show: String =
      children.map(_.show.prependedAll("> "))
        .fold(s"- $name (dir)")(_ + "\n" + _)

  def calcSizes(fsdir: FSDir): Unit =
    fsdir.children.foreach:
      case FSFFile(size, name) => ()
      case d: FSDir            => calcSizes(d)
    fsdir.size = fsdir.children.map(_.size).sum

  def part1(input: Seq[Command]): Int =
    val root = buildDirs(input)
    calcSizes(root)
    sizesLessThanN(root, 100000).map(_.size).sum

  def sizesLessThanN(fsdir: FSDir, n: Int): Seq[FSDir] =
    lazy val children =
      fsdir.children.flatMap:
        case it: FSDir => sizesLessThanN(it, n)
        case _: FSFile => Seq()
      .toSeq
    if fsdir.size < n then children.appended(fsdir)
    else children

  def findSmallestGreaterThanN(fsdir: FSDir, n: Int): Option[FSDir] =
    val smallestChild =
      fsdir.children.collect:
        case it: FSDir => findSmallestGreaterThanN(it, n)
      .minByOption(_.map(_.size).getOrElse(Int.MaxValue)).flatten
    val goodfsdir =
      if fsdir.size < n then None
      else Some(fsdir)
    (goodfsdir, smallestChild) match
      case (Some(parent), Some(child)) =>
        if parent.size > child.size then Some(child)
        else Some(parent)
      case _ => goodfsdir.orElse(smallestChild)

  def part2(input: Seq[Command]): Int =
    val root = buildDirs(input)
    calcSizes(root)
    //
    val total = 70000000
    val required = 30000000
    val used = root.size
    val available = total - used
    val toFree = required - available

    findSmallestGreaterThanN(root, toFree).map(_.size).get
