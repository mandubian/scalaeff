package effects
package file

import java.io.{File, BufferedReader, FileReader, FileWriter}

import cats.data.Xor

import shapeless._

sealed trait Mode
case object Read extends Mode
case object Write extends Mode

sealed trait OpenFile[M <: Mode]
case class FileRead(reader: BufferedReader) extends OpenFile[Read.type]
case class FileWrite(writer: FileWriter) extends OpenFile[Write.type]

sealed trait ErrorFile
case object CannotOpen extends ErrorFile
case object EOF extends ErrorFile

trait Moder[M <: Mode] {
  def mode: Mode

  def cast[O <: OpenFile[_]](o: OpenFile[M]): O = o.asInstanceOf[O]

  def castM[O <: OpenFile[_]](o: O): OpenFile[M] = o.asInstanceOf[OpenFile[M]]
}

object Moder {
  implicit object ReadMode  extends Moder[Read.type]  { val mode = Read }
  implicit object WriteMode extends Moder[Write.type] { val mode = Write }
}

sealed trait FileIO extends Effect

case class Open[Mo <: Mode](fname: String)(implicit moder: Moder[Mo]) extends FileIO {
  type T = Boolean
  type ResI = Unit
  type ResO = FileStatus[Mo]

  def handle[M[_], X](res: Unit)(k: Boolean => Xor[ErrorFile, OpenFile[Mo]] => M[X]): M[X] = {
    val fp = new File(fname)
    moder.mode match {
      case Read  => if(fp.canRead()) {
        k(true)(Xor.Right(moder.castM(FileRead(new BufferedReader(new FileReader(fp))))))
      } else {
        k(false)(Xor.Left(CannotOpen))
      }
      case Write => if(fp.canWrite()) {
        k(true)(Xor.Right(moder.castM(FileWrite(new FileWriter(fp)))))
      } else {
        k(false)(Xor.Left(CannotOpen))
      }
    }
  }
}

case class Close[Mo <: Mode](implicit moder: Moder[Mo]) extends FileIO {
  type T = Unit
  type ResI = FileStatus[Mo]
  type ResO = Unit 

  def handle[M[_], X](res: FileStatus[Mo])(k: Unit => Unit => M[X]): M[X] = res match {
    case Xor.Left(e) => k()()
    case Xor.Right(o) => moder.mode match {
      case Read => k(moder.cast[FileRead](o).reader.close)()
      case Write => k(moder.cast[FileWrite](o).writer.close)()
    }
  }
}

case object ReadLine extends FileIO {
  type T = Option[String]
  type ResI = FileStatus[Read.type]
  type ResO = FileStatus[Read.type]

  def handle[M[_], X](res: FileStatus[Read.type])(k: Option[String] => FileStatus[Read.type] => M[X]): M[X] = 
    res match {
      case Xor.Left(e) => k(None)(Xor.Left(e))
      case Xor.Right(FileRead(reader)) => Option(reader.readLine()) match {
        case None => k(None)(Xor.Left(EOF))
        case Some(s) => k(Some(s))(res)
      }
    }

}

case class WriteString(s: String) extends FileIO {
  type T = Unit
  type ResI = FileStatus[Write.type]
  type ResO = FileStatus[Write.type]

  def handle[M[_], X](res: FileStatus[Write.type])(k: Unit => FileStatus[Write.type] => M[X]): M[X] =
    res match {
      case Xor.Left(e) => k(None)(Xor.Left(e))
      case Xor.Right(FileWrite(writer)) => k(writer.write(s))(res)
    }

}

case object IsEOF extends FileIO {
  type T = Boolean
  type ResI = FileStatus[Read.type]
  type ResO = FileStatus[Read.type]

  def handle[M[_], X](res: FileStatus[Read.type])(k: Boolean => FileStatus[Read.type] => M[X]): M[X] ={
    res match {
      case Xor.Left(e) => k(true)(Xor.Left(e))
      case Xor.Right(FileRead(reader)) =>
        reader.mark(1)
        if(reader.read() == -1) { reader.reset(); k(true)(res) }
        else { reader.reset(); k(false)(res) }
    }
  }
}


object FileIO {

  trait Labelled[L] {
    def open[M[_], Mo <: Mode, ES <: HList, ESO <: HList](file: String)(
      implicit prf: EffElem[FileIO@@L, Unit, FileStatus[Mo], ES, ESO], moder: Moder[Mo]
    ): EffM[M, Boolean, ES, ESO] =
      EffM.call[M, FileIO@@L, ES, ESO](Open[Mo](file))

    def close[M[_], Mo <: Mode, ES <: HList, ESO <: HList](
      implicit prf: EffElem[FileIO@@L, FileStatus[Mo], Unit, ES, ESO], moder: Moder[Mo]
    ): EffM[M, Unit, ES, ESO] = EffM.call[M, FileIO@@L, ES, ESO](Close[Mo])

    def readLine[M[_], ES <: HList](
      implicit prf: EffElem[FileIO@@L, FileStatus[Read.type], FileStatus[Read.type], ES, ES]
    ): EffM[M, Option[String], ES, ES] =
      EffM.call[M, FileIO@@L, ES, ES](ReadLine)

    def writeString[M[_], ES <: HList](s: String)(
      implicit prf: EffElem[FileIO@@L, FileStatus[Write.type], FileStatus[Write.type], ES, ES]
    ): EffM[M, Unit, ES, ES] =
      EffM.call[M, FileIO@@L, ES, ES](WriteString(s))
  
    def writeLine[M[_], ES <: HList](s: String)(
      implicit prf: EffElem[FileIO@@L, FileStatus[Write.type], FileStatus[Write.type], ES, ES]
    ): EffM[M, Unit, ES, ES] =
      writeString(s + "\n")

    def isEof[M[_], ES <: HList](
      implicit prf: EffElem[FileIO@@L, FileStatus[Read.type], FileStatus[Read.type], ES, ES]
    ): EffM[M, Boolean, ES, ES] = 
      EffM.call[M, FileIO@@L, ES, ES](IsEOF)
  }

  def apply[L] = new Labelled[L] {}

  def open[M[_], Mo <: Mode, ES <: HList, ESO <: HList](file: String)(
    implicit prf: EffElem[FileIO, Unit, FileStatus[Mo], ES, ESO], moder: Moder[Mo]
  ): EffM[M, Boolean, ES, ESO] =
    EffM.call[M, FileIO, ES, ESO](Open[Mo](file))

  def close[M[_], Mo <: Mode, ES <: HList, ESO <: HList](
    implicit prf: EffElem[FileIO, FileStatus[Mo], Unit, ES, ESO], moder: Moder[Mo]
  ): EffM[M, Unit, ES, ESO] = EffM.call[M, FileIO, ES, ESO](Close[Mo])

  def readLine[M[_], ES <: HList](
    implicit prf: EffElem[FileIO, FileStatus[Read.type], FileStatus[Read.type], ES, ES]
  ): EffM[M, Option[String], ES, ES] =
    EffM.call[M, FileIO, ES, ES](ReadLine)

  def writeString[M[_], ES <: HList](s: String)(
    implicit prf: EffElem[FileIO, FileStatus[Write.type], FileStatus[Write.type], ES, ES]
  ): EffM[M, Unit, ES, ES] =
    EffM.call[M, FileIO, ES, ES](WriteString(s))

  def writeLine[M[_], ES <: HList](s: String)(
    implicit prf: EffElem[FileIO, FileStatus[Write.type], FileStatus[Write.type], ES, ES]
  ): EffM[M, Unit, ES, ES] = 
    writeString(s + "\n")

  def isEof[M[_], ES <: HList](
    implicit prf: EffElem[FileIO, FileStatus[Read.type], FileStatus[Read.type], ES, ES]
  ): EffM[M, Boolean, ES, ES] = 
    EffM.call[M, FileIO, ES, ES](IsEOF)
}

