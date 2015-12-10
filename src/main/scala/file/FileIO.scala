/*
Copyright 2015 Miles Sabin & Pascal Voitot

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package effects
package file

import java.io.{File, BufferedReader, FileReader, FileWriter}

import cats.data.Xor

import shapeless._

sealed trait Mode
case object Read extends Mode
case object Write extends Mode

sealed trait FileHandler[M <: Mode]
case class FileRead(reader: BufferedReader) extends FileHandler[Read.type]
case class FileWrite(writer: FileWriter) extends FileHandler[Write.type]

sealed trait FileError
case object CannotOpen extends FileError
case object EOF extends FileError

trait Moder[M <: Mode] {
  def mode: Mode

  def cast[O <: FileHandler[_]](o: FileHandler[M]): O = o.asInstanceOf[O]

  def castM[O <: FileHandler[_]](o: O): FileHandler[M] = o.asInstanceOf[FileHandler[M]]
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

  def handle[M[_], X](res: Unit)(k: Boolean => Xor[FileError, FileHandler[Mo]] => M[X]): M[X] = {
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

  def open0[M[_], Mo <: Mode](file: String)(
    implicit moder: Moder[Mo]
  ): EffM[M, Boolean, MkEff[FileIO, Unit] :: HNil, MkEff[FileIO, FileStatus[Mo]] :: HNil] =
    EffM.call[M, FileIO, MkEff[FileIO, Unit] :: HNil](Open[Mo](file))

  def open[Mo <: Mode](file: String)(
    implicit ctx: Ctx, moder: Moder[Mo]
  ): EffM[ctx.M, Boolean, MkEff[FileIO, Unit] :: HNil, MkEff[FileIO, FileStatus[Mo]] :: HNil] =
    open0(file)

  def close0[M[_], Mo <: Mode]()(
    implicit moder: Moder[Mo]
  ): EffM[M, Unit, MkEff[FileIO, FileStatus[Mo]] :: HNil, MkEff[FileIO, Unit] :: HNil] =
    EffM.call[M, FileIO, MkEff[FileIO, FileStatus[Mo]] :: HNil](Close[Mo])

  def close[Mo <: Mode]()(
    implicit ctx: Ctx, moder: Moder[Mo]
  ): EffM[ctx.M, Unit, MkEff[FileIO, FileStatus[Mo]] :: HNil, MkEff[FileIO, Unit] :: HNil] =
    EffM.call[ctx.M, FileIO, MkEff[FileIO, FileStatus[Mo]] :: HNil](Close[Mo])

  def readLine0[M[_]]: EffM[M, Option[String], MkEff[FileIO, FileStatus[Read.type]] :: HNil, MkEff[FileIO, FileStatus[Read.type]] :: HNil] =
    EffM.call[M, FileIO, MkEff[FileIO, FileStatus[Read.type]] :: HNil](ReadLine)

  def readLine(implicit ctx: Ctx): EffM[ctx.M, Option[String], MkEff[FileIO, FileStatus[Read.type]] :: HNil, MkEff[FileIO, FileStatus[Read.type]] :: HNil] =
    readLine0

  def writeString0[M[_]](s: String): EffM[M, Unit, MkEff[FileIO, FileStatus[Write.type]] :: HNil, MkEff[FileIO, FileStatus[Write.type]] :: HNil] =
    EffM.call[M, FileIO, MkEff[FileIO, FileStatus[Write.type]] :: HNil](WriteString(s))

  def writeString(s: String)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[FileIO, FileStatus[Write.type]] :: HNil, MkEff[FileIO, FileStatus[Write.type]] :: HNil] =
    writeString0(s)

  def writeLine0[M[_]](s: String): EffM[M, Unit, MkEff[FileIO, FileStatus[Write.type]] :: HNil, MkEff[FileIO, FileStatus[Write.type]] :: HNil] =
    writeString0(s + "\n")

  def writeLine(s: String)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[FileIO, FileStatus[Write.type]] :: HNil, MkEff[FileIO, FileStatus[Write.type]] :: HNil] =
    writeLine0(s)

  def isEof0[M[_]]: EffM[M, Boolean, MkEff[FileIO, FileStatus[Read.type]] :: HNil, MkEff[FileIO, FileStatus[Read.type]] :: HNil] = 
    EffM.call[M, FileIO, MkEff[FileIO, FileStatus[Read.type]] :: HNil](IsEOF)

  def isEof(implicit ctx: Ctx): EffM[ctx.M, Boolean, MkEff[FileIO, FileStatus[Read.type]] :: HNil, MkEff[FileIO, FileStatus[Read.type]] :: HNil] = 
    isEof0

  trait Labelled[L] {
    def open0[M[_], Mo <: Mode](file: String)(
      implicit moder: Moder[Mo]
    ): EffM[M, Boolean, MkEff[FileIO@@L, Unit] :: HNil, MkEff[FileIO@@L, FileStatus[Mo]] :: HNil] =
      EffM.call[M, FileIO@@L, MkEff[FileIO@@L, Unit] :: HNil](Open[Mo](file))

    def open[Mo <: Mode](file: String)(
      implicit ctx: Ctx, moder: Moder[Mo]
    ): EffM[ctx.M, Boolean, MkEff[FileIO@@L, Unit] :: HNil, MkEff[FileIO@@L, FileStatus[Mo]] :: HNil] =
      open0(file)

    def close0[M[_], Mo <: Mode]()(
      implicit moder: Moder[Mo]
    ): EffM[M, Unit, MkEff[FileIO@@L, FileStatus[Mo]] :: HNil, MkEff[FileIO@@L, Unit] :: HNil] =
      EffM.call[M, FileIO@@L, MkEff[FileIO@@L, FileStatus[Mo]] :: HNil](Close[Mo])

    def close[Mo <: Mode]()(
      implicit ctx: Ctx, moder: Moder[Mo]
    ): EffM[ctx.M, Unit, MkEff[FileIO@@L, FileStatus[Mo]] :: HNil, MkEff[FileIO@@L, Unit] :: HNil] =
      EffM.call[ctx.M, FileIO@@L, MkEff[FileIO@@L, FileStatus[Mo]] :: HNil](Close[Mo])

    def readLine0[M[_]]: EffM[M, Option[String], MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil] =
      EffM.call[M, FileIO@@L, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil](ReadLine)

    def readLine(implicit ctx: Ctx): EffM[ctx.M, Option[String], MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil] =
      readLine0

    def writeString0[M[_]](s: String): EffM[M, Unit, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil] =
      EffM.call[M, FileIO@@L, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil](WriteString(s))

    def writeString(s: String)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil] =
      writeString0(s)

    def writeLine0[M[_]](s: String): EffM[M, Unit, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil] =
      writeString0(s + "\n")

    def writeLine(s: String)(implicit ctx: Ctx): EffM[ctx.M, Unit, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Write.type]] :: HNil] =
      writeLine0(s)

    def isEof0[M[_]]: EffM[M, Boolean, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil] = 
      EffM.call[M, FileIO@@L, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil](IsEOF)

    def isEof(implicit ctx: Ctx): EffM[ctx.M, Boolean, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil, MkEff[FileIO@@L, FileStatus[Read.type]] :: HNil] = 
      isEof0
  }

  def apply[L] = new Labelled[L] {}


}

