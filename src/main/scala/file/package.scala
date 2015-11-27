package effects

import cats.data.Xor


package object file {

  type FileStatus[Mo <: Mode] = Xor[ErrorFile, OpenFile[Mo]]

}

