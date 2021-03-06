package effects


import org.scalatest.concurrent.{AsyncAssertions, PatienceConfiguration, ScalaFutures}
import org.scalatest.{Assertions, BeforeAndAfterAll, FeatureSpec, FlatSpec, Matchers, Suite}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import cats.data.Xor

import shapeless._
import ops.hlist._

// import file._
import state._
import stdio._

object Implicits {
  import scala.concurrent.Future

  implicit def handlerGet[A]: Handler.Aux[Get[A], A, A, A, Future] = new Handler[Get[A], Future] {
    type T = A
    type ResI = A
    type ResO = A

    def handle[X](e: Get[A])(a: ResI)(k: T => ResO => Future[X]): Future[X] = k(a)(a)
  }

  implicit def handlerPut[A, B]: Handler.Aux[Put[A, B], Unit, A, B, Future] = new Handler[Put[A, B], Future] {
    type T = Unit
    type ResI = A
    type ResO = B

    def handle[X](e: Put[A, B])(res: A)(k: Unit => B => Future[X]): Future[X] = k(())(e.b)
  }

  implicit val handlerPutStr: Handler.Aux[PutStr, Unit, Unit, Unit, Future] = new Handler[PutStr, Future] {
    type T = Unit
    type ResI = Unit
    type ResO = Unit

    def handle[X](e: PutStr)(a: Unit)(k: Unit => Unit => Future[X]): Future[X] = k(print(e.s))()
  }

  implicit val handlerPutChar: Handler.Aux[PutChar, Unit, Unit, Unit, Future] = new Handler[PutChar, Future] {
    type T = Unit
    type ResI = Unit
    type ResO = Unit

    def handle[X](e: PutChar)(a: Unit)(k: Unit => Unit => Future[X]): Future[X] = k(print(e.c))()
  }

  implicit val handlerGetStr: Handler.Aux[GetStr.type, String, Unit, Unit, Future] = new Handler[GetStr.type, Future] {
    type T = String
    type ResI = Unit
    type ResO = Unit

    def handle[X](e: GetStr.type)(a: Unit)(k: String => Unit => Future[X]): Future[X] = k(scala.io.StdIn.readLine())()
  }

  implicit val handlerGetChar: Handler.Aux[GetChar.type, Char, Unit, Unit, Future] = new Handler[GetChar.type, Future] {
    type T = Char
    type ResI = Unit
    type ResO = Unit

    def handle[X](e: GetChar.type)(a: Unit)(k: Char => Unit => Future[X]): Future[X] = k(scala.io.StdIn.readChar())()
  }
}

class EffSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))
  
  sealed trait Label
  case object Foo extends Label
  case object Bar extends Label
  // type Foo = Foo.type
  // type Bar = Bar.type

  "Eff" should "simplest State" in {

    // Define our program with Effects in an abstract context of execution M[_] (very simple for now as I need to port the handler code to other effects)
    // Please note that M doesn't require anything here
    def eff[M[_]] = for {
      k <- State.get0[M, Int]
      _ <- State.put0[M, Int](k + 2)
      l <- State.get0[M, Int]
    } yield (l)

    // Define our Handlers for a given context of execution (dummy Future here)
    import scala.concurrent.Future

    implicit def handlerGet[A]: Handler.Aux[Get[A], A, A, A, Future] = new Handler[Get[A], Future] {
      type T = A
      type ResI = A
      type ResO = A

      def handle[X](e: Get[A])(a: ResI)(k: T => ResO => Future[X]): Future[X] = k(a)(a)
    }

    implicit def handlerPut[A, B]: Handler.Aux[Put[A, B], Unit, A, B, Future] = new Handler[Put[A, B], Future] {
      type T = Unit
      type ResI = A
      type ResO = B

      def handle[X](e: Put[A, B])(res: A)(k: Unit => B => Future[X]): Future[X] = k(())(e.b)
    }

    // Execute program
    // On execution site, M[_] just requires to be an Applicative to run it
    // (and in reality a alleycats Pure would be enough, we don't need the Functor)
    import cats.std.future._
    import scala.concurrent.ExecutionContext.Implicits.global

    // Run the program
    // if anything is missing (the State initial resource, the Get/Put handlers for Future),
    // it won't compile
    val r = eff.run(MkEff[State, Int](3) :: HNil).futureValue

    r should equal (5)
  }

import scala.concurrent.Future
import cats.std.future._
import scala.concurrent.ExecutionContext.Implicits.global

it should "label" in {
  import Implicits._

  def eff[M[_]] = effective[M]{ implicit ctx =>
      for {
        k   <- Foo-:State.get[Int]
        k2  <- Bar-:State.get[Int]
        _   <- Bar-:State.put[Int](k + k2)
        k3  <- Bar-:State.get[Int]
      } yield (k3)
  }

  val r = eff[Future].run(Foo-:State.Res(3) :: Bar-:State.Res(2) :: HNil).futureValue
  println("r:"+r)
  r should equal (5)
}

it should "mix State & StdIO" in {
  import Implicits._

  def eff[M[_]] = effective[M]{ implicit ctx =>
      for {
        _   <- StdIO.putStrLn("Enter a name:")
        // n   <- StdIO.getStr
        n = "toto"
        k   <- Foo-:State.get[Int]
        k2  <- Bar-:State.get[Int]
        _   <- Bar-:State.put[Int](n.length + k + k2)
        k3  <- Bar-:State.get[Int]
      } yield (n -> k3)
  }

  val (name, r) = eff[Future].run(Foo-:State.Res(3) :: Bar-:State.Res(2) :: StdIO.Res() :: HNil).futureValue
  // r should equal (5 + name.length)

}

it should "More complex State+Label+ full effective" in {
  import Implicits._

  val eff = effective[Future, (State-:Int@:Foo.type) :: (State-:String@:Bar.type) :: (StdIO-:Unit) :: HNil]{ implicit ctx =>
    for {
      k0  <- Foo-:State.get[Int]
      _   <- Bar-:State.put("works")
      _   <- Foo-:State.update((i:Int) => i + 5)
      k   <- Foo-:State.get[Int]
      _   <- StdIO.putStrLn(s"tmp state:$k")
      _   <- Bar-:State.update((s:String) => s + s"_$k")
      s   <- Bar-:State.get[String]
      _   <- Foo-:State.updateM[Int, String]((k:Int) => k0.toString + s"_$s")
      r   <- Foo-:State.get[String]
      _   <- StdIO.putStrLn(s"final state:$r")
    } yield (r)
  }


  val r = eff.run(Foo-:State.Res(3) :: Bar-:State.Res("") :: StdIO.Res() :: HNil).futureValue

  println("Res:"+r)
  r should equal ("3_works_8")
}

it should "More complex State+Label+ simple effective" in {
  import Implicits._

  val eff = effective[Future]{ implicit ctx =>
    for {
      k0  <- Foo-:State.get[Int]
      _   <- Bar-:State.put("works")
      _   <- Foo-:State.update((i:Int) => i + 5)
      k   <- Foo-:State.get[Int]
      _   <- StdIO.putStrLn(s"tmp state:$k")
      _   <- Bar-:State.update((s:String) => s + s"_$k")
      s   <- Bar-:State.get[String]
      _   <- Foo-:State.updateM[Int, String]((k:Int) => k0.toString + s"_$s")
      r   <- Foo-:State.get[String]
      _   <- StdIO.putStrLn(s"final state:$r")
    } yield (r)
  }


  val r = eff.run(Foo-:State.Res(3) :: Bar-:State.Res("") :: StdIO.Res() :: HNil).futureValue

  println("Res:"+r)
  r should equal ("3_works_8")
}


/*  
  "Eff" should "flatMap isoList" in {
    val eff = State[Foo].put0[Future, Int](3).lift[(State@@Foo<>Int) :: (State@@Bar<>String) :: HNil].flatMap { _ => 
      State[Bar].put0[Future, String]("works").lift[(State@@Bar<>String) :: (State@@Foo<>Int) :: HNil]
    }

    val r = eff.run(MkEff[State@@Bar, String]("") :: MkEff[State@@Foo, Int](0) :: HNil).futureValue

    r should equal (())
  }

  it should "flatMap ESOBiggerThanESI2" in {
    val eff = State[Foo].put0[Future, Int](3).lift[(State@@Foo<>Int) :: (State@@Bar<>String) :: HNil].flatMap { _ => 
      State[Bar].put0[Future, String]("works")
    }

    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    r should equal (())
  }

  it should "flatMap ESOSmallerThanESI2" in {
    val eff = State[Foo].put0[Future, Int](3).flatMap { _ => 
      State[Bar].put0[Future, String]("works").lift[(State@@Bar<>String) :: (State@@Foo<>Int) :: HNil]
    }

    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    r should equal (())
  }

  it should "flatMap ESIESOPrepend" in {
    val eff = State[Foo].put0[Future, Int](3).flatMap { _ => 
      State[Bar].put0[Future, String]("works")
    }

    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    r should equal (())
  }

  it should "State without labels update" in {

    val eff =
      for {
        _  <- State[Foo].update0[Future, Int](i => i + 3)
        k  <- State[Foo].get0[Future, Int]
      } yield (k)

    val r = eff.run(MkEff[State@@Foo, Int](0) :: HNil).futureValue

    println("Res:"+r)
    r should equal (3)
  }

  it should "State without labels" in {

    val eff =
      for {
        _   <- State[Foo].put0[Future, Int](3)
        _   <- State[Bar].put0[Future, String]("works")
        k   <- State[Foo].get0[Future, Int]
        k2  <- State[Bar].get0[Future, String]
        _   <- State[Foo].putM0[Future, Int, String](k.toString + k2)
        k3  <- State[Foo].get0[Future, String]
      } yield (k3)

    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    println("Res:"+r)
    r should equal ("3works")
  }

  it should "State with 0 functions" in {

    val eff =
      for {
        _   <- State[Foo].put0[Future, Int](3)
        _   <- State[Bar].put0[Future, String]("works")
        k   <- State[Foo].get0[Future, Int]
        k2  <- State[Bar].get0[Future, String]
        _   <- State[Foo].putM0[Future, Int, String](k.toString + k2)
        k3  <- State[Foo].get0[Future, String]
      } yield (k3)

    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    println("Res:"+r)
    r should equal ("3works")
  }

  it should "State effective" in {

    val eff = effective[Future, (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil]{ implicit ctx =>
      for {
        k0  <- State[Foo].get[Int]
        _   <- State[Bar].put("works")
        _   <- State[Foo].update((i:Int) => i + 5)
        k   <- State[Foo].get[Int]
        _   <- State[Bar].update((s:String) => s + s"_$k")
        s   <- State[Bar].get[String]
        _   <- State[Foo].updateM[Int, String]((k:Int) => k0.toString + s"_$s")
        r   <- State[Foo].get[String]
      } yield (r)
    }


    val r = eff.run(MkEff[State@@Foo, Int](3) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    println("Res:"+r)
    r should equal ("3_works_8")
  }

  it should "State effective0" in {

    val eff = effective[Future]{ implicit ctx =>
      for {
        _   <- State[Foo].put(3)
        _   <- State[Bar].put("works")
        k   <- State[Foo].get[Int]
        k2  <- State[Bar].get[String]
        _   <- State[Foo].putM[Int, String](k.toString + k2)
        k3  <- State[Foo].get[String]
      } yield (k3)
    }


    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    println("Res:"+r)
    r should equal ("3works")
  }

  it should "mix State & StdIO" in {
    val eff = effective[Future, (State@@Foo<>Int) :: (State@@Bar<>String) :: (StdIO<>Unit) :: HNil]{ implicit ctx =>
      for {
        // _   <- StdIO.println(s"Enter it:")
        // s   <- StdIO.getStr()
        _   <- State[Foo].put(3)
        // _   <- State[Bar].put(s"works_$s")
        _   <- State[Bar].put(s"works")
        k   <- State[Foo].get[Int]
        _   <- StdIO.println(s"Foo $k")
        k2  <- State[Bar].get[String]
        _   <- StdIO.println(s"Bar $k2")
        _   <- State[Foo].putM[Int, String](k.toString + k2)
        k3  <- State[Foo].get[String]
      } yield (k3)
      // } yield (s -> k3)
    }

    // val (s, r) = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: MkEff[StdIO, Unit](()) :: HNil).futureValue
    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: MkEff[StdIO, Unit](()) :: HNil).futureValue

    println("Res:"+r)
    // r should equal (s"3works_$s")
    r should equal ("3works")
  }

  it should "FileIO simple" in {

    val eff = effective[Future, MkEff[FileIO, Unit] :: MkEff[StdIO, Unit] :: HNil] { implicit ctx =>
      for {
        b <-  FileIO.open[Read.type]("toto.txt")
        _ <-  b match {
                case true => for {
                  _ <- StdIO.println("Opened")
                  k <- FileIO.readLine
                  _ <- StdIO.println(s"Read $k")
                } yield ()

                case false => 
                  StdIO.println("Can't Open").lift[(FileIO<>FileStatus[Read.type])::(StdIO<>Unit)::HNil]
              }
        _ <- FileIO.close[Read.type]
      } yield (())
    }
    val r = eff.run(MkEff[FileIO, Unit](()) :: MkEff[StdIO, Unit](()) :: HNil).futureValue
    println("Res:"+r)
 
  }

  it should "FileIO write" in {

    val eff = effective[Future] { implicit ctx =>
      for {
        b <-  FileIO.open[Write.type]("tata.txt")
        _ <-  b match {
                case true => for {
                  _ <- StdIO.println("Opened")
                  _ <- FileIO.writeLine("blabla")
                  _ <- StdIO.println(s"wrote line")
                } yield ()

                case false => 
                  StdIO.println("Can't Open").lift[(FileIO<>FileStatus[Write.type])::(StdIO<>Unit)::HNil]
              }
        _ <- FileIO.close[Write.type]
      } yield (())
    }
    val r = eff.run(MkEff[FileIO, Unit](()) :: MkEff[StdIO, Unit](()) :: HNil).futureValue
    println("Res:"+r)
 
  }


  it should "FileIO read/write" in {

    // Just 
    

    val eff = effective[Future] { implicit ctx =>
      for {
        r <-  FileIO[Foo].open[Read.type]("toto.txt")
        w <-  FileIO[Bar].open[Write.type]("tata.txt")
        _ <-  { 
                // In current verion, we still need to lift all FX branches to a common local stack
                // to help Scalac in the recursion...
                type LocalStack = (FileIO@@Foo<>FileStatus[Read.type])::(FileIO@@Bar<>FileStatus[Write.type])::(StdIO<>Unit)::HNil
                
                if(r && w) {
                  def rec: EffM[Future, Unit, LocalStack, LocalStack] = for {
                    isEof <-  FileIO[Foo].isEof
                    _     <-  if(!isEof) {
                                for {
                                  s <-  FileIO[Foo].readLine
                                  _ <-  StdIO.print(s"Read $s")
                                  _ <-  s match {
                                          case Some(s) =>
                                            (for {
                                              _ <- FileIO[Bar].writeLine(s)
                                              _ <- StdIO.println(s"... Wrote $s")
                                            } yield ()).lift[LocalStack]
                                          case None => 
                                            StdIO.println(s"... Nothing to write").lift[LocalStack]
                                        }
                                  _ <- rec
                                } yield ()
                              } else {
                                StdIO.println("EOF").lift[LocalStack]
                              }
                  } yield ()
                  rec
                } else {
                  StdIO.println("Can't Open").lift[LocalStack]
                }
              }
        _ <-  FileIO[Foo].close[Read.type]
        _ <-  FileIO[Bar].close[Write.type]
      } yield (())
    }

    // Initialize Resources
    val r = eff.run(
              MkEff[FileIO@@Foo, Unit](()) ::
              MkEff[FileIO@@Bar, Unit](()) ::
              MkEff[StdIO, Unit](()) ::
              HNil
            ).futureValue
    println("Res:"+r)
 
  }
*/
}

    // implicitly[RemoveAll.Aux[
    //   MkEff[StdIO, Unit] :: MkEff[FileIO, FileStatus[Read.type]] :: HNil,
    //   MkEff[FileIO, FileStatus[Read.type]] :: HNil,
    //   (MkEff[FileIO, FileStatus[Read.type]] :: HNil, MkEff[StdIO, Unit] :: HNil)
    // ]]

    // implicitly[Merge.Aux[
    //   MkEff[StdIO, Unit] :: MkEff[FileIO, FileStatus[Read.type]] :: HNil,
    //   MkEff[StdIO, Unit] :: HNil,
    //   MkEff[StdIO, Unit] :: MkEff[FileIO, FileStatus[Read.type]] :: HNil
    // ]]
    // FlatMappable.ESOSmallerThanESI2Different[
    //   MkEff[FileIO, Unit] :: HNil,
    //   MkEff[FileIO, FileStatus[Read.type]] :: HNil,
    //   MkEff[StdIO, Unit] :: MkEff[FileIO, FileStatus[Read.type]] :: HNil,
    //   MkEff[StdIO, Unit] :: MkEff[FileIO, Unit] :: HNil,
    //   MkEff[StdIO, Unit] :: HNil,
    //   MkEff[FileIO, Unit] :: MkEff[StdIO, Unit] :: HNil
    // ]


    // implicitly[FlatMappable.Aux[
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil
    // ]]

    // implicitly[FlatMappable.Aux[
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Bar<>String) :: HNil,
    //   (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil
    // ]]

    // implicitly[FlatMappable.Aux[
    //   (State@@Foo<>Int) :: HNil,
    //   (State@@Foo<>Int) :: HNil,
    //   (State@@Bar<>String) :: HNil,
    //   (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil
    // ]]

    // FlatMappable.one[
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: HNil,
    //   (State@@Foo<>Int) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil
    // ]

    // val eff0: EffM[
    //   Future, Unit,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil
    // ] = State[Foo].put(3).flatMap { _ => State[Bar].put("works") }

    // implicitly[
    //   IsoList[
    //     (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //     (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil
    //   ]
    // ]

    // IsoList.tail[
    //   (State@@Foo<>Int), (State@@Bar<>String) :: HNil,
    //   (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil,
    //   (State@@Bar<>String) :: HNil
    // ]
    // FlatMappable.iso[
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
    //   (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil,
    //   (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil
    // ]
