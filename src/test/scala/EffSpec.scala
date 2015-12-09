package effects

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalatest.concurrent.{AsyncAssertions, PatienceConfiguration, ScalaFutures}
import org.scalatest.{Assertions, BeforeAndAfterAll, FeatureSpec, FlatSpec, Matchers, Suite}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import cats.std.future._
import cats.data.Xor

import shapeless._
import ops.hlist._

import file._
import state._
import stdio._

class EffSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  trait Label
  case object Foo extends Label
  type Foo = Foo.type
  case object Bar extends Label
  type Bar = Bar.type
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
*/
  it should "simple FileIO" in {

    type ES = (FileIO<>FileStatus[Read.type])::(StdIO<>Unit)::HNil

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

  /*it should "FileIO" in {
    type ES = (FileIO<>Unit) :: (StdIO<>Unit) :: HNil
    type ES2 = (FileIO<>FileStatus[Read.type]) :: (StdIO<>Unit) :: HNil

    val eff = for {
      b <-  FileIO.open[Future, Read.type, ES, ES2]("toto.txt")
      _ <-  b match {
              case true   => 
                for {
                  l <-  FileIO.readLine[Future, ES2]
                  _ <-  l match {
                          case Some(l) => StdIO.putStrLn[Future, ES2](l)
                          case None => StdIO.putStrLn[Future, ES2]("")
                        }
                  _ <-  FileIO.close[Future, Read.type, ES2, ES]
                } yield ()
                
              case false  => for {
                _ <- StdIO.putStrLn[Future, ES2]("Can't open file")
                _ <- FileIO.close[Future, Read.type, ES2, ES]
              } yield ()
            }
    } yield ()

    val r = eff.run(MkEff[FileIO, Unit](()) :: MkEff[StdIO, Unit](()) :: HNil).futureValue

    println("Res:"+r)
  }

  it should "FileIO with labels" in {
    type ES = (FileIO@@Foo<>Unit) :: (StdIO<>Unit) :: HNil
    type ES2 = (FileIO@@Foo<>FileStatus[Read.type]) :: (StdIO<>Unit) :: HNil

    val eff = for {
      b <-  FileIO[Foo].open[Future, Read.type, ES, ES2]("toto.txt")
      _ <-  b match {
              case true   => 
                for {
                  l <-  FileIO[Foo].readLine[Future, ES2]
                  _ <-  l match {
                          case Some(l) => StdIO.putStrLn[Future, ES2](l)
                          case None => StdIO.putStrLn[Future, ES2]("")
                        }
                  _ <-  FileIO[Foo].close[Future, Read.type, ES2, ES]
                } yield ()
                
              case false  => for {
                _ <- StdIO.putStrLn[Future, ES2]("Can't open file")
                _ <- FileIO[Foo].close[Future, Read.type, ES2, ES]
              } yield ()
            }
    } yield ()

    val r = eff.run(MkEff[FileIO@@Foo, Unit](()) :: MkEff[StdIO, Unit](()) :: HNil).futureValue

    println("Res:"+r)
  }

  it should "FileIO read + write" in {
    type ES =   (FileIO@@Foo<>Unit) ::
                (FileIO@@Bar<>Unit) ::
                (StdIO<>Unit) ::
                (State<>Int) ::
                HNil

    type ES2 =  (FileIO@@Foo<>FileStatus[Read.type]) ::
                (FileIO@@Bar<>Unit) ::
                (StdIO<>Unit) ::
                (State<>Int) ::
                HNil


    type ES3 =  (FileIO@@Foo<>FileStatus[Read.type]) ::
                (FileIO@@Bar<>FileStatus[Write.type]) ::
                (StdIO<>Unit) ::
                (State<>Int) ::
                HNil

    val eff = for {
      r <-  FileIO[Foo].open[Future, Read.type, ES, ES2]("toto.txt")
      w <-  FileIO[Bar].open[Future, Write.type, ES2, ES3]("tata.txt")
      _ <-  if(r && w) {
                def rw: EffM[Future, Unit, ES3, ES3] = for {
                  isEof <- FileIO[Foo].isEof[Future, ES3]
                  _ <-  if(!isEof) 
                          for {
                            s <-  FileIO[Foo].readLine[Future, ES3]
                            _ <-  s match {
                                    case Some(s) => for {
                                      _ <- FileIO[Bar].writeLine[Future, ES3](s)
                                      _ <- StdIO.putStrLn[Future, ES3](s"Read/Write $s")
                                      _ <- State.update[Future, Int, ES3](_ + 1)
                                    } yield ()
                                    case None => EffM.pure[Future, ES3, Unit](())
                                  }
                            _ <-  rw
                          } yield ()
                        else EffM.pure[Future, ES3, Unit](())
                } yield (())

                rw
            } else StdIO.putStrLn[Future, ES3](s"Couldn't open one of files $r $w")

      _ <-  FileIO[Bar].close[Future, Write.type, ES3, ES2]
      _ <-  FileIO[Foo].close[Future, Read.type, ES2, ES]
      i <-  State.get[Future, Int, ES]
    } yield (i)

    val r = eff.run(
      MkEff[FileIO@@Foo, Unit]() ::
      MkEff[FileIO@@Bar, Unit]() ::
      MkEff[StdIO, Unit]() ::
      MkEff[State, Int](0) ::
      HNil
    ).futureValue

    println(s"Read/Write $r lines")
  }

  it should "FileIO read + write 2" in {
    type ES =   (FileIO<>Unit) ::
                HNil

    type ES2 =  (FileIO<>FileStatus[Read.type]) ::
                HNil

    val eff: EffM[Future, ] = for {
      r <-  FileIO.open2[Future, Read.type, ES]("toto.txt")
      r <-  FileIO.close2[Future, Read.type, ES2]
    } yield (())
  }*/
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
