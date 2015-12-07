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

// import file._
import state._
// import stdio._

class EffSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  trait Label
  case object Foo extends Label
  type Foo = Foo.type
  case object Bar extends Label
  type Bar = Bar.type

  "Eff" should "State" in {

    // type ES = State<>Foo@@Int :: HNil
    // type ES2 = State<>Foo@@String :: HNil

    val effectful = Effectful[
      Future,
      (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
      (State@@Foo<>String) :: (State@@Bar<>String) :: HNil
    ]

    import effectful._

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

    IsoList.tail[
      (State@@Foo<>Int), (State@@Bar<>String) :: HNil,
      (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil,
      (State@@Bar<>String) :: HNil
    ]
    FlatMappable.iso[
      (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
      (State@@Foo<>Int) :: (State@@Bar<>String) :: HNil,
      (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil,
      (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil
    ]


    val eff: EffM[Future, Int, ESI, ESO] = for {
      _   <- State[Foo].put(3)
      _   <- State[Bar].put("works")
      k   <- State[Foo].get[Int]
      k2  <- State[Bar].get[String]
      _   <- State[Foo].putB[Future, Int, String](k.toString + k2)
      // k3  <- State[Foo].get[String]
    } yield (k)

    // type ESI2 = (State@@Bar<>String) :: (State@@Foo<>Int) :: HNil

    // val eff2 = 
    //   State[Foo].put(3).flatMap { _ =>
    //     State[Bar].put("works").flatMap { _ =>
    //       State[Foo].get[Int].map(k => k)
    //     }
    //   }


    // val eff: EffM[Future, String, ES, ES2] = for {
    //   _ <- State.put[Future, Int @@ Foo, ES](5)
    //   _ <- State.put[Future, Int @@ Bar, ES](10)
    //   _ <- State.update[Future, Int @@ Foo, ES](i => i + 7)
    //   i <- State.get[Future, Int @@ Foo, ES]
    //   _ <- State.update[Future, Int @@ Bar, ES](j => i + j + 8)
    //   j <- State.get[Future, Int @@ Bar, ES]
    //   _ <- State.putM[Future, Int @@ Foo, String, ES](s"toto_$j")      
    //   k <- State.get
    // } yield (k)

    val r = eff.run(MkEff[State@@Foo, Int](0) :: MkEff[State@@Bar, String]("") :: HNil).futureValue

    println("Res:"+r)
    // r should equal ("3works")
  }

  /*"State" should "StateEnv" in {
    val env = Env[Future, MkEff[State, Int @@ Foo] :: MkEff[State, Int @@ Bar] :: HNil]
    import env._

    val stateEnv = StateEnv(env)

    val eff = for {
      _ <- stateEnv.put[Int @@ Foo](5)
      _ <- stateEnv.put[Int @@ Bar](10)
      _ <- stateEnv.update[Int @@ Foo](i => i + 7)
      i <- stateEnv.get[Int @@ Foo]
      _ <- stateEnv.update[Int @@ Bar](j => i + j + 8)
      _ <- stateEnv.putM[Int @@ Foo, String, MkEff[State, String] :: MkEff[State, Int @@ Bar] :: HNil]("toto")      
      j <- stateEnv.getM[Int @@ Bar, MkEff[State, String] :: MkEff[State, Int @@ Bar] :: HNil]
    } yield (j)

    val r = eff.run(MkEff[State, Int @@ Foo](0) :: MkEff[State, Int @@ Bar](5) :: HNil).futureValue

    println("Res:"+r)
    r should equal (30)
  }

  it should "StateEnv0" in {

    type ES = MkEff[State, Int @@ Foo] :: MkEff[State, Int @@ Bar] :: HNil
    type ES2 = MkEff[State, String] :: MkEff[State, Int @@ Bar] :: HNil

    val stateEnv = StateEnv0[Future]()

    val eff = for {
      _ <- stateEnv.put[Int @@ Foo, ES](5)
      _ <- stateEnv.put[Int @@ Bar, ES](10)
      _ <- stateEnv.update[Int @@ Foo, ES](i => i + 7)
      i <- stateEnv.get[Int @@ Foo, ES]
      _ <- stateEnv.update[Int @@ Bar, ES](j => i + j + 8)
      j <- stateEnv.get[Int @@ Bar, ES]
      _ <- stateEnv.putM[Int @@ Foo, String, ES, ES2](s"toto_$j")      
      k <- stateEnv.get[String, ES2]
    } yield (k)

    val r = eff.run(MkEff[State, Int @@ Foo](0) :: MkEff[State, Int @@ Bar](5) :: HNil).futureValue

    println("Res:"+r)
    r should equal ("toto_30")
  }
  
  it should "FileIO" in {
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