package effpi.examples.recovery.test

import scala.concurrent.duration.Duration

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import scala.util.Random

package object types {
  case class Data()

  type T1 = OK | Req
  case class OK()
  case class Req()

  type P[C1 <: OChan[Data], C2 <: OChan[Data]] = Out[C1, Data] >>: Out[C2, Data]

  type Q1[C4 <: OChan[Data], Y <: T1] <: Process = Y match {
    case OK => PNil
    case Req => Out[C4, Data]
  }

  type Q[C1 <: IChan[Data], C3 <: IChan[T1], C4 <: OChan[Data]] =
    In[C1, Data, (x : Data) => In[C3, T1, (y : T1) => Q1[C4, y.type]]]

  type R[C2 <: IChan[Data], C3 <: OChan[T1], C4 <: IChan[Data]] =
    // In[C2, Data, (x : Data) => Out[C3, OK]]
    InErr[C2, Data, (x : Data) => Out[C3, OK],
          (err : Throwable) => (Out[C3, Req] >>: In[C4, Data, (y : Data) => PNil])]
}

package object implementation {
  import types._
  implicit val timeout: Duration = Duration("100 seconds")

  val rng = new Random()

  def setupP(c1 : OChan[Data], c2 : OChan[Data]) : P[c1.type, c2.type] = {
    send(c1, Data()) >> {
      println(s"[P] Am I going to die?")
      if (rng.nextBoolean()) throw RuntimeException("[P] Yes.")
      println(s"[P] No.")
      send(c2, Data()) // could make it so that it errors here
    }
  }

  def setupQ(c1 : IChan[Data], c3 : IChan[T1], c4 : OChan[Data]) : Q[c1.type, c3.type, c4.type] = {
    receive(c1) { data =>
      println(s"[Q] Received Data from P.")
      receive(c3) { t1 =>
        println(s"[Q] Received msg from R: ${t1}")
        t1 match {
          case _ : OK => nil
          case _ : Req => send(c4, data)
        }
      }
    }
  }

  // def setupR(c2 : IChan[Data], c3 : OChan[T1], c4 : IChan[Data]) : R[c2.type, c3.type, c4.type] = {
  //   receive(c2) { (data : Data) =>
  //     println(s"[R] Received Data from P.")
  //     send(c3, OK())
  //   }
  //   { (err : Throwable) =>
  //     println(s"[R] Timout waiting on P; assume it crashed.")
  //     send(c3, Req()) >>
  //     receive(c4) { (data : Data) =>
  //       println(s"[R] Received Data from Q.")
  //       nil
  //     }
  //   }
  // }

  //** Intention (doesn't work)
  def setupR(c2 : IChan[Data], c3 : OChan[T1], c4 : IChan[Data]) : R[c2.type, c3.type, c4.type] = {
    receiveErr(c2) ({ (data : Data) =>
      println(s"[R] Received Data from P.")
      send(c3, OK())
    },
    { (err : Throwable) =>
      println(s"[R] Timout waiting on P; assume it crashed.")
      send(c3, Req()) >>
      receive(c4) { (data : Data) =>
        println(s"[R] Received Data from Q.")
        nil
      }
    }, Duration("5 seconds"))
  }

  //***********************************************************************************************
  //** Sanity check

  //** Works fine:
  type Test1[C1 <: IChan[Data], C2 <: OChan[T1]] =
    InErr[C1, Data,
          ((x : Data) => PNil),
          ((err : Throwable) => PNil)]
  def test1(c1 : IChan[Data], c2 : OChan[T1]) : Test1[c1.type, c2.type] = {
    receiveErr(c1)(((data : Data) => nil),((err : Throwable) => nil), Duration("5 seconds"))
  }

  //** Doesn't typecheck:
  type TestOK[C2 <: OChan[T1]] = Out[C2, T1]
  type Test2[C1 <: IChan[Data], C2 <: OChan[T1]] =
    InErr[C1, Data, ((x : Data) => TestOK[C2]), ((err : Throwable) => PNil)]

  def testOK(c2 : OChan[T1]) : TestOK[c2.type] = send(c2, OK()) // Typechecks
  def test2(c1 : IChan[Data], c2 : OChan[T1]) : Test2[c1.type, c2.type] = { // Doesn't
    receiveErr(c1)(((data : Data) => testOK(c2)),((err : Throwable) => nil), Duration("5 seconds"))
  }

  //** END
  //***********************************************************************************************

  def setup(c1 : Chan[Data], c2 : Chan[Data], c3 : Chan[T1], c4 : Chan[Data]) : Par3[P[c1.type, c2.type], Q[c1.type, c3.type, c4.type], R[c2.type, c3.type, c4.type]] = {
    par(setupP(c1, c2), setupQ(c1, c3, c4), setupR(c2, c3, c4));
  }
}

// To run this example, try:
// sbt "examples/runMain effpi.examples.trying.test.Main"
object Main {
  import types._
  import implementation._
  def main(): Unit = main(Array())

  def main(args: Array[String]) = {
    val (c1, c2, c3, c4) = (Chan[Data](),   // P -> Q
                            Chan[Data](),   // P -> R
                            Chan[T1](),     // R -> Q
                            Chan[Data]())   // Q -> R

    eval(setup(c1, c2, c3, c4))
  }
}
