package effpi_sandbox.EVoting

import effpi.process._
import effpi.process.dsl._
import effpi.channel.Channel
import effpi.channel.{InChannel, OutChannel}
import scala.concurrent.duration.Duration

case class Authenticate(x : String)
case class No(x : String)
case class Ok(x : String)
case class Reject(x : String)
case class Result(x : Int)
case class Yes(x : String)



type V[C0 <: OutChannel[Authenticate], C1 <: InChannel[Reject | Ok], C2 <: OutChannel[No | Yes], C3 <: InChannel[Result], C4 <: InChannel[Result]] = Out[C0, Authenticate] >>: In[C1, Reject | Ok, (x0 : Reject | Ok) => V0[x0.type, C2, C3, C4]]

type V0[X0 <: Reject | Ok, C2 <: OutChannel[No | Yes], C3 <: InChannel[Result], C4 <: InChannel[Result]] <: Process = X0 match {
  case Reject => PNil
  case Ok => ((Out[C2, No] >>: In[C4, Result, (x0 : Result) => PNil]) | (Out[C2, Yes] >>: In[C3, Result, (x0 : Result) => PNil]))
}

type S[C0 <: InChannel[Authenticate], C1 <: OutChannel[Reject | Ok], C2 <: InChannel[No | Yes], C3 <: OutChannel[Result], C4 <: OutChannel[Result]] = In[C0, Authenticate, (x0 : Authenticate) => ((Out[C1, Reject] >>: PNil) | (Out[C1, Ok] >>: In[C2, No | Yes, (x1 : No | Yes) => S0[x1.type, C3, C4]]))]

type S0[X0 <: No | Yes, C3 <: OutChannel[Result], C4 <: OutChannel[Result]] <: Process = X0 match {
  case No => Out[C4, Result] >>: PNil
  case Yes => Out[C3, Result] >>: PNil
}

implicit val timeout: Duration = Duration("60 seconds")

def v(c0 : OutChannel[Authenticate], c1 : InChannel[Reject | Ok], c2 : OutChannel[No | Yes], c3 : InChannel[Result], c4 : InChannel[Result]) : V[c0.type, c1.type, c2.type, c3.type, c4.type] = {
  println(s"-- V sending Authenticate on c0 ($c0)")
  send(c0, new Authenticate("")) >> {
    println(s"-- V sent Authenticate on c0 ($c0)")
    println(s"-- V expecting (Reject | Ok) on c1 ($c1)")
    receive(c1) {(x0 : Reject | Ok) =>
      println(s"-- V received (Reject | Ok) on c1 ($c1)")
      v0(x0, c2, c3, c4)
    }
  }
}

def v0(x : Ok, c2 : OutChannel[No | Yes], c3 : InChannel[Result], c4 : InChannel[Result]) : V0[x.type, c2.type, c3.type, c4.type] = x match {
  case y : Ok => {
    println(s"-- V received Ok")
    val x0 = 0
    if (x0 == 0) {
      send(c2, new No("")) >> {
        println(s"-- V sent No on c2 ($c2)")
        println(s"-- V expecting Result on c4 ($c4)")
        receive(c4) {(x1 : Result) => 
          println(s"-- V received Result on c4 ($c4)")
          println("-- V exits")
          nil
        }
      }
    } else {
      send(c2, new Yes("")) >> {
        println(s"-- V sent Yes on c2 ($c2)")
        println("-- V exits")
        nil
      }
    } 

  }
}

def s(c0 : InChannel[Authenticate], c1 : OutChannel[Reject | Ok], c2 : InChannel[No | Yes], c3 : OutChannel[Result], c4 : OutChannel[Result]) : S[c0.type, c1.type, c2.type, c3.type, c4.type] = {
  println(s"-- S expecting Authenticate on c0 ($c0)")
  receive(c0) {(x0 : Authenticate) => 
    println(s"-- S received Authenticate on c0 ($c0)")
    val x1 = 0
    if (x1 == 0) {
      send(c1, new Reject("")) >> {
        println(s"-- S sent Reject on c1 ($c1)")
        println("-- S exits")
        nil
      }
    } else {
      send(c1, new Ok("")) >> {
        println(s"-- S sent Ok on c1 ($c1)")
        println(s"-- S expecting (No | Yes) on c2 ($c2)")
        receive(c2) {(x2 : No | Yes) =>
          println(s"-- S received (No | Yes) on c2 ($c2)")
          s0(x2, c3, c4)
        }
      }
    } 

  }
}

def s0(x : No | Yes, c3 : OutChannel[Result], c4 : OutChannel[Result]) : S0[x.type, c3.type, c4.type] = x match {
  case y : No => {
    println(s"-- S received No")
    println(s"-- S sending Result on c4 ($c4)")
    send(c4, new Result(42)) >> {
      println(s"-- S sent Result on c4 ($c4)")
      println("-- S exits")
      nil
    }
  }
  case y : Yes => {
    println(s"-- S received Yes")
    println(s"-- S sending Result on c3 ($c3)")
    send(c3, new Result(42)) >> {
      println(s"-- S sent Result on c3 ($c3)")
      println("-- S exits")
      nil
    }
  }
}

object Main {
  def main() : Unit = main(Array())

  def main(args : Array[String]) = {
    var c0 = Channel[Authenticate]()
    var c1 = Channel[Reject | Ok]()
    var c2 = Channel[No | Yes]()
    var c3 = Channel[Result]()
    var c4 = Channel[Result]()

    eval(par(v(c0, c1, c2, c3, c4), s(c0, c1, c2, c3, c4)))
  }
}