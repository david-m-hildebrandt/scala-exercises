package com.lantern.study.future

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class FutureWork {


}

object FutureWork {

  implicit val ec = ExecutionContext.global

  def main(args: Array[String]): Unit = {

    getData.onComplete {
      case Success(result) => println(s"getDataResult: main $result")
      case Failure(_) => println("failed")
    }

    Thread.sleep(5000)

  }

  def getData: Future[String] = {
    println("FutureWork.main: Start")
    val requestA = Request("a", hasRandomLifespan = false, 4)
    val requestB = Request("b", hasRandomLifespan = false, 1)

    val service = new Service

    val responseA = service.execute(requestA)
    val responseB = service.execute(requestB)

    /*    while (!(responseA.isCompleted && responseB.isCompleted)) {
          Thread.sleep(100)
          println("waited 100")
        }
    */
    var output = ""

    responseA.onComplete {
      case Success(rA) => responseB.onComplete { case Success(rB) =>
        output = s"Response A: $rA Response B: $rB"
        println(s"getDataResult: onComplete: $output")

      }
    }

    responseA.flatMap(rA => responseB.map(rB => s"Response A: $rA Response B: $rB"))

  }
}
