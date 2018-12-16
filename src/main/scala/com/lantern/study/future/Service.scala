package com.lantern.study.future

import scala.concurrent.{ExecutionContext, Future}

class Service {
  implicit val ec = ExecutionContext.global
  def execute(request: Request): Future[Response] = {
    Future {
      println(s"Received: request: $request")
      Thread.sleep(request.timeToLive * 1000)
      val response = new ResponseError(request.id)
      println(s"Sent: response: $response")
      response
    }
  }
}

