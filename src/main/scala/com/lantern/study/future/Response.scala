package com.lantern.study.future

sealed case class Response (id: String) {
//  override def toString: String = this.getClass.getSimpleName
}
class ResponseSuccess (id: String) extends Response (id: String)
class ResponseError (id: String) extends Response (id: String)

