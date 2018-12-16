package animal

class Animal {

  private val id : Int = 1

  override def toString: String = s"id: $id"
}

class Cat extends Animal {

  private val id: Int = 2
  //
//  override def toString: String = super.toString
  override def toString: String = s"id: $id"



}




