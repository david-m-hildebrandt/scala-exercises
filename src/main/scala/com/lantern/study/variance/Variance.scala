package com.lantern.study.variance

object Variance {

  class Animate

  class Animal extends Animate

  class Cow extends Animal

  class Co[+A]

  class Contra[-A]

  def covariant(a: Co[Animal]): Unit = ()
  def contraVariant(a: Contra[Animal]): Unit = ()

  def main(a: Array[String]): Unit = {

    covariant(new Co[Cow]) // Cow is child to Animal, allowed by Co[+A] and Co[Animal], so covariant
    //covariant(new Co[Animate]) // Animate is parent to Animal, not allowed by Co[+A]

    // contraVariant(new Contra[Cow]) // Cow is child to Animal, not allowed by Contra[-A]
    contraVariant(new Contra[Animate]) // Animate is parent to Animal, allowed by Contra[-A] and Contra[Animal]
  }

}
