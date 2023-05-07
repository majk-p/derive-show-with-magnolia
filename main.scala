//> using scala "3.2.2"
//> using lib "org.typelevel::cats-effect:3.4.10"
//> using lib "org.typelevel::cats-core:2.9.0"
//> using lib "com.softwaremill.magnolia1_3::magnolia:1.3.0"
//> using options "-source:future"

import magnolia1.*

trait Show[A] {
  def show(value: A): String
}

object Show {

  object givens extends AutoDerivation[Show] {

    given Show[String] = value => value
    given [A](using Numeric[A]): Show[A] = _.toString

    // generate Show instance for case classes 
    override def join[T](caseClass: CaseClass[Show, T]): Show[T] = 
      new Show[T] {
        def show(value: T) = { 
          val name = caseClass.typeInfo.short
          val serializedParams = caseClass.parameters.map { parameter =>
            s"${parameter.label} = ${parameter.typeclass.show(parameter.deref(value))}"
          }.mkString(", ")
          s"$name($serializedParams)"
        }
      }
      
    // generate Show instance for sealed traits
    override def split[T](sealedTrait: SealedTrait[Show, T]): Show[T] = 
      new Show[T] {
        def show(value: T): String = 
          sealedTrait.choose(value){ subtype => 
            subtype.typeclass.show(subtype.cast(value))
          }
        
      }

  }

}


case class MyCaseClass(number: Long, name: String)
enum Animal {
  case Dog
  case Cat
  case Other(kind: String)
}


@main
def main() = {
  import Show.givens.given

  println(
    summon[Show[MyCaseClass]].show(MyCaseClass(number = 5, name = "test"))
  )
  println(
    summon[Show[Animal]].show(Animal.Dog)
  )
  println(
    summon[Show[Animal]].show(Animal.Other("snake"))
  )
  
}