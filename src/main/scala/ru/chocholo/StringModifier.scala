package ru.chocholo

import ru.chocholo.StringModifier.StringModifierOps
import ru.chocholo.StringTest.maskStringsPoly
import shapeless.ops.hlist.{Mapper, Replacer}
import shapeless.{::, Generic, HList, HNil, Lazy, Poly, Poly1}


trait StringModifier[A] {
  def map(current: A, mappingFunc: String => String): A
}

object StringModifier {
  implicit val stringModifier: StringModifier[String] = (value, mapping) => mapping(value)
  implicit val intModifier: StringModifier[Int] = (a, _) => a

  implicit val hNilInstance: StringModifier[HNil] = (a, _) => a

  implicit def hListInstance[H, T <: HList](implicit hInstance: Lazy[StringModifier[H]], tInstance: Lazy[StringModifier[T]]): StringModifier[H :: T] =
  {
    case (h :: t, mappingFunc) =>
      hInstance.value.map(h, mappingFunc) :: tInstance.value.map(t, mappingFunc)
  }

  implicit def genericInstance[A, R](implicit generic: Generic.Aux[A, R], rInstance: Lazy[StringModifier[R]]): StringModifier[A] =
    (value, func) => generic.from(rInstance.value.map(generic.to(value), func))

  implicit class StringModifierOps[A](value: A) {
    def markStrings(mappingFunc: String => String)(implicit modifier: StringModifier[A]): A =
      modifier.map(value, mappingFunc)
  }
}

object Masking extends Poly1 {
  implicit val stringCase: Case.Aux[String, String] = at(_ => "XXX")
  implicit def anyValCase[P <: AnyVal]: Case.Aux[P, P] = at(identity)

  implicit def genericInstance[A, ARepr <: shapeless.HList](
    implicit generic: Generic.Aux[A, ARepr],
    mapper: Mapper.Aux[Masking.type, ARepr, ARepr]
  ): Case.Aux[A, A] =
    at(v => maskStringsPoly(v)(Masking))
}

case class StringModificationTest(a: Int, b: String, c: Int, sub: StringSubClassTest)
case class StringSubClassTest(test: String)

object StringTest {

  def updateString[A, ARepr <: shapeless.HList](value: A)(
    implicit aGen: shapeless.Generic.Aux[A, ARepr],
    replacer: Replacer.Aux[ARepr, String, String, (String, ARepr)],
  ): A = aGen.from(aGen.to(value).updatedElem("XX"))

  def maskStringsPoly[A, ARepr <: shapeless.HList, P <: Poly](value: A)(poly: P)(
    implicit aGen: shapeless.Generic.Aux[A, ARepr],
    mapper: Mapper.Aux[P, ARepr, ARepr]
  ): A = aGen.from(mapper.apply(aGen.to(value)))


  def main(args: Array[String]): Unit = {
    // 1
    println(StringModificationTest(1, "abc", 2, StringSubClassTest("1")).markStrings(_ => "X"))

    // 2
    println(updateString(StringModificationTest(1, "abc", 2, StringSubClassTest("2"))))

    //3
    println(maskStringsPoly(StringModificationTest(1, "abc", 2, StringSubClassTest("3")))(Masking))
  }
}
