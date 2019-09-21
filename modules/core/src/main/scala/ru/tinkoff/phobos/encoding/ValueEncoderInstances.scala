package ru.tinkoff.phobos.encoding

import java.util.Base64

trait ValueEncoderInstances {

  implicit val booleanEncoder: ValueEncoder[Boolean]       = _.toString
  implicit val charEncoder: ValueEncoder[Char]             = _.toString
  implicit val byteEncoder: ValueEncoder[Byte]             = _.toString
  implicit val shortEncoder: ValueEncoder[Short]           = _.toString
  implicit val intEncoder: ValueEncoder[Int]               = _.toString
  implicit val longEncoder: ValueEncoder[Long]             = _.toString
  implicit val floatEncoder: ValueEncoder[Float]           = _.toString
  implicit val doubleEncoder: ValueEncoder[Double]         = _.toString
  implicit val bigDecimalEncoder: ValueEncoder[BigDecimal] = _.toString
  implicit val bigIntEncoder: ValueEncoder[BigInt]         = _.toString
  implicit val base64Encoder: ValueEncoder[Array[Byte]]    = Base64.getEncoder.encodeToString _ // TODO: hexEncoder

}
