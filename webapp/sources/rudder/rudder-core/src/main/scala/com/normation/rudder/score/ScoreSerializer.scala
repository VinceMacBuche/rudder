package com.normation.rudder.score

import zio.json._

object ScoreSerializer {

  implicit val scoreValueEncoder: JsonEncoder[ScoreValue] = JsonEncoder[String].contramap(_.value)
  implicit val scoreValueDecoder: JsonDecoder[ScoreValue] = JsonDecoder[String].mapOrFail(ScoreValue.fromString)

  implicit val noDetailsScoreEncoder: JsonEncoder[NoDetailsScore] = DeriveJsonEncoder.gen
  implicit val noDetailsScoreDecoder: JsonDecoder[NoDetailsScore] = DeriveJsonDecoder.gen

  implicit val globalScoreEncoder: JsonEncoder[GlobalScore] = DeriveJsonEncoder.gen
  implicit val globalScoreDecoder: JsonDecoder[GlobalScore] = DeriveJsonDecoder.gen
}
