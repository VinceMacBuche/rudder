package com.normation.rudder.score

import zio.json._

object ScoreSerializer {

  implicit val scoreValueEncoder: JsonEncoder[ScoreValue] = JsonEncoder[String].contramap(_.value)
  implicit val scoreValueDecoder: JsonDecoder[ScoreValue] = JsonDecoder[String].mapOrFail(ScoreValue.fromString)

  implicit val globalScoreValueEncoder: JsonEncoder[GlobalScore] = DeriveJsonEncoder.gen
  implicit val globalScoreValueDecoder: JsonDecoder[GlobalScore] = DeriveJsonDecoder.gen
}
