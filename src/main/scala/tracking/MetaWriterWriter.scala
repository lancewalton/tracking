package tracking

import argonaut.Argonaut.ToJsonIdentity
import tracking.json.metaCodec
import tracking.model.architecture._
import tracking.model.IdentifierAndTitle
import tracking.model.Meta

object MetaWriter extends App {
  println(Meta(IdentifierAndTitle("foo", "Foo"), "the function").asJson.spaces2)
}