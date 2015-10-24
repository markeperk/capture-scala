package models


import sorm._

object DB extends Instance(
  entities = Seq(Entity[Hardata]()),
  url = "jdbc:h2:mem:test"
)
