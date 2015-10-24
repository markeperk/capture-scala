package models

import play.api.libs.json.Json

case class Hardata(data: Int)

object Hardata {

   implicit val hardataFormat = Json.format[Hardata]
}
