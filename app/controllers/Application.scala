package controllers

import models.{DB, Hardata}
import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.data.Form
import play.api.data.Forms._
import scala.language.postfixOps
import scala.io.Source
import scala.math.Ordering.Implicits._



class Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

	def upload = Action(parse.multipartFormData) { request =>
	  request.body.file("har").map { har =>
	    import java.io.File
	    val ref = har.ref.toString()
	    val tempFilePath = ref.substring(ref.indexOf("(") + 1, ref.length - 1)
	    // println("Temp File Path: " + tempFilePath)
	  	var data = scala.io.Source.fromFile(tempFilePath).getLines.mkString("\n")
			// println(data.getClass.getSimpleName)
			val jsonObject = scala.util.parsing.json.JSON.parseFull(data)
			val globalMap = jsonObject.get.asInstanceOf[Map[String, Any]]
			val logMap = globalMap.get("log").get.asInstanceOf[Map[String, Any]]
			//Page Values
			val pageData = logMap.get("pages").get.asInstanceOf[List[Map[String, Any]]]
			println("Page URL: " + pageData(0).get("title").get.asInstanceOf[String])
			pageData.foreach(i => println("  Page onContentLoad: " +
				i.get("pageTimings").get.asInstanceOf[Map[String, Any]]
				 .get("onContentLoad").get.asInstanceOf[Double])
			)
			pageData.foreach(i => println("  Page onLoad: " +
				i.get("pageTimings").get.asInstanceOf[Map[String, Any]]
				 .get("onLoad").get.asInstanceOf[Double])
			)
			//Entry Values
			val entryData = logMap.get("entries").get.asInstanceOf[List[Map[String, Any]]]	
			var loadTime = 0.0
			entryData.foreach(i => loadTime += i.get("time").get.asInstanceOf[Double])
			val avgLatency = entryData.map(i => 
					 i.get("time").get.asInstanceOf[Double] - 
					 i.get("timings").get.asInstanceOf[Map[String, Any]]
					  .get("receive").get.asInstanceOf[Double]
				).sum / entryData.length
			val avgSend = entryData.map(i => i.get("timings").get.asInstanceOf[Map[String, Any]]
					 .get("send").get.asInstanceOf[Double]
				).sum / entryData.length
			val avgWait = entryData.map(i => i.get("timings").get.asInstanceOf[Map[String, Any]]
					 .get("wait").get.asInstanceOf[Double]
				).sum / entryData.length
			val avgReceive = entryData.map(i => i.get("timings").get.asInstanceOf[Map[String, Any]]
					 .get("receive").get.asInstanceOf[Double]
				).sum / entryData.length
			//Size
			val contentSize = entryData.map(i => i.get("response").get.asInstanceOf[Map[String, Any]]
					 .get("content").get.asInstanceOf[Map[String, Any]]
					 .get("size").get.asInstanceOf[Double]
				).sum		
			val entrySize = entryData.map(i => 
					i.get("request").get.asInstanceOf[Map[String, Any]]
					 .get("headersSize").get.asInstanceOf[Double] + 
					i.get("request").get.asInstanceOf[Map[String, Any]]
					 .get("bodySize").get.asInstanceOf[Double]
				).sum
			//Entry Worst Offenders
			val time = entryData.map(i => 
					i.get("time").get.asInstanceOf[Double])
			val contentSizeList = entryData.map(i => i.get("response").get.asInstanceOf[Map[String, Any]]
					 .get("content").get.asInstanceOf[Map[String, Any]]
					 .get("size").get.asInstanceOf[Double]
				)
			val name = entryData.map(i => 
					(i.get("request").get.asInstanceOf[Map[String, Any]]
					 .get("url").get.asInstanceOf[String]).substring(
					 (i.get("request").get.asInstanceOf[Map[String, Any]]
					 .get("url").get.asInstanceOf[String]).lastIndexOf("/") + 1,
		 			 (i.get("request").get.asInstanceOf[Map[String, Any]]
					 .get("url").get.asInstanceOf[String]).length)
					)
			val entryAllTime = name.zip(time).toMap
			val entryAllSize = name.zip(contentSizeList).toMap
			val entryTimeTen = entryAllTime.toSeq.sortWith(_._2 > _._2).take(10)
			val entrySizeTen = entryAllSize.toSeq.sortWith(_._2 > _._2).take(10)
			//Entry Content-Type
			val contentType = entryData.map(i => i.get("response").get.asInstanceOf[Map[String, Any]]
					 .get("content").get.asInstanceOf[Map[String, Any]]
					 .get("mimeType").get.asInstanceOf[String]
				)
			val contentTypeCount = contentType.groupBy(x=>x).mapValues(x=>x.length)		
			//Entry Status
			val statusType = entryData.map(i => 
					 i.get("response").get.asInstanceOf[Map[String, Any]]
					 	.get("status").get.asInstanceOf[Double] + ": " + 
					 i.get("response").get.asInstanceOf[Map[String, Any]]
					 	.get("statusText").get.asInstanceOf[String] 
				)
			val statusTypeCount = statusType.groupBy(x=>x).mapValues(x=>x.length)
			//Cookies
			val numOfCookies = entryData.map(i => 
					 i.get("request").get.asInstanceOf[Map[String, Any]]
					 	.get("cookies").get.asInstanceOf[List[String]].length + 
					 i.get("response").get.asInstanceOf[Map[String, Any]]
					 	.get("cookies").get.asInstanceOf[List[String]].length 
				).sum
//			DB.save(numOfCookies)

			//Print Values
			println("Total Requests: " + entryData.length)
			println(" Transferred (Entry): " + entrySize)
			println(" Transferred (Content): " + contentSize)
			println("   Avg. Transfer: " + contentSize/entryData.length)
			println(" Duration (Total): " + loadTime)
			println("   Avg. Duration: " + loadTime/entryData.length)
			println("   Avg. Latency: " + avgLatency)
			println(" Avg. Time Request Sent: " + avgSend)
			println(" Avg. Time Waiting: " + avgWait)
			println(" Avg. Time to Download: " + avgReceive)
			println("Number of Cookies: " + numOfCookies)
			println("Response Content-Types:")
			for((k,v) <- contentTypeCount) println("  " + k + " (" + v + ")")
			println("Response Statuses:")
			for((k,v) <- statusTypeCount) println("  " + k + " (" + v + ")")
			println("Worst Offenders (by LoadTime)")
			for((k,v) <- entryTimeTen) println("   Duration: (" + v + ") Entry Name: " + k)
			println("Worst Offenders (by Size)")
			for((k,v) <- entrySizeTen) println("   Size: (" + v + ") Entry Name: " + k)

			val message = "Total Requests: " + entryData.length + " Transferred (Entry): " + entrySize

	    Ok(views.html.index(message))
	  }.getOrElse {
	    Redirect(routes.Application.index).flashing(
	      "error" -> "Missing file"
	    )
	  }
	}
	def getData = Action {
		val dbData = DB.query[Hardata].fetch
		Ok(Json.toJson(dbData))
	}

}



	// def upload = Action(parse.multipartFormData) { request =>
	//   request.body.file("har").map { har =>
	//     import java.io.File
	//     val ref = har.ref.toString()
	//     val tempFilePath = ref.substring(ref.indexOf("(") + 1, ref.length - 1)
	//     println("Temp File Path: " + tempFilePath)
	//   	var data = scala.io.Source.fromFile(tempFilePath).getLines().mkString("\n")
	// 		println(data.getClass.getSimpleName)
	// 		// println(JSON.parseFull(data))

	//     Ok(data)
	//   }.getOrElse {
	//     Redirect(routes.Application.index).flashing(
	//       "error" -> "Missing file"
	//     )
	//   }
	// }