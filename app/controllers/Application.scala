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
import scala.collection.mutable.ListBuffer


class Application extends Controller {

  def index = Action {
  	val message = List("Please upload a .har file. Thank you")
    Ok(views.html.index(message))
  }
	def upload = Action(parse.multipartFormData) { request =>
	  request.body.file("har").map { har =>
	    import java.io.File
	    val ref = har.ref.toString()
	    val tempFilePath = ref.substring(ref.indexOf("(") + 1, ref.length - 1)
	  	var data = scala.io.Source.fromFile(tempFilePath).getLines.mkString("\n")
			val jsonObject = scala.util.parsing.json.JSON.parseFull(data)
			val globalMap = jsonObject.get.asInstanceOf[Map[String, Any]]
			val logMap = globalMap.get("log").get.asInstanceOf[Map[String, Any]]
			//Page Info
			val pageData = logMap.get("pages").get.asInstanceOf[List[Map[String, Any]]]
			val pageURL = pageData(0).get("title").get.asInstanceOf[String]
			val pageOnContentLoad = pageData(0)
				 .get("pageTimings").get.asInstanceOf[Map[String, Any]]
				 .get("onContentLoad")
				 .get.asInstanceOf[Double]
			val pageOnLoad = pageData(0)
				 .get("pageTimings").get.asInstanceOf[Map[String, Any]]
				 .get("onLoad").get.asInstanceOf[Double]
			//Entry Values
			val entryData = logMap.get("entries").get.asInstanceOf[List[Map[String, Any]]]	
			var loadTime = 0.0 //just trying foreach vs map
					entryData.foreach(i => loadTime += i.get("time").get.asInstanceOf[Double])
			val avgLatency = entryData.map( i => 
				  i.get("time").get.asInstanceOf[Double] - 
				  i.get("timings").get.asInstanceOf[Map[String, Any]]
				   .get("receive").get.asInstanceOf[Double]
				).sum / entryData.length
			val avgSend = entryData.map( i => 
					i.get("timings").get.asInstanceOf[Map[String, Any]]
					 .get("send").get.asInstanceOf[Double]
				).sum / entryData.length
			val avgWait = entryData.map( i => 
					i.get("timings").get.asInstanceOf[Map[String, Any]]
					 .get("wait").get.asInstanceOf[Double]
				).sum / entryData.length
			val avgReceive = entryData.map(i => 
					i.get("timings").get.asInstanceOf[Map[String, Any]]
					 .get("receive").get.asInstanceOf[Double]
				).sum / entryData.length
			//Approaches to Request Size
			val contentSize = entryData.map(i => 
					i.get("response").get.asInstanceOf[Map[String, Any]]
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
			val contentSizeList = entryData.map(i => 
					i.get("response").get.asInstanceOf[Map[String, Any]]
					 .get("content").get.asInstanceOf[Map[String, Any]]
					 .get("size").get.asInstanceOf[Double]
				)
			val name = entryData.map { i => 
					var req = i.get("request").get.asInstanceOf[Map[String, Any]]
										 .get("url").get.asInstanceOf[String]
					if (req.lastIndexOf('/') == req.length - 1) {
							req = req.substring(0, req.length - 1)
					}
					req.substring(req.lastIndexOf("/") + 1, req.length)
				}
			val entryAllTime = name.zip(time).toMap
			val entryAllSize = name.zip(contentSizeList).toMap
			val entryTimeTen = entryAllTime.toSeq.sortWith(_._2 > _._2).take(10)
			val entrySizeTen = entryAllSize.toSeq.sortWith(_._2 > _._2).take(10)
			//Entry Content-Type
			val contentType = entryData.map(i => 
					i.get("response").get.asInstanceOf[Map[String, Any]]
					 .get("content").get.asInstanceOf[Map[String, Any]]
					 .get("mimeType").get.asInstanceOf[String]
				)
			val contentTypeCount = contentType.groupBy(x=>x).mapValues(x=>x.length)		
			//Entry Status
			val statusType = entryData.map(i => 
					 (i.get("response").get.asInstanceOf[Map[String, Any]]
					 	.get("status").get.asInstanceOf[Double]).toInt + ": " + 
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
			//DB.save(numOfCookies) //wrong "Type" to save data - will work on this later
			//Data List
			var dataList = new ListBuffer[String]()
					dataList += "Filename: " + har.filename
					dataList += "--------"
					dataList += "Page URL: " + pageURL
					dataList += "- Page onContentLoad: " + convertTime(pageOnContentLoad)
					dataList += "- Page onLoad: " + convertTime(pageOnLoad)
					dataList += "--------"
					dataList += "Total Requests: " + entryData.length
					dataList += "Transferred (Entry-Size): " + formatBytes(entrySize)
					dataList += "Transferred (Content-Size): " + formatBytes(contentSize)
					dataList += "- Avg. Transfer Size: " + formatBytes(contentSize/entryData.length)
					dataList += "Duration (Total): " + convertTime(loadTime)
					dataList += "- Avg. Duration: " + convertTime(loadTime/entryData.length)
					dataList += "- Avg. Latency: " + convertTime(avgLatency)
					dataList += "- Avg. Time Request Sent: " + convertTime(avgSend)
					dataList += "- Avg. Time Waiting: " + convertTime(avgWait)
					dataList += "- Avg. Time to Download: " + convertTime(avgReceive)
					dataList += "Number of Cookies: " + numOfCookies
					dataList += "--------"
					dataList += "Number of Content-Types:"
						for((k,v) <- contentTypeCount) dataList += ("- " + k + " (" + v + ")")
					dataList += "--------"
					dataList += "Number of Response Status-Types: "
						for((k,v) <- statusTypeCount) dataList += ("- " + k + " (" + v + ")")
					dataList += "--------"
					dataList += "Worst Offenders (by LoadTime): "
						for((k,v) <- entryTimeTen) dataList += "- Duration: " + convertTime(v) + "; Name: " + k
					dataList += "--------"
					dataList += "Worst Offenders (by Size): "
						for((k,v) <- entrySizeTen) dataList += "- Size: " + formatBytes(v) + "; Name: " + k
			val dataForDisplay = dataList.toList
	    Ok(views.html.index(dataForDisplay))
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
  def convertTime(args: Double) : String = {
		import java.text.SimpleDateFormat;
		return new SimpleDateFormat("s.SSS").format(args) + " ms"
  }
  def formatBytes(args: Double) : String = {
  	import java.text.DecimalFormat
    val units = List("b", "kb", "mb")
    val digitGroups = (Math.log10(args) / Math.log10(1024)).toInt
    return new DecimalFormat("#,##0.#").format(args / Math.pow(1024, digitGroups)) + " " + units(digitGroups)
	}
}



