package shttp

import java.net.URI
import java.net.http.HttpClient.Redirect
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

import scala.collection.mutable

// https://users.scala-lang.org/t/how-to-add-a-method-to-an-case-class-instance/4313/4
// https://stackoverflow.com/questions/27695325/scala-case-class-put-methods-in-companion-object

/**
  * @param followRedirects         - One of values of java.net.http.Redirect
  * @param connectTimeoutInSeconds - java.time.Duration.ofSeconds(connectTimeoutInSeconds)
  */
case class ShttpClient(followRedirects: String = "NEVER",
                       connectTimeoutInSeconds: Long = 3000) {

  private val postContentType = "application/x-www-form-urlencoded"
  private val jsonContentType = "application/json"
  private val client: HttpClient = buildClient()

  def buildClient(): HttpClient = {

    val client: HttpClient = HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofSeconds(connectTimeoutInSeconds))
      .followRedirects(Redirect.valueOf(followRedirects))
      .build()

    client
  }

  def HEAD(uri: String = "http://httpbin.org"): ShttpHeaders = {

    val request: HttpRequest = HttpRequest
      .newBuilder()
      .uri(URI.create(uri))
      .build()

    val httpResponse = client.send(request, BodyHandlers.ofString())

    def buildHeaders(): mutable.Map[String, String] = {
      val headers = scala.collection.mutable.Map[String, String]()

      httpResponse
        .headers()
        .map
        .forEach((k, v) => {
          var values = mutable.Buffer[String]()
          v.forEach(v1 => {
            values += v1
          })

          headers(k) = values.mkString(", ")
        })

      headers
    }

    ShttpHeaders(buildHeaders().toSeq)
  }

  def GET(uri: String = "http://httpbin.org/get",
          headers: Seq[(String, String)] = Seq()): HttpResponse[String] = {

    val request: HttpRequest = HttpRequest
      .newBuilder()
      .uri(URI.create(uri))
      .build()

    client.send(request, BodyHandlers.ofString())
  }

  def GETMulti(uris: Seq[String]): Seq[HttpResponse[String]] = {

    val httpResponses = for (uri <- uris) yield {

      val request: HttpRequest = HttpRequest
        .newBuilder()
        .uri(URI.create(uri))
        .build()

      client.send(request, BodyHandlers.ofString())
    }

    httpResponses
  }

  def POSTFormData(
    uri: String,
    data: Seq[(String, String)] = Seq()
  ): HttpResponse[String] = {

    val fields = (for ((k, v) <- data) yield k + "=" + v).mkString("&")
    val bodyPublisher = HttpRequest.BodyPublishers.ofString(fields)

    val request = HttpRequest
      .newBuilder()
      .POST(bodyPublisher)
      .uri(URI.create(uri))
      .header("Content-Type", postContentType)
      .build()

    import java.net.http.HttpResponse
    client.send(request, HttpResponse.BodyHandlers.ofString)
  }

  def POSTJson(uri: String, json: String): HttpResponse[String] = {

    val request = HttpRequest
      .newBuilder()
      .POST(HttpRequest.BodyPublishers.ofString(json))
      .uri(URI.create(uri))
      .header("Content-Type", jsonContentType)
      .build

    client.send(request, HttpResponse.BodyHandlers.ofString)
  }
}
