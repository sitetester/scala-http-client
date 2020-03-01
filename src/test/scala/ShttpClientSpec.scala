import org.scalatest.flatspec.AnyFlatSpec
import shttp.ShttpClient

class ShttpClientSpec extends AnyFlatSpec {

  "SHttpClient.HEAD()" should "return only headers" in {

    val shttpHeaders = ShttpClient().HEAD()

    assert(shttpHeaders.headers.nonEmpty)
    assert(
      shttpHeaders.getHeader("access-control-allow-credentials")._2.toBoolean
    )
    assert(!shttpHeaders.headerExists("access-control-allow-credentials123"))
  }

  "SHttpClient.GET()" should "return HTTResponse" in {

    val httpResponse = ShttpClient().GET()
    assert(httpResponse.body().length > 0)
  }

  "SHttpClient.GETMulti()" should "return multiple HTTResponseS" in {
    val httpResponses =
      ShttpClient().GETMulti(
        Seq("http://google.com", "https://httpbin.org/get")
      )

    assert(httpResponses.size > 1)
  }

  "SHttpClient.POSTFormData()" should "POST form fields & return HTTResponseS" in {
    var data: Seq[(String, String)] = Seq()

    data = data :+ ("username", "admin")
    data = data :+ ("password", "demo")
    data = data :+ ("custom", "secret")

    val httpResponse =
      ShttpClient().POSTFormData("http://httpbin.org/post", data)

    assert(httpResponse.statusCode() === 200)
  }

  "SHttpClient.POSTJson()" should "return POST json and return HTTResponse" in {

    val json =
      """{"jsonrpc":"2.0","method":"eth_getBlockByNumber","params": ["0x5BAD55",false],"id":1}"""

    val httpResponse = ShttpClient().POSTJson(
      "https://mainnet.infura.io/v3/c8d36b72d2d04f16a94931809cdf6383",
      json
    )

    assert(httpResponse.statusCode() === 200)
  }
}
