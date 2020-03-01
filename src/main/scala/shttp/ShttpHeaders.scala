package shttp

case class ShttpHeaders(headers: Seq[(String, String)] = Seq()) {

  def getHeader(header: String): (String, String) = {
    headers.filter(_._1 == header).head
  }

  def headerExists(header: String): Boolean = {
    headers.count(_._1 == header) > 0
  }
}
