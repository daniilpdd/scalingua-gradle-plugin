package pro.sgaz.scalingua

case class ParseFailedException(message: String, cause: Throwable) extends Exception(message, cause)
