package pro.sgaz.scalingua

import org.slf4j.Logger
import ru.makkarpov.scalingua.LanguageId

import java.io._
import java.nio.charset.StandardCharsets

object GenerationContext {
  val HashMarker = "## Hash: ## "
  val ScalaHashPrefix = s"// $HashMarker"
}

case class GenerationContext(pkg: String, implicitCtx: Option[String], lang: LanguageId, hasTags: Boolean,
                             src: File, target: File, log: Logger)
{
  val srcHash: String = src.hashString

  def mergeContext(ctx: Option[String]): Option[String] = (implicitCtx, ctx) match {
    case (None,    None)    => None
    case (Some(x), None)    => Some(x)
    case (None,    Some(y)) => Some(y)
    case (Some(x), Some(y)) => Some(x + ":" + y)
  }

  def filePrefix = "/" + pkg.replace('.', '/') + (if (pkg.nonEmpty) "/" else "")

  def checkBinaryHash: Boolean = target.exists() && {
    val storedHash = {
      val is = new DataInputStream(new FileInputStream(target))
      try is.readUTF()
      catch {
        case t: Throwable =>
          t.printStackTrace()
          ""
      } finally is.close()
    }

    srcHash == storedHash
  }

  def checkTextHash: Boolean = target.exists() && {
    import GenerationContext.HashMarker

    val storedHash = {
      val rd = new BufferedReader(new InputStreamReader(new FileInputStream(target), StandardCharsets.UTF_8))
      try {
        val l = rd.readLine()
        if ((l ne null) && l.contains(HashMarker)) {
          val idx = l.indexOf(HashMarker)
          l.substring(idx + HashMarker.length)
        } else ""
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          ""
      } finally rd.close()
    }

    srcHash == storedHash
  }
}
