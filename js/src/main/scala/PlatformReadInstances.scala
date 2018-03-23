package sc0opt

import scala.scalajs.js
import scala.scalajs.js.annotation._

// js version

@js.native
@JSImport("os", JSImport.Namespace)
object OS extends js.Object {
  val EOL: String = js.native
}

class ParseException(s: String, errorOffset: Int) extends Exception(s)

private[sc0opt] object platform {
  val _NL = OS.EOL

  type ParseException = sc0opt.ParseException
  def mkParseEx(s: String, p: Int) = new ParseException(s, p)

  trait PlatformReadInstances {
    import java.net.URI
    implicit val uriRead: Read[URI] = Read.reads { new URI(_) }
  }

  def applyArgumentExHandler[C](desc: String, arg: String): PartialFunction[Throwable, Either[Seq[String], C]] = {
    case e: NumberFormatException => Left(Seq(desc + " expects a number but was given '" + arg + "'"))
    case e: ParseException        => Left(Seq(desc + " expects a Scala duration but was given '" + arg + "'"))
    case e: Throwable             => Left(Seq(desc + " failed when given '" + arg + "'. " + e.getMessage))
  }
}
