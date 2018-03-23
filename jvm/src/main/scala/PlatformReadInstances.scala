package sc0opt

import java.net.UnknownHostException

private[sc0opt] object platform {
  val _NL = System.getProperty("line.separator")

  import java.util.{Locale, Calendar, GregorianCalendar}
  import java.text.SimpleDateFormat
  import java.io.File
  import java.net.{InetAddress, URI}

  type ParseException = java.text.ParseException
  def mkParseEx(s: String, p: Int) = new java.text.ParseException(s, p)

  trait PlatformReadInstances {
    def calendarRead(pattern: String): Read[Calendar] = calendarRead(pattern, Locale.getDefault)
    def calendarRead(pattern: String, locale: Locale): Read[Calendar] =
      Read.reads { s =>
        val fmt = new SimpleDateFormat(pattern)
        val c = new GregorianCalendar
        c.setTime(fmt.parse(s))
        c
      }

    implicit val yyyymmdddRead: Read[Calendar] = calendarRead("yyyy-MM-dd")
    implicit val fileRead: Read[File]           = Read.reads { new File(_) }
    implicit val inetAddress: Read[InetAddress] = Read.reads { InetAddress.getByName(_) }
    implicit val uriRead: Read[URI]             = Read.reads { new URI(_) }
  }

  def applyArgumentExHandler[C](desc: String, arg: String): PartialFunction[Throwable, Either[Seq[String], C]] = {
      case e: NumberFormatException => Left(Seq(desc + " expects a number but was given '" + arg + "'"))
      case e: UnknownHostException  => Left(Seq(desc + " expects a host name or an IP address but was given '" + arg + "' which is invalid"))
      case e: ParseException        => Left(Seq(desc + " expects a Scala duration but was given '" + arg + "'"))
      case e: Throwable             => Left(Seq(desc + " failed when given '" + arg + "'. " + e.getMessage))
    }


}
