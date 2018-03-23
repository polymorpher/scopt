import org.specs2._
import java.util.{Calendar, GregorianCalendar}
import java.io.{ByteArrayOutputStream, File}
import java.net.{ URI, InetAddress }
import scala.concurrent.duration.Duration

class ImmutableParserSpecJVM extends Specification { def is = args(sequential = true) ^ s2"""
  This is a specification to check the immutable parser

  opt[Calendar]("foo") action { x => x } should
    parse 2000-01-01 out of --foo 2000-01-01                    ${calendarParser("--foo", "2000-01-01")}
    parse 2000-01-01 out of --foo=2000-01-01                    ${calendarParser("--foo=2000-01-01")}
    fail to parse --foo bar                                     ${calendarParserFail("--foo", "bar")}
    fail to parse --foo=bar                                     ${calendarParserFail("--foo=bar")}

  opt[File]("foo") action { x => x } should
    parse test.txt out of --foo test.txt                        ${fileParser("--foo", "test.txt")}
    parse test.txt out of --foo=test.txt                        ${fileParser("--foo=test.txt")}

  opt[InetAddress]("foo") action { x => x } should
    parse 8.8.8.8 out of --foo 8.8.8.8                          ${inetAddressParser("--foo", "8.8.8.8")}
    parse 8.8.8.8 out of --foo=8.8.8.8                          ${inetAddressParser("--foo=8.8.8.8")}
  """

  import SpecUtil._

  val calendarParser1 = new sc0opt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[Calendar]("foo").action( (x, c) => c.copy(calendarValue = x) )
    help("help")
  }
  def calendarParser(args: String*) = {
    val result = calendarParser1.parse(args.toSeq, Config())
    result.get.calendarValue.getTime === new GregorianCalendar(2000, Calendar.JANUARY, 1).getTime
  }
  def calendarParserFail(args: String*) = {
    val result = calendarParser1.parse(args.toSeq, Config())
    result === None
  }

  val fileParser1 = new sc0opt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[File]("foo").action( (x, c) => c.copy(fileValue = x) )
    help("help")
  }
  def fileParser(args: String*) = {
    val result = fileParser1.parse(args.toSeq, Config())
    result.get.fileValue === new File("test.txt")
  }

  val inetAddressParser1 = new sc0opt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[InetAddress]("foo").action( (x, c) => c.copy(inetAddressValue = x) )
    help("help")
  }
  def inetAddressParser(args: String*) = {
    val result = inetAddressParser1.parse(args.toSeq, Config())
    result.get.inetAddressValue === InetAddress.getByName("8.8.8.8")
  }

  case class Config(flag: Boolean = false, intValue: Int = 0, longValue: Long = 0L, stringValue: String = "",
    doubleValue: Double = 0.0, boolValue: Boolean = false, debug: Boolean = false,
    bigDecimalValue: BigDecimal = BigDecimal("0.0"),
    calendarValue: Calendar = new GregorianCalendar(1900, Calendar.JANUARY, 1),
    fileValue: File = new File("."),
    uriValue: URI = new URI("http://localhost"),
    inetAddressValue: InetAddress = InetAddress.getByName("0.0.0.0"),
    durationValue: Duration = Duration("0s"),
    key: String = "", a: String = "", b: String = "",
    seqInts: Seq[Int] = Seq(),
    mapStringToBool: Map[String,Boolean] = Map(),
    seqTupleStringString: Seq[(String, String)] = Nil, charValue: Char = 0)
}
