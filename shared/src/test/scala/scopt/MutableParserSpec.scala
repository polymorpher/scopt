import org.specs2._
import java.io.ByteArrayOutputStream

class MutableParserSpec extends Specification { def is = args(sequential = true) ^ s2"""
  This is a specification to check the mutable parser

  opt[Unit]('f', "foo") foreach { x => x } should
    parse () out of --foo                                       ${unitParser("--foo")}
    parse () out of -f                                          ${unitParser("-f")}

  opt[Int]('f', "foo") foreach { x => x } should
    parse 1 out of --foo 1                                      ${intParser("--foo", "1")}
    parse 1 out of --foo:1                                      ${intParser("--foo:1")}
    parse 1 out of --foo=1                                      ${intParser("--foo=1")}
    parse 1 out of -f 1                                         ${intParser("-f", "1")}
    parse 1 out of -f:1                                         ${intParser("-f:1")}
    parse 1 out of --foo 0x01                                   ${intParser("--foo","0x01")}
    parse 1 out of --foo:0x01                                   ${intParser("--foo:0x01")}
    parse 1 out of --foo=0x01                                   ${intParser("--foo=0x01")}
    parse 1 out of -f 0x1                                       ${intParser("-f", "0x1")}
    parse 1 out of -f:0x1                                       ${intParser("-f:0x1")}
    fail to parse --foo                                         ${intParserFail{"--foo"}}
    fail to parse --foo bar                                     ${intParserFail("--foo", "bar")}
    fail to parse --foo=bar                                     ${intParserFail("--foo=bar")}

  opt[Long]('f', "foo") foreach { x => x } should
    parse 1 out of --foo 1                                      ${longParser("--foo", "1")}
    parse 1 out of --foo:1                                      ${longParser("--foo:1")}
    parse 1 out of --foo=1                                      ${longParser("--foo=1")}
    parse 1 out of -f 1                                         ${longParser("-f", "1")}
    parse 1 out of -f:1                                         ${longParser("-f:1")}
    parse 1 out of --foo 0x01                                   ${longParser("--foo","0x01")}
    parse 1 out of --foo:0x01                                   ${longParser("--foo:0x01")}
    parse 1 out of --foo=0x01                                   ${longParser("--foo=0x01")}
    parse 1 out of -f 0x1                                       ${longParser("-f", "0x1")}
    parse 1 out of -f:0x1                                       ${longParser("-f:0x1")}

  opt[String]("foo") foreach { x => x } should
    parse "bar" out of --foo bar                                ${stringParser("--foo", "bar")}
    parse "bar" out of --foo:bar                                ${stringParser("--foo:bar")}
    parse "bar" out of --foo=bar                                ${stringParser("--foo=bar")}

  opt[Double]("foo") foreach { x => x } should
    parse 1.0 out of --foo 1.0                                  ${doubleParser("--foo", "1.0")}
    parse 1.0 out of --foo:1.0                                  ${doubleParser("--foo:1.0")}
    parse 1.0 out of --foo=1.0                                  ${doubleParser("--foo=1.0")}
    fail to parse --foo bar                                     ${doubleParserFail("--foo", "bar")}
    fail to parse --foo=bar                                     ${doubleParserFail("--foo=bar")}

  opt[Boolean]("foo") foreach { x => x } should
    parse true out of --foo true                                ${trueParser("--foo", "true")}
    parse true out of --foo:true                                ${trueParser("--foo:true")}
    parse true out of --foo=true                                ${trueParser("--foo=true")}
    parse true out of --foo 1                                   ${trueParser("--foo", "1")}
    parse true out of --foo:1                                   ${trueParser("--foo:1")}
    parse true out of --foo=1                                   ${trueParser("--foo=1")}
    fail to parse --foo bar                                     ${boolParserFail("--foo", "bar")}
    fail to parse --foo=bar                                     ${boolParserFail("--foo=bar")}

  opt[(String, Int)]("foo") foreach { x => x } should
    parse ("k", 1) out of --foo k=1                             ${pairParser("--foo", "k=1")}
    parse ("k", 1) out of --foo:k=1                             ${pairParser("--foo:k=1")}
    parse ("k", 1) out of --foo=k=1                             ${pairParser("--foo=k=1")}
    fail to parse --foo                                         ${pairParserFail("--foo")}
    fail to parse --foo bar                                     ${pairParserFail("--foo", "bar")}
    fail to parse --foo k=bar                                   ${pairParserFail("--foo", "k=bar")}
    fail to parse --foo=k=bar                                   ${pairParserFail("--foo=k=bar")}

  opt[String]("foo") required() foreach { x => x } should
    fail to parse Nil                                           ${requiredFail()}

  opt[String]("foo").required().withFallback(() => "someFallback") should
    parse provided value                                        ${requiredWithFallback(args = Seq("--stringValue", "provided"), expected = "provided")}
    use fallback value                                          ${requiredWithFallback(args = Nil, expected = "someFallback")}

  opt[Unit]("debug") hidden() foreach { x => x } should
    parse () out of --debug                                     ${unitParserHidden("--debug")}

  unknown options should
    fail to parse by default                                    ${intParserFail("-z", "bar")}

  opt[(String, Int)]("foo") foreach { x => x } validate { x =>
    if (x > 0) success else failure("Option --foo must be >0") } should
    fail to parse --foo 0                                       ${validFail("--foo", "0")}

  arg[Int]("<port>") foreach { x => x } should
    parse 80 out of 80                                          ${intArg("80")}
    be required and should fail to parse Nil                    ${intArgFail()}

  arg[String]("<a>"); arg[String]("<b>") foreach { x => x } should
    parse "b" out of a b                                        ${multipleArgs("a", "b")}

  arg[String]("<a>") foreach { x => x} unbounded() optional(); arg[String]("<b>") optional() should
    parse "b" out of a b                                        ${unboundedArgs("a", "b")}
    parse nothing out of Nil                                    ${emptyArgs()}

  cmd("update") foreach { x => x } should
    parse () out of update                                      ${cmdParser("update")}

  help("help") if OneColumn should
    print usage text --help                                     ${helpParserOneColumn()}

  help("help") if TwoColumn should
    print usage text --help                                     ${helpParserTwoColumns()}

  reportError("foo") should
    print "Error: foo\n"                                        ${reportErrorParser("foo")}

  reportWarning("foo") should
    print "Warning: foo\n"                                      ${reportWarningParser("foo")}

  showHeader should
    print "scopt 3.x\n"                                         ${showHeaderParser()}

  showUsage should
    print usage text                                            ${showUsageParser()}
                                                                """

  import SpecUtil._

  def unitParser(args: String*) = {
    var foo = false
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Unit]('f', "foo").foreach( _ => foo = true )
      help("help")
    }
    val result = parser.parse(args.toSeq)
    (result === true) and (foo === true)
  }

  def unitParserHidden(args: String*) = {
    var debug = false
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Unit]("debug").hidden().foreach( _ => debug = true )
      help("help")
    }
    val result = parser.parse(args.toSeq)
    debug === true
  }

  def intParser(args: String*) = {
    var foo = 0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Int]('f', "foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq)
    foo === 1
  }

  def longParser(args: String*) = {
    var foo = 0L
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Long]('f', "foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq)
    foo === 1L
  }

  def intParserFail(args: String*) = {
    var foo = 0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Int]('f', "foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq) === false
  }

  def stringParser(args: String*) = {
    var foo = ""
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[String]("foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq)
    foo === "bar"
  }

  def doubleParser(args: String*) = {
    var foo = 0.0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Double]("foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq)
    foo === 1.0
  }

  def doubleParserFail(args: String*) = {
    var foo = 0.0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Double]("foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq) === false
  }

  def trueParser(args: String*) = {
    var foo = false
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Boolean]("foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq)
    foo === true
  }

  def boolParserFail(args: String*) = {
    var foo = false
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Boolean]("foo").foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq) === false
  }

  def pairParser(args: String*) = {
    var foo = ""
    var value = 0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[(String, Int)]("foo").foreach({ case (k, v) =>
        foo = k
        value = v
      })
      help("help")
    }
    parser.parse(args.toSeq)
    (foo === "k") and (value === 1)
  }

  def pairParserFail(args: String*) = {
    var foo = ""
    var value = 0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[(String, Int)]("foo").foreach({ case (k, v) =>
        foo = k
        value = v
      })
      help("help")
    }
    parser.parse(args.toSeq) === false
  }

  def requiredFail(args: String*) = {
    var foo = ""
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[String]("foo").required().foreach( x => foo = x )
      help("help")
    }
    parser.parse(args.toSeq) === false
  }

  def requiredWithFallback(args: Seq[String], expected: String) = {
    var stringValue = ""
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[String]("stringValue").required().withFallback(() => "someFallback")
        .foreach( x => stringValue = x )
      help("help")
    }
    parser.parse(args) === true
    stringValue === expected
  }

  def validFail(args: String*) = {
    var foo = 0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Int]('f', "foo").foreach( x => foo = x ).
        validate( x =>
          if (x > 0) success
          else failure("Option --foo must be >0") ).
        validate( x => failure("Just because") )
      help("help")
    }
    parser.parse(args.toSeq) === false
  }

  def intArg(args: String*) = {
    var port = 0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[Int]("<port>").foreach( x => port = x )
      help("help")
    }
    parser.parse(args.toSeq)
    port === 80
  }

  def intArgFail(args: String*) = {
    var port = 0
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[Int]("<port>").foreach( x => port = x )
      help("help")
    }
    parser.parse(args.toSeq) === false
  }

  def multipleArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[String]("<a>").foreach( x => a = x )
      arg[String]("<b>").foreach( x => b = x )
      help("help")
    }
    parser.parse(args.toSeq)
    (a === "a") and (b === "b")
  }

  def unboundedArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[String]("<a>").foreach( x => a = x ).unbounded()
      arg[String]("<b>").foreach( x => b = x )
      help("help")
    }
    parser.parse(args.toSeq)
    (a === "b") and (b === "")
  }

  def emptyArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[String]("<a>").foreach( x => a = x ).unbounded().optional()
      arg[String]("<b>").foreach( x => b = x ).optional()
      help("help")
    }
    parser.parse(args.toSeq) === true
  }

  def cmdParser(args: String*) = {
    var foo = false
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      cmd("update").foreach( _ => foo = true )
      help("help")
    }
    val result = parser.parse(args.toSeq)
    (result === true) and (foo === true)
  }

  def helpParserOneColumn(args: String*) = {
    case class Config(foo: Int = -1, xyz: Boolean = false,
      libName: String = "", maxCount: Int = -1, verbose: Boolean = false, debug: Boolean = false,
      mode: String = "", keepalive: Boolean = false)
    var c = Config()
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      override def renderingMode = sc0opt.RenderingMode.OneColumn
      head("scopt", "3.x")

      opt[Int]('f', "foo").foreach( x => c = c.copy(foo = x) ).
        text("foo is an integer property")

      opt[(String, Int)]("max").foreach( { case (k, v) =>
        c = c.copy(libName = k, maxCount = v) }).
        validate( x =>
          if (x._2 > 0) success
          else failure("Value <max> must be >0") ).
        keyValueName("<libname>", "<max>").
        text("maximum count for <libname>")

      opt[Unit]("verbose").foreach( _ => c = c.copy(verbose = true) ).
        text("verbose is a flag")

      opt[Unit]("debug").hidden().foreach( _ => c = c.copy(debug = true) ).
        text("this option is hidden in the usage text")

      help("help").text("prints this usage text")

      note("some notes.".newline)

      cmd("update").foreach( _ => c.copy(mode = "update") ).
        text("update is a command.").
        children(
          opt[Unit]("not-keepalive").abbr("nk").
            foreach( _ => c.copy(keepalive = false) ).text("disable keepalive"),
          opt[Boolean]("xyz").foreach( x =>
            c = c.copy(xyz = x) ).text("xyz is a boolean property"),
          opt[Unit]("debug-update").hidden().
            foreach( _ => c = c.copy(debug = true) ).
            text("this option is hidden in the usage text")
        )
    }
    parser.parse(args.toSeq)
    parser.usage === """scopt 3.x
Usage: scopt [update] [options]

  -f <value> | --foo <value>
        foo is an integer property
  --max:<libname>=<max>
        maximum count for <libname>
  --verbose
        verbose is a flag
  --help
        prints this usage text
some notes.

Command: update [options]
update is a command.
  -nk | --not-keepalive
        disable keepalive
  --xyz <value>
        xyz is a boolean property"""
  }

  def helpParserTwoColumns(args: String*) = {
    case class Config(foo: Int = -1, xyz: Boolean = false,
      libName: String = "", maxCount: Int = -1, verbose: Boolean = false, debug: Boolean = false,
      mode: String = "", keepalive: Boolean = false)
    var c = Config()
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")

      opt[Int]('f', "foo").foreach( x => c = c.copy(foo = x) ).
        text("foo is an integer property")

      opt[(String, Int)]("max").foreach( { case (k, v) =>
        c = c.copy(libName = k, maxCount = v) }).
        validate( x =>
          if (x._2 > 0) success
          else failure("Value <max> must be >0") ).
        keyValueName("<libname>", "<max>").
        text("maximum count for <libname>")

      opt[Unit]("verbose").foreach( _ => c = c.copy(verbose = true) ).
        text("verbose is a flag")

      opt[Unit]("debug").hidden().foreach( _ => c = c.copy(debug = true) ).
        text("this option is hidden in the usage text")

      help("help").text("prints this usage text")

      note("some notes.".newline)

      cmd("update").foreach( _ => c.copy(mode = "update") ).
        text("update is a command.").
        children(
          opt[Unit]("not-keepalive").abbr("nk").
            foreach( _ => c.copy(keepalive = false) ).text("disable keepalive"),
          opt[Boolean]("xyz").foreach( x =>
            c = c.copy(xyz = x) ).text("xyz is a boolean property"),
          opt[Unit]("debug-update").hidden().
            foreach( _ => c = c.copy(debug = true) ).
            text("this option is hidden in the usage text")
        )
    }
    parser.parse(args.toSeq)
    parser.usage === """scopt 3.x
Usage: scopt [update] [options]

  -f, --foo <value>        foo is an integer property
  --max:<libname>=<max>    maximum count for <libname>
  --verbose                verbose is a flag
  --help                   prints this usage text
some notes.

Command: update [options]
update is a command.
  -nk, --not-keepalive     disable keepalive
  --xyz <value>            xyz is a boolean property"""
  }

  def printParserError(body: sc0opt.OptionParser[Unit] => Unit): String = {
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      help("help") text("prints this usage text")
    }
    val bos = new ByteArrayOutputStream()
    Console.withErr(bos) { body(parser) }
    bos.toString("UTF-8")
  }
  def printParserOut(body: sc0opt.OptionParser[Unit] => Unit): String = {
    val parser = new sc0opt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      help("help") text("prints this usage text")
    }
    val bos = new ByteArrayOutputStream()
    Console.withOut(bos) { body(parser) }
    bos.toString("UTF-8")
  }
  def reportErrorParser(msg: String) = {
    printParserError(_.reportError(msg)) === "Error: foo".newline
  }
  def reportWarningParser(msg: String) = {
    printParserError(_.reportWarning(msg)) === "Warning: foo".newline
  }
  def showHeaderParser() = {
    printParserOut(_.showHeader) === "scopt 3.x".newline
  }
  def showUsageParser() = {
    printParserOut(_.showUsage) === """scopt 3.x
Usage: scopt [options]

  --help  prints this usage text
"""
  }
}
