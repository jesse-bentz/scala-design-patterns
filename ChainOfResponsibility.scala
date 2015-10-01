case class Command(name: String)

object Response {
  def success(command: Command) = Response("Ok! " + command.name)
  def unhandled = Response("Unhandled")
}

case class Response(value: String)

trait Handler {
  val handle: PartialFunction[Command, Response]
}

object HandlerA extends Handler {
  val handle: PartialFunction[Command, Response] = {
    case c: Command if c.name == "a" => Response.success(c)
  }
}

object HandlerB extends Handler {
  val handle: PartialFunction[Command, Response] = {
    case c: Command if c.name == "b" => Response.success(c)
  }
}

object DefaultHandler extends Handler {
  val handle: PartialFunction[Command, Response] = {
    case c: Command => Response.unhandled
  }
}

object Controller {
  def receive(command: Command): Response = {
    val handlers = {
      HandlerA.handle orElse
      HandlerB.handle orElse
      DefaultHandler.handle
    }
    handlers(command)
  }
}

// test
List("a", "b", "c").foreach { cmd =>
  val command = Command(cmd)
  val response = Controller.receive(command)
  println(s"Got response $response for command $command")
}

