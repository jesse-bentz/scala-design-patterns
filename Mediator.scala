/*
Allows loose coupling by encapsulating the way disparate sets of objects interact and communicate
with each other. Allows for the actions of each object set to vary independently of one another.
*/

class Mediator {
  private val players = collection.mutable.ListBuffer[Player]()

  def send(message: Message, from: Player) {
    players.filter(_ != from).foreach(_.receive(message))
  }

  def add(player: Player) = players += player
  def add(players: List[Player]) = this.players ++= players
  def remove(player: Player) = players -= player
}

case class Message(value: String)

trait Player {
  val mediator: Mediator
  def receive(message: Message)
  def send(message: Message) = mediator.send(message, this)
}

case class Player1(mediator: Mediator) extends Player {
  def receive(message: Message) {
    println("Thanks for the message: " + message.value)
  }
}

case class Player2(mediator: Mediator) extends Player {
  def receive(message: Message) {
    println("Ignoring your message: " + message.value)
  }
}

val mediator = new Mediator()
val player1 = Player1(mediator)
val player2 = Player2(mediator)
mediator.add(List(player1, player2))
player1.send(Message("test1"))
player2.send(Message("test2"))
