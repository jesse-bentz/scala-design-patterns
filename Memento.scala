import scala.collection.mutable.MutableList

case class Memento(x: String)

class Originator(var x: String) {

  def set(x: String) = {
    println("Setting state: " + x)
    this.x = x
  }

  def toMemento: Memento = {
    println("Saving state: " + x)
    Memento(x)
  }

  def restore(m: Memento) {
    println("Restoring to state: " + m.x)
    this.x = m.x
  }
}

object Caretaker {
  val states = MutableList[Memento]()

  def addMemento(m: Memento) = states += m

  def getMemento(i: Int): Option[Memento] = {
    if (states.isDefinedAt(i)) Some(states(i)) else None
  }

}

val originator = new Originator("state1")
Caretaker.addMemento(originator.toMemento)
originator.set("state2")
originator.set("state3")
Caretaker.addMemento(originator.toMemento)
Caretaker.getMemento(0).map(originator.restore(_))


