
abstract class Observer(val subject: Subject) {

  subject.attach(this)

  def update = {
    val ss = subject.getState
    println(s"$this observed subject state change: $ss")
  }
}

class Observer1(subject: Subject) extends Observer(subject)
class Observer2(subject: Subject) extends Observer(subject)

case class SubjectState(state: Int)

class Subject(var state: Int) {
  val observers = collection.mutable.ArrayBuffer[Observer]()

  def attach(observer: Observer) {
    println(s"Attaching observer $observer")
    observers += observer
  }

  def notifyObservers {
    observers.foreach(_.update)
  }

  def setState(state: Int) {
    this.state = state
    notifyObservers
  }

  def getState: SubjectState = new SubjectState(state)
}

val subject = new Subject(1)
val observers = List(new Observer1(subject), new Observer2(subject))
subject.setState(2)
