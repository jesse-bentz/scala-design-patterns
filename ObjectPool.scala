
abstract class ObjectPool[T] {

  val lifetimeSecs: Int
  val inUse = collection.mutable.Map[T, Long]()
  val available = collection.mutable.Map[T, Long]()

  def get: T = synchronized {
    val obj = clean.headOption.map { o => available -= o; o }.getOrElse(create)
    inUse(obj) = now
    obj
  }

  // clean out available and return list of available objects
  private def clean: List[T] = {
    available.filter { case (o, ts) => isDirty(o, ts) }.foreach { case (o, ts) =>
      println("Removing dirty item: " + o)
      available -= o
      expire(o)
    }
    available.keys.toList
  }

  def isDirty(o: T, ts: Long): Boolean = {
    ts < (now - lifetimeSecs) || !validate(o)
  }

  def checkIn(o: T) = synchronized {
    inUse -= o
    available(o) = now
  }

  def create: T
  def validate(o: T): Boolean
  def expire(o: T): Boolean

  def now: Long = System.currentTimeMillis / 1000
}

case class Resource(name: String, n: Int)

object TestPool extends ObjectPool[Resource] {
  val lifetimeSecs: Int = 1
  def create: Resource = Resource("test", scala.util.Random.nextInt(100))
  def validate(r: Resource): Boolean = r.n % 2 == 0
  def expire(r: Resource): Boolean = true
}

// test
for (i <- 1 to 10) {
  val r = TestPool.get
  println("Got resource: " + r)
  Thread.sleep(1000)
  if (i % 2 == 0) TestPool.checkIn(r)
}


