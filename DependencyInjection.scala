// dependency-injection through self typing (compile time)

trait Cache {
  def loadFromCache(key: String): Option[CacheItem]
  def saveToCache(key: String, item: CacheItem)
}

case class CacheItem(value: Int)

// require cache as depedency
trait Service { self: Cache =>

  def request(key: String): Int = {
    val result = loadFromCache(key).map(_.value) getOrElse get(key)
    saveToCache(key, CacheItem(result))
    result
  }

  // uncached response value
  def get(key: String): Int
}

trait MemoryCache extends Cache {
  val cache = collection.mutable.Map[String, CacheItem]()

  def loadFromCache(key: String): Option[CacheItem] = {
    val cached = cache.get(key)
    println(s"Got cached value $cached for key $key")
    cached
  }

  def saveToCache(key: String, item: CacheItem) { cache(key) = item }
}

object TestService extends Service with MemoryCache {
  def get(key: String): Int = scala.util.Random.nextInt(10)
}

// test
List("a", "b", "c", "a", "b").foreach { x =>
  val response = TestService.request(x)
  println(s"Got response $response for $x")
}
