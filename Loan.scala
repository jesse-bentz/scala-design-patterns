// properly dispose of resource when it goes out of scope

trait Resource {
  def dispose: Unit
}

def withResource[T, R <: Resource](r: R)(f: R => T): T = try f(r) finally r.dispose

case class TestResource(name: String) extends Resource {
  def dispose: Unit = {
    println(s"Disposing of resource: $name")
  }
}

val resource = TestResource("foo")
withResource(resource) { r =>
  println("Using resource: " + r)
}

