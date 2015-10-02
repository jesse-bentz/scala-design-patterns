/*
Allow you to compose objects into tree structures to represent part-whole hierarchies.
Composite lets clients treat individual objects and compositions of objects uniformly.
*/

// geographic component
trait Component {
  val name: String
  def getPopulation: Int
}

trait Composite[T <: Component] extends Component {
  val children = collection.mutable.ListBuffer[T]()
  def getPopulation: Int = children.map(_.getPopulation).sum
  def add(child: T) = children += child
  def remove(child: T) = children -= child
}

trait Leaf extends Component {
  val population: Int
  def getPopulation: Int = population
}

case class Country(name: String) extends Composite[State]
case class State(name: String) extends Composite[County]
case class County(name: String) extends Composite[City]
case class City(name: String, population: Int) extends Leaf

val cities = List(City("Portland", 610000), City("Beaverton", 93500))
val errorCity = City("Oops", 500)
val county = County("Multnomah")
val state = State("Oregon")
val country = Country("USA")

cities.foreach(county.add(_))
county.add(errorCity)
county.remove(errorCity)
state.add(county)
country.add(state)

println(s"Population: ${country.getPopulation}")
