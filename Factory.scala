trait Animal
class Gecko extends Animal
class Jaguar extends Animal

object AnimalFactory {
  def apply(animal: String): Option[Animal] = animal match {
    case "gecko" => Some(new Gecko())
    case "jaguar" => Some(new Jaguar())
    case _ => None
  }
}

AnimalFactory("gecko")

