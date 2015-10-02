/*
- Visitor lets you define a new operation without changing the classes of the elements on which it operates.
- Allows for one or more operation to be applied to a set of objects at runtime, decoupling the operations
  from the object structure.
- double-dispatch
- operations on hetergenous collections
*/

// element (accepts visitor, that is, operation to be performed on element)
trait Visitable {
  def accept(v: Visitor)
}

trait Employee extends Visitable {
  val name: String
  val id = name
}

// concrete elements
case class HourlyEmployee(name: String, rate: Int, hours: Int) extends Employee {
  def accept(v: Visitor) = v.visit(this)
}

case class SalaryEmployee(name: String, salary: Int) extends Employee {
  def accept(v: Visitor) = v.visit(this)
}

case class CEO(name: String, salary: Int, bonus: Int) extends Employee {
  def accept(v: Visitor) = v.visit(this)
}

// visitor (operation to be performed on each element)
trait Visitor {
  def visit(e: HourlyEmployee)
  def visit(e: SalaryEmployee)
  def visit(e: CEO)
}

abstract class PayVisitor extends Visitor {
  // use internal mutable map to avoid return type on visit method
  protected val pay = collection.mutable.Map[String, Int]()
  def getPayData: Map[String, Int] = pay.toMap
}

class MonthlyPayVisitor extends PayVisitor {
  def visit(e: HourlyEmployee) { pay(e.id) = e.rate * e.hours }
  def visit(e: SalaryEmployee) { pay(e.id) = salarayPay(e.salary) }
  def visit(e: CEO) { pay(e.id) = salarayPay(e.salary) }
  private def salarayPay(salary: Int) = salary / 12
}

class BonusPayVisitor extends PayVisitor {
  def visit(e: HourlyEmployee) { pay(e.id) = 0 }
  def visit(e: SalaryEmployee) { pay(e.id) = 0 }
  def visit(e: CEO) { pay(e.id) = e.bonus }
}

val employees = List(
  HourlyEmployee("Jim", 50, 40 * 4),
  SalaryEmployee("Linda", 80000),
  CEO("Tommy", 150000, 10000)
  )

val monthlyPayVisitor = new MonthlyPayVisitor
employees.foreach(_.accept(monthlyPayVisitor))
println("Monthly Pay Report: " + monthlyPayVisitor.getPayData)

val bonusPayVisitor = new BonusPayVisitor
employees.foreach(_.accept(bonusPayVisitor))
println("Bonus Pay Report: " + bonusPayVisitor.getPayData)
