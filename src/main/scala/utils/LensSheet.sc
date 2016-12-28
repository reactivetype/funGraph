import utils.Lens._
import utils.Lens

case class Person(name: String, age: Int)
case class Household(husband: Person, wife: Person)

val nameLens: Lens[Person, String] = focus(
  get = (p: Person) => p.name,
  update = (f: String => String) => (p: Person) => p.copy(
    name = f(p.name)
  )
)

val wifeLens: Lens[Household, Person] = focus(
  get = (h: Household) => h.wife,
  update = (f: Person => Person) => (h: Household) => h.copy(
    wife = f(h.wife)
  )
)

val wifeNameLens: Lens[Household, String] =
  wifeLens ~> nameLens

val p1 = Person("Rich", 23)
nameLens.get(p1)

val p2 = Person("Poor", 20)


val h = Household(p1,p2)

val wifeName = wifeNameLens.get(h)

2 + 2


