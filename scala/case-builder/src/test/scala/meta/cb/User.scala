package meta.cb

@buildable case class User
(
  name: Name,
  age: Int
)

@buildable case class Name
(
  first: String = "Ivan",
// TODO  middle: Option[String] = None,
  last: String = "Ivanov"
)
