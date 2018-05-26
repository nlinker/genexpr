# Case Builder

## The problem

Case classes in Scala are the one of the best feature. They are concise, immutable by default, 
easy to use and extensible in the case we need to extend.

```scala
case class Name(
  first: String,
  middle: Option[String] = None,
  last: String,
)

case class User(
  name: Name,
  age: Int,
)

def foo(): Unit = {
  // I don't remember her father's name, sorry :-)
  val name = Name(last = "Селезнёва", first = "Алиса")
  val user = User(name, 15)
  println(s"Pilot is ${user}")
}
```

However, from the Java side to use them is as painful as ordinary Java classes with final 
fields and one big constructor:
```java
void foo() {
  Name name = new Name("Алиса", None, "Селезнёва");
  User user = new User(name, 15)
}
```

This looks not scary enough, but if we take some real-world example, like
```scala
case class Collection(
    private final Long exportDate;
    private final Integer collectionId;
    private final String name;
    private final String titleVersion;
    private final String searchTerms;
    private final Integer parentalAdvisoryId;
    private final String artistDisplayName;
    private final String viewUrl;
    private final String artworkUrl;
    private final LocalDate originalReleaseDate;
    private final LocalDate itunesReleaseDate;
    private final String labelStudio;
    private final String contentProviderName;
    private final String copyright;
    private final String pLine;
    private final Integer mediaTypeId;
    private final Boolean isCompilation;
    private final Integer collectionTypeId;
)
```
we immediately see, that such classes are hard to work in Java - the fields are often mixed up and get wrong values.
Unfortunately, Java has no named arguments and default parameters, unlike Scala has.
The common solution to the problem is to use Builder pattern, which solves the problem with the incorrect initialization:
```java
Collection collection = Collection.builder()
    .exportDate(Instant.now().toMillis())
    ...
    .isCompilation(false);
```
However, having solved one problem we got 2 others:
1. We need to generate these builders, it is very hard task to create and maintain them by hand.
2. We cannot guarantee in compile time, that all the fields are initialized.

