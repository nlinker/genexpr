# What does the project for?

The project is evaluation of OOP surrogate of free monads. 
[Free monads](http://degoes.net/articles/modern-fp) is powerful tool to abstract side effecting operations and to make the business code easily testable and compose things easier.
Although they are good, to lean the approach requires pretty much effort and often discouraged in large teams with many junior developers.

## How to run

1. Install sbt (http://www.scala-sbt.org/0.13/docs/Setup.html)
2. Execute `sbt run`

## Example business task

Assume we have a KV-storage with simple transactions in it, so the operations on it are:
```scala
   put(k: K, v: V): Unit
   get(k: K): Option[V]
   tx[A](ops: () => A): A // where ops is the expression containing these operations
```

Example task, that can be solved with this API could be the following simple operation 
`transfer` with the underlying storage with the types `K=String` and `V=Int`:

```scala
def transfer(user1: String, user2: String, amount: Int): Try[Unit] = {
  val acc1opt = get(user1)
  val acc2opt = get(user2)
  // ... TODO

}
