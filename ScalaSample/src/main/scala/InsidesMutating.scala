import scala.collection.immutable

object InsidesMutating {

  class AccountX(var id: String, var name: String, var balance: Double) {
    override def toString(): String = s"Account: Name=${name} Balance=${balance}"
  }

  class AccountY(val id: String, val name: String, val balance: Double) {

    override def toString(): String = s"Account: Name=${name} Balance=${balance}"

    def WithBalance(_balance: Double) = new Account(id, name, _balance)

    def WithName(_name: String) = new Account(id, _name, balance)
  }

  case class Account(id: String, name: String, balance: Double)

  def main(args: Array[String]): Unit = {

    val accounts = immutable.HashMap.empty +
      ("a1" -> Account("a1", "Bob", 90.0)) +
      ("a2" -> Account("a2", "Joe", 70.0))

    println(accounts)
    //val accountsA = accounts.updated("a1", { accounts("a1").balance -= 10; accounts("a1"); })
    val accountsA = accounts.updated("a1", {
      val account = accounts("a1")
      account.copy(balance = account.balance - 10)
    })
    println(accounts)
    println(accountsA)

  }

}
