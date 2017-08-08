

class BankAccountX(private var _balance: Double, val holder: String) {

    def balance = _balance;

    def deposit(amount: Double) = {
        if (_balance + amount < 0)
            throw new Exception("You don't have enough money!")
        _balance = _balance + amount
    }

    def withdraw(amount: Double) =
      deposit(-amount)

    override def toString() =
      s"Account for $holder has a balance of $balance."
}

class BankAccount(val balance: Double, val holder: String) {
    def deposit(amount: Double) = {
        if (balance + amount < 0)
            throw new Exception("You don't have enough money!")
        new BankAccount(balance + amount, holder)
    }

    def withdraw(amount: Double) =
        deposit(-amount)

    override def toString() =
        s"Account for $holder has a balance of $balance."
}

