
data Account =
  Account {
    id :: String,
    name :: String,
    balance :: Double
  } deriving (Show, Eq, Read)

a1 = Account "a1" "Bob" 90

a2 = a { balance = (balance a) - 10 } 


