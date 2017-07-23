import Debug.Trace

canI    = trace "You can." True
shouldI = trace "Probably not." False

_and_ e1 e2 = if e1 then e2 else False 

main = do
  putStrLn $ show (shouldI `_and_` canI)

