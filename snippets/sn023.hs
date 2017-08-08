

(|>) a f = f a

items = [0..100] |> map (\x -> x * x)
                 |> filter (\x -> x `mod` 3 == 0)
                 |> take 10
                 |> map show
