

reduce :: (a -> a -> a) -> [a] -> Maybe a
reduce reducer [x] = Just x
reduce reducer [] = Nothing 
reduce reducer xs =
  let xslen = length xs
      xsa = reduce reducer $ take (xslen `div` 2) xs
      xsb = reduce reducer $ drop (xslen `div` 2) xs 
  in
    case (xsa, xsb) of
      (Just a, Just b)  -> Just $ reducer a b   
      (Just a, Nothing) -> Just a 
      (Nothing, Just b) -> Just b 
      (_, _)            -> Nothing


