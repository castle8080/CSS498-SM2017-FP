module LList (
    LList(LLNil,LLCons),
    ll_head,
    ll_tail,
    ll_length,
    ll_update,
    ll_append,
    ll_create
) where

data LList a =
    LLNil |
    LLCons a (LList a)
    deriving (Eq, Ord, Show, Read)

ll_head (LLCons x _) = x

ll_tail (LLCons _ xs) = xs

ll_length LLNil = 0
ll_length (LLCons _ xs) =
    1 + (ll_length xs)

ll_update (LLCons _ xs) 0 x =
    LLCons x $ xs

ll_update (LLCons v xs) n x =
    LLCons v $ ll_update xs (n - 1) x
  
ll_append LLNil x =
    LLCons x $ LLNil

ll_append (LLCons v xs) x =
    LLCons v $ ll_append xs x

ll_create [] = LLNil
ll_create (x:xs) = LLCons x $ ll_create xs