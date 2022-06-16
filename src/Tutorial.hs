{-# LANGUAGE DataKinds #-}

module Tutorial where 

import Keelung 

identity :: Comp GF181 (Expr 'Num GF181)
identity = do 
    x <- inputVar 
    return $ Var x 
