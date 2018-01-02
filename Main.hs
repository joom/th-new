{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Compile

main :: IO ()
main = do
  notTExp :: (TExp (Bool -> Bool)) <- runQ [|| not ||]
  not' <- compile notTExp
  print $ not' True
