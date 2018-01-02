{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Compile where

import Control.Exception
import Control.Monad
import Data.Typeable
import Language.Haskell.TH
import System.Directory
import System.FilePath
import System.IO
import Unsafe.Coerce

import GHC
import GHC.Paths (libdir)
import DynFlags  (defaultFatalMessager,
                  defaultFlushOut)

compile :: forall a . Typeable a => TExp a -> IO a
compile texp =
  bracket (open texp) cleanup $ \tfile -> do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        void $ setSessionDynFlags dflags
        target <- guessTarget tfile Nothing
        setTargets [target]
        r <- load LoadAllTargets
        case r of
          Failed    -> error "Compilation failed"
          Succeeded -> do
            setContext [IIDecl $ simpleImportDecl (mkModuleName "TempMod")]
            result <- compileExpr ("TempMod.myFunc")
            let result' = unsafeCoerce result :: a
            return result'
  where
    open :: TExp a -> IO FilePath
    open e = do
        (tfile, h) <- openTempFile "." "TempMod.hs"
        hPutStr h (unlines
                   [ "module TempMod where"
                   , "import GHC.Classes"
                   , "import GHC.Num"
                   , "import GHC.Base"
                   , ""
                   , "myFunc :: " ++ show (typeOf (undefined :: a))
                   , "myFunc = " ++ pprint (unType e)] )
        hFlush h
        hClose h
        return tfile

    cleanup :: FilePath -> IO ()
    cleanup path = do
        removeFile path
        removeFile (addExtension root ".hi")
        removeFile (addExtension root ".o")
      where
        root :: FilePath
        (root, _) = splitExtension path
