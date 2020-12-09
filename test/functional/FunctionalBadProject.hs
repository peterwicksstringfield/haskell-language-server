{-# LANGUAGE OverloadedStrings #-}

module FunctionalBadProject (tests) where

import           Control.Monad.IO.Class
import           Language.Haskell.LSP.Test hiding (message)
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (ignoreTestBecause)

tests :: TestTree
tests = testGroup "behaviour on malformed projects" [
    ignoreTestBecause "conflicting hie.yaml files" $
      testCase "deals with cabal file with unsatisfiable dependency" $
        runSession hlsCommandExamplePlugin codeActionSupportCaps "test/testdata/badProjects/cabal" $ do
            doc <- openDoc "Foo.hs" "haskell"
            diags <- waitForDiagnosticsFromSource doc "cradle"
            liftIO $ expectDiagnostic diags ["Could not resolve dependencies"]
    ]
