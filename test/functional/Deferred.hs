{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Deferred(tests) where

import Control.Monad.IO.Class
import Control.Lens ((^?), (^.), _Just, _Right)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens hiding (id, message)
import Test.Hls.Util
import Test.Tasty
--import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "deferred responses" [

    testCase "do not affect hover requests" $ runSession hlsCommand fullCaps "test/testdata" $ do
      doc <- openDoc "FuncTest.hs" "haskell"

      let expectHoverText hover =
              liftIO $ length (hover ^? _Just . contents) > 0
                       @? "Found some hover text"

      getHover doc (Position 4 2) >>= expectHoverText

      _ <- getDocumentSymbols doc

      getHover doc (Position 4 2) >>= expectHoverText

        -- Now that we have cache the following request should be instant
      locations <- getHighlights doc (Position 7 0)
      liftIO $ locations @?= [
                    DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 5, _character = 6}
                       , _end   = Position {_line = 5, _character = 8}
                       }
                     , _kind  = Just HkRead
                     }
                    , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   ]

    , testCase "instantly respond to failed modules with no cache" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "FuncTestFail.hs" "haskell"
        defs <- getDefinitions doc (Position 1 11)
        liftIO $ defs @?= []

    , testCase "respond to untypecheckable modules with parsed module cache" $
      runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "FuncTestError.hs" "haskell"
        Left (sym:_) <- getDocumentSymbols doc
        liftIO $ sym ^. name @?= "Main"

    -- TODO does not compile
    -- , testCase "returns hints as diagnostics" $ runSession hlsCommand fullCaps "test/testdata" $ do
    --     _ <- openDoc "FuncTest.hs" "haskell"

    --     cwd <- liftIO getCurrentDirectory
    --     let testUri = filePathToUri $ cwd </> "test/testdata/FuncTest.hs"

    --     diags <- skipManyTill loggingNotification publishDiagnosticsNotification
    --     liftIO $ diags ^? params @?= (Just $ PublishDiagnosticsParams
    --                 { _uri         = testUri
    --                 , _diagnostics = List
    --                     [ Diagnostic
    --                         (Range (Position 9 6) (Position 10 18))
    --                         (Just DsInfo)
    --                         (Just (StringValue "Redundant do"))
    --                         (Just "hlint")
    --                         "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
    --                         Nothing
    --                     ]
    --                 }
    --             )
        -- let args' = H.fromList [("pos", toJSON (Position 7 0)), ("file", toJSON testUri)]
        --     args = List [Object args']
        --
        -- executeRsp <- request WorkspaceExecuteCommand (ExecuteCommandParams "hare:demote" (Just args) Nothing)
        -- liftIO $ executeRsp ^. result @?= Just (Object H.empty)

        -- editReq <- message :: Session ApplyWorkspaceEditRequest
        -- let expectedTextEdits = List [TextEdit (Range (Position 6 0) (Position 7 6)) "  where\n    bb = 5"]
        --     expectedTextDocEdits = List [TextDocumentEdit (VersionedTextDocumentIdentifier testUri (Just 0)) expectedTextEdits]
        -- liftIO $ editReq ^. params . edit @?= WorkspaceEdit
        --       Nothing
        --       (Just expectedTextDocEdits)
    , multiServerTests
    , multiMainTests
    ]

multiServerTests :: TestTree
multiServerTests = testGroup "multi-server setup" [
    testCase "doesn't have clashing commands on two servers" $ do
        let getCommands = runSession hlsCommand fullCaps "test/testdata" $ do
                rsp <- initializeResponse
                let uuids = rsp ^? result . _Right . capabilities . executeCommandProvider . _Just . commands
                return $ fromJust uuids
        List uuids1 <- getCommands
        List uuids2 <- getCommands
        liftIO $ Set.empty @=? (Set.fromList uuids1 `Set.intersection` Set.fromList uuids2)
    ]

multiMainTests :: TestTree
multiMainTests = testGroup "multiple main modules" [
    testCase "Can load one file at a time, when more than one Main module exists"
        $ runSession hlsCommand fullCaps "test/testdata" $ do
            doc <- openDoc "hlint/ApplyRefact2.hs" "haskell"
            diags <- waitForDiagnosticsFromSource doc "hlint"

            liftIO $ length diags @?= 2

            doc2 <- openDoc "FileWithWarning.hs" "haskell"
            diags2 <- waitForDiagnosticsFromSource doc2 "typecheck"

            liftIO $ length diags2 @?= 1
    ]
