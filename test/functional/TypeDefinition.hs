module TypeDefinition (tests) where

import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import System.Directory
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (expectFailBecause)

tests :: TestTree
tests = testGroup "type definitions" [
    testCase "finds local definition of record variable"
        $ getTypeDefinitionTest (11, 23) (8, 1) (8, 29)
    , testCase "finds local definition of newtype variable"
        $ getTypeDefinitionTest (16, 21) (13, 1) (13, 30)
    , testCase "finds local definition of sum type variable"
        $ getTypeDefinitionTest (21, 13) (18, 1) (18, 26)
    , testCase "finds local definition of sum type constructor"
        $ getTypeDefinitionTest (24, 7) (18, 1) (18, 26)
    , testCase "finds non-local definition of type def"
        $ getTypeDefinitionTest (30, 17) (27, 1) (27, 17)
    , testCase "find local definition of type def"
        $ getTypeDefinitionTest (35, 16) (32, 1) (32, 18)
    , expectFailBecause "Finding symbols cross module is currently not supported" $
      testCase "find type-definition of type def in component"
        $ runTestSession $ do
            doc      <- openDoc "src/Lib2.hs" "haskell"
            otherDoc <- openDoc "src/Lib.hs" "haskell"
            closeDoc otherDoc
            defs <- getTypeDefinitions doc (toPos (13, 20))
            liftIO $ expectToFindDefinition defs (8, 1) (8, 29)
    , testCase "find definition of parameterized data type"
        $ getTypeDefinitionTest (40, 19) (37, 1) (37, 31)
    ]

runTestSession :: Session a -> IO a
runTestSession = failIfSessionTimeout . runSession hlsCommand fullCaps "test/testdata/gototest"

expectToFindDefinition :: [Location] -> (Int, Int) -> (Int, Int) -> Assertion
expectToFindDefinition defs definitionRangeStart definitionRangeEnd = do
    fp <- canonicalizePath "test/testdata/gototest/src/Lib.hs"
    defs @?= [ Location (filePathToUri fp)
                        (Range (toPos definitionRangeStart)
                               (toPos definitionRangeEnd))
             ]

getTypeDefinitionTest :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Assertion
getTypeDefinitionTest symbolPosition definitionRangeStart definitionRangeEnd =
    runTestSession $ do
        doc  <- openDoc "src/Lib.hs" "haskell"
        defs <- getTypeDefinitions doc $ toPos symbolPosition
        liftIO $ expectToFindDefinition defs definitionRangeStart definitionRangeEnd

--NOTE: copied from Haskell.Ide.Engine.ArtifactMap
toPos :: (Int,Int) -> Position
toPos (l,c) = Position (l-1) (c-1)
