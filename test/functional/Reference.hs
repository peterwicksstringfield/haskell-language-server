module Reference (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Data.List
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "references" [
    ignoreTestBecause "no references handler" $
    testCase "works with definitions" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "References.hs" "haskell"
        let pos = Position 2 7 -- foo = bar <--
        refs <- getReferences doc pos True
        liftIO $ map (Location (doc ^. uri)) [
                mkRange 4 0 4 3
            , mkRange 8 11 8 14
            , mkRange 7 7 7 10
            , mkRange 4 14 4 17
            , mkRange 4 0 4 3
            , mkRange 2 6 2 9
            ] `isInfixOf` refs @? "Contains references"

    , ignoreTestBecause "no references handler" $
      testCase "works without definitions" $ runSession hlsCommand fullCaps "test/testdata" $ do
          doc <- openDoc "References.hs" "haskell"
          let pos = Position 2 7 -- foo = bar <--
          -- This False means do not include the definition of bar as a
          -- reference to bar.
          refs <- getReferences doc pos False
          liftIO $ not (Location (doc ^. uri) (mkRange 4 0 4 3) `elem` refs)
              @? "Should not treat the definition as a reference when we ask to exclude definitions"
    ]
    where mkRange sl sc el ec = Range (Position sl sc) (Position el ec)
