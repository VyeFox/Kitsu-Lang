import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void (Void)
import System.IO (readFile)

import KitsuByteCode (Expression, KitsuModule (KitsuModule))
import KitsuParse (parseModule)
import KitsuSeasoning (Seasoning, ParseKernel)
import KitsuSpiceRack (simpleLiterals, stringLiteral, tupleLiteral, valueDefinition, typeDefinition)

main :: IO ()
main = do
    res <- readFile "../testing/example.kitsu"
    MP.parseTest (parseModule (simpleLiterals <> stringLiteral <> tupleLiteral <> valueDefinition <> typeDefinition :: Seasoning ParseKernel Void)) res

