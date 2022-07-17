import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void (Void)
import System.IO (readFile)

import KitsuComponents (reflect)
import KitsuByteCode (Expression, KitsuModule (KitsuModule))
import KitsuParse (parseModule)
import KitsuSyntaxBundling (ParseKernel, SyntaxReflection)
import KitsuSyntaxBundles (stringLiteral, tupleLiteral, valueDefinition, typeDefinition, baseExpressions)

main :: IO ()
main = do
    res <- readFile "../testing/example.kitsu"
    MP.parseTest (parseModule (reflect $ baseExpressions <> stringLiteral <> tupleLiteral <> valueDefinition <> typeDefinition :: SyntaxReflection ParseKernel Void)) res

