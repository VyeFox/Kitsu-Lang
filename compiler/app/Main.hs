import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Data.Void (Void)

import KitsuByteCode (Expression)
import KitsuComponents (reflect)
import KitsuSyntaxBundling (ParseKernel, rExpression)
import KitsuSyntaxBundles (baseExpressions, stringLiteral, tupleLiteral, valueDefinition, typeDefinition)

main :: IO ()
main = do
    exp <- getLine
    MP.parseTest (rExpression (reflect $ tupleLiteral <> stringLiteral <> baseExpressions <> valueDefinition <> typeDefinition) MP.eof :: MP.Parsec Void String (ParseKernel Expression)) exp
    
