import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )
import System.IO (readFile)

import KitsuByteCode (Expression)
import KitsuComponents (parseExpression)
import KitsuSeasoning (ParseKernel)
import KitsuSpiceRack (simpleLiterals, stringLiteral, tupleLiteral)

main :: IO ()
main = do
    dir <- getLine
    res <- readFile dir
    MP.parseTest (parseExpression (tupleLiteral <> stringLiteral <> simpleLiterals) <* MP.eof :: MP.Parsec Void String (ParseKernel Expression)) res
