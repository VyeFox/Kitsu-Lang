import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import KitsuByteCode (Expression)
import KitsuComponents (parseLiteral, parseExpression, TypeDefAttached)
import Data.Functor.Identity (Identity)

main :: IO ()
main = do
    res <- getLine
    MP.parseTest (parseExpression parseLiteral <* MP.eof :: MP.Parsec Void String (TypeDefAttached Expression)) res
