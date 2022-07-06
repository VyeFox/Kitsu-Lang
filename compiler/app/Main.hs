import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import KitsuByteCode (Expression)
import KitsuComponents (parseLiteral, parseClosure)

main :: IO ()
main = do
    res <- getLine
    MP.parseTest (parseClosure parseLiteral <* MP.eof :: MP.Parsec Void String Expression) res
