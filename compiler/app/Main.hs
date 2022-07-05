import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import KitsuByteCode (Literal)
import KitsuComponents (parseLiteral)

main :: IO ()
main = do
    res <- getLine
    MP.parseTest (parseLiteral <* MP.eof :: MP.Parsec Void String Literal) res
