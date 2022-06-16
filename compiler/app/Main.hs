import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import Commons
import BuildParse

main :: IO ()
main = getLine >>= MP.parseTest (filepath <* MP.eof :: MP.Parsec () String String)
