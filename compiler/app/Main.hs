import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import BuildParse (KitBuildLine, kitbuildfile)

main :: IO ()
main = getLine >>= MP.parseTest (kitbuildfile:: MP.Parsec Void String [KitBuildLine])
