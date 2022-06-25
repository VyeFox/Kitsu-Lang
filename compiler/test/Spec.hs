import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import qualified BuildParse
import qualified Commons


buildsrcexprs :: [String]
buildsrcexprs = [
    -- good expressions
    "./src",
    "/libs",
    "var_name",
    "\"/libs\" <> ./src <> src",
    -- bad expressions
    "not <-> application",
    "not-a-name",
    "name <>",
    ""
    ]

buildlines :: [String]
buildlines = [
    -- good lines
    "do action",
    "do echo \"Hello World!\"",
    "do name \"-flag\" arg",
    "set src := ./src <> ./libs",
    "file ./build/service.h << src",
    -- bad lines
    "not_a_command",
    "do",
    "set",
    "file"
    ]

builfiles :: [String]
builfiles = [
    -- good files
    "do echo \"enter build\" \n\
    \set src := ./src <> ./libs   \t\n\
    \file ./build/service.h << src\n\n\
    \do echo \"exit build\"",

    "do echo \"just a do\"\n",
    
    "file ./build/service.h << ./src <> src <> \"/weird dir\"  "
    ]

testparseall :: Show a => MP.Parsec String String a -> [String] -> IO ()
testparseall parser cases = () <$ sequence ((\c -> print c *> MP.parseTest parser c) <$> cases)

main :: IO ()
main = do
    putStrLn "\n::Build File Expression Testing::\n"
    testparseall (BuildParse.kitexpression <* MP.eof) buildsrcexprs
    putStrLn "\n::Build File Line Testing::\n"
    testparseall (BuildParse.kitbuildline <* MP.eof) buildlines
    putStrLn "\n::Build File Testing::\n"
    testparseall BuildParse.kitbuildfile builfiles

