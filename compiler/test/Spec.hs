import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import qualified BuildParse
import qualified Commons

filepaths :: [String]
filepaths = [
    -- good paths
    "./local",
    "./local/build",
    "/",
    "/global",
    "/global/libs",
    "./!weird-path::/.",
    -- bad paths
    "",
    "bad-path",
    "!weird-bad-path::/.",
    "//"
    ]

quotfilepaths :: [String]
quotfilepaths = [
    -- wrapped good paths
    "\"./local\"",
    "\"./local/build\"",
    "\"/\"",
    "\"/global\"",
    "\"/global/libs\"",
    "\"./!weird-path::/.\"",
    -- special good paths
    "\"./ \\t\\n\\r\"",
    "\"/ \\t\\n\\r\"",
    "\"./\\\\\"",
    "\"/\\\\\"",
    -- wrapped bad paths
    "\"\"",
    "\"bad-path\"",
    "\"!weird-bad-path::/.\"",
    "\"//\"",
    -- special bad paths
    "\"\t\"",
    "\"\n\"",
    "\"\r\"",
    "\"\\\"",
    "\"\0\""
    ]

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
    "file ./build/service.hpp =<< cpp << src",
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
    \file ./build/service.hpp =<< cpp << src\n\n\
    \do echo \"exit build\"",

    "do echo \"just a do\"\n",
    
    "file ./build/service.hpp =<< cpp << ./src <> src <> \"/weird dir\"  "
    ]

testparseall :: Show a => MP.Parsec Void String a -> [String] -> IO ()
testparseall parser cases = () <$ sequence ((\c -> print c *> MP.parseTest parser c) <$> cases)

main :: IO ()
main = do
    putStrLn "\n::Raw File Path Testing::\n"
    testparseall (Commons.rawpath <* MP.eof) filepaths
    putStrLn "\n::Str File Path Testing::\n"
    testparseall (Commons.strpath <* MP.eof) quotfilepaths
    putStrLn "\n::Build File Expression Testing::\n"
    testparseall (BuildParse.kitexpression <* MP.eof) buildsrcexprs
    putStrLn "\n::Build File Line Testing::\n"
    testparseall (BuildParse.kitbuildline <* MP.eof) buildlines
    putStrLn "\n::Build File Testing::\n"
    testparseall BuildParse.kitbuildfile builfiles

