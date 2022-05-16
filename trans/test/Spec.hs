import Lib

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

testmsgs :: [String]
testmsgs = [
    "abc",
    "{}",
    "{ }",
    "{ abc }",
    "{ abc,  abc}",
    "{abc}",
    "{{{}}}",
    "{{abc}, {{abc}}, abc}"
    ]

main :: IO ()
main = () <$ sequence ((MP.parseTest (curleyBracketTree parseName)) <$> testmsgs)
