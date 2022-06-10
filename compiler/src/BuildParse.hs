module BuildParse where

import Commons

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )


type BuildParser = MP.Parsec Void String


data Target
    = RAW
    | CPP
    | JS
    | JAVA
    deriving (Show, Eq)


data KitExpression
    = VarName String -- src
    | Path String -- ./servicesrc
    | Union KitExpression KitExpression -- src + ./servicesrc
    deriving (Show)


data KitBuildLine
    = Do String [String] -- do echo hello
    | SetVar String KitExpression -- set servicesrc = src <> ./servicesrc
    | File String Target KitExpression -- file ./build/service.hpp << cpp << servicesrc
    deriving (Show)


varname :: BuildParser String
varname = MP.label "variable name" $ MP.some $ MP.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"


target :: BuildParser Target
target = MP.label "transpilation target" $
    MP.try (RAW <$ MP.string "raw") <|>
    MP.try (CPP <$ MP.string "cpp") <|>
    MP.try (JS <$ MP.string "js") <|>
    (JAVA <$ MP.string "java")


kitexpression :: BuildParser KitExpression
kitexpression = MP.try union <|> rest
    where
        var = VarName <$> varname
        path = Path <$> MP.label "file path" (MP.try rawpath <|> strpath)
        rest = MP.try var <|> path
        union = Union <$> rest <*> MP.label "union" (inlinegap *> MP.string "<>" *> inlinegap *> kitexpression)


kitbuildline :: BuildParser KitBuildLine
kitbuildline = MP.try docmd <|> MP.try setvar <|> file
    where
        cmdwrd = MP.label "command word" $ MP.try rawfile <|> strfile
        docmd = Do <$> (MP.string "do" *> inlinegap *> cmdwrd) <*> MP.many (MP.try $ inlinegap *> cmdwrd)
        setvar = SetVar <$> (MP.string "set" *> inlinegap *> varname) <*> (inlinegap *> MP.string ":=" *> inlinegap *> kitexpression)
        file = File <$> (MP.string "file" *> inlinegap *> (MP.try rawpath <|> strpath)) <*> (inlinegap *> MP.string "=<<" *> inlinegap *> target <* inlinegap <* MP.string "<<") <*> (inlinegap *> kitexpression)


kitbuildfile :: BuildParser [KitBuildLine]
kitbuildfile = startbuffer *> ((:) <$> kitbuildline <*> MP.many (MP.try $ linegap *> kitbuildline)) <* softend
    where
        linegap = MP.label "next line" $ MP.some (MP.try $ inlinespace *> MP.char '\n')
        startbuffer = MP.many (MP.try $ inlinespace *> MP.char '\n')



