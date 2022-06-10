module BuildParse where

import Commons

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )


type BuildParser = MP.Parsec Void String


data Target = RAW | CPP | JS | JAVA


data KitExpression
    = VarName String -- src
    | Path String -- ./servicesrc
    | Union KitExpression KitExpression -- src + ./servicesrc


data KitBuildLine
    = Do String [String] -- do echo hello
    | SetVar String KitExpression -- set servicesrc = src + ./servicesrc
    | File String Target KitExpression -- file ./build/service.hpp << cpp << servicesrc


varname :: BuildParser String
varname = MP.many $ MP.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"


target :: BuildParser Target
target = (RAW <$ MP.string "raw") <|> (CPP <$ MP.string "cpp") <|> (JS <$ MP.string "js") <|> (JAVA <$ MP.string "java")


kitexpression :: BuildParser KitExpression
kitexpression = MP.try union <|> rest
    where
        var = VarName <$> varname
        path = Path <$> (MP.try rawpath <|> strpath)
        rest = MP.try var <|> path
        union = Union <$> rest <*> (inlinegap *> MP.string "+" *> inlinegap *> kitexpression)


kitbuildline :: BuildParser KitBuildLine
kitbuildline = MP.try docmd <|> MP.try setvar <|> file
    where
        cmdwrd = MP.many $ MP.oneOf pathchars
        docmd = Do <$> (MP.string "do" *> inlinegap *> cmdwrd) <*> MP.many (inlinegap *> cmdwrd)
        setvar = SetVar <$> (MP.string "set" *> inlinegap *> varname) <*> (inlinegap *> kitexpression)
        file = File <$> (MP.string "file" *> inlinegap *> (MP.try rawpath <|> strpath)) <*> (inlinegap *> target) <*> (inlinegap *> kitexpression)


kitbuildfile :: BuildParser [KitBuildLine]
kitbuildfile = inner <* softend
    where
        inner = (:) <$> kitbuildline <*> MP.many (space *> MP.char '\n' *> kitbuildline)



