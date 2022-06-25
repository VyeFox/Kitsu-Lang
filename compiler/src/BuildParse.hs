module BuildParse where

import Commons

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP


type BuildParser = MP.Parsec String String


data KitExpression
    = VarName String -- src
    | Path String -- ./servicesrc
    | Union KitExpression KitExpression -- src + ./servicesrc
    deriving (Show)


data KitBuildLine
    = Do String [String] -- do echo hello
    | SetVar String KitExpression -- set servicesrc = src <> ./servicesrc
    | File String KitExpression -- file ./build/service.hpp << cpp << servicesrc
    deriving (Show)


name :: BuildParser String
name = MP.label "name" $ selectUntil1 MP.space1 <:> (MP.many (MP.noneOf "\"") <* MP.eof)


filepath :: BuildParser String
filepath = MP.label "filepath" $ (MP.try stringLiteral <|> (selectUntil1 MP.space1 <:> (MP.many (MP.noneOf "\"") <* MP.eof))) <:> (inner <* MP.eof)
    where
        step = MP.label "step" $ (:) <$> MP.char '/' <*> MP.some (MP.noneOf "/\0")
        inner =
            MP.try ((<$>) join $ (:) <$> MP.string "." <*> MP.many (MP.try step)) <|>
            MP.try ((<$>) join $ MP.some step) <|>
            MP.string "/"


kitexpression :: BuildParser KitExpression
kitexpression = MP.try union <|> rest
    where
        var = VarName <$> name
        path = MP.label "file path" $ Path <$> filepath
        rest = MP.try var <|> path
        union = Union <$> rest <*> (MP.hspace1 *> MP.string "<>" *> MP.hspace1 *> kitexpression)


kitbuildline :: BuildParser KitBuildLine
kitbuildline = MP.try docmd <|> MP.try setvar <|> file
    where
        cmdwrd = MP.label "command word" $ MP.try stringLiteral <|> name
        docmd = Do <$> (MP.string "do" *> MP.hspace1 *> cmdwrd) <*> MP.many (MP.try $ MP.hspace1 *> cmdwrd)
        setvar = SetVar <$> (MP.string "set" *> MP.hspace1 *> name) <*> (MP.hspace1 *> MP.string ":=" *> MP.space1 *> kitexpression)
        file = File <$> (MP.string "file" *> MP.hspace1 *> filepath) <*> (MP.hspace1 *> MP.string "<<" *> MP.hspace1 *> kitexpression)


kitbuildfile :: BuildParser [KitBuildLine]
kitbuildfile = startbuffer *> ((:) <$> kitbuildline <*> MP.many (MP.try $ linegap *> kitbuildline)) <* MP.space <* MP.eof
    where
        linegap = MP.label "next line" $ MP.some (MP.try $ MP.hspace *> MP.char '\n')
        startbuffer = MP.many (MP.try $ MP.hspace *> MP.char '\n')



