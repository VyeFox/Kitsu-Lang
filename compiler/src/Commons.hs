{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Commons where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )


-- optional white space no returns
inlinespace :: forall e m. (MP.MonadParsec e String m) => m String
inlinespace = MP.many $ MP.oneOf " \t"

-- optional white space can return
space :: forall e m. (MP.MonadParsec e String m) => m String
space = MP.many $ MP.oneOf " \t\n\r"

-- white space no returns
inlinegap :: forall e m. (MP.MonadParsec e String m) => m String
inlinegap = MP.some $ MP.oneOf " \t"

-- white space can return
gap :: forall e m. (MP.MonadParsec e String m) => m String
gap = MP.some $ MP.oneOf " \t\n\r"

-- not typable: " \"\t\n\r\\"
pathchars :: [Char]
pathchars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "|¬`¦!£$%^&*()_+-={}[]:@~;'#<>?,."


rawfile :: forall e m. (MP.MonadParsec e String m) => m String
rawfile = MP.some $ MP.oneOf pathchars


strfile :: forall e m. (MP.MonadParsec e String m) => m String
strfile = MP.char '"' *> MP.label "string body" (MP.many pathchar) <* MP.char '"'
    where
        pathchar = 
            MP.try ('\"' <$ MP.string "\\\"") <|>
            MP.try ('\t' <$ MP.string "\\t") <|>
            MP.try ('\n' <$ MP.string "\\n") <|>
            MP.try ('\r' <$ MP.string "\\r") <|>
            MP.try ('\\' <$ MP.string "\\\\") <|>
            MP.oneOf (' ':pathchars)


-- path "/" is supported for proper file path parsing and not because I think it's a good idea
rawpath :: forall e m. (MP.MonadParsec e String m) => m String
rawpath = MP.try localpath <|> MP.try path <|> MP.string "/" -- 
    where
        pathchar = MP.oneOf pathchars
        segment = (:) <$> MP.char '/' <*> ((:) <$> pathchar <*> MP.many pathchar)
        path = (<$>) join $ (:) <$> segment <*> MP.many segment
        localpath = (:) <$> MP.char '.' <*> (join <$> MP.many segment)


-- path "/" is supported for proper file path parsing and not because I think it's a good idea
strpath :: forall e m. (MP.MonadParsec e String m) => m String
strpath = MP.char '"' *> MP.label "string body" (MP.try localpath <|> MP.try path <|> MP.string "/") <* MP.char '"'
    where
        pathchar =
            MP.try ('\"' <$ MP.string "\\\"") <|>
            MP.try ('\t' <$ MP.string "\\t") <|>
            MP.try ('\n' <$ MP.string "\\n") <|>
            MP.try ('\r' <$ MP.string "\\r") <|>
            MP.try ('\\' <$ MP.string "\\\\") <|>
            MP.oneOf (' ':pathchars)
        segment = (:) <$> MP.char '/' <*> MP.some pathchar
        path = join <$> MP.some segment
        localpath = join <$> ((:) <$> MP.string "." <*> MP.many segment)


softend :: forall e m. (MP.MonadParsec e String m) => m String
softend = space <* MP.eof




