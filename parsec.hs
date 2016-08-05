{-# LANGUAGE LambdaCase 
           , DeriveFunctor
           , OverloadedStrings
  #-}

import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative

data Parser a 
    = Parser { runParser :: Text -> [(a, Text)] }
    deriving(Functor)

getParses :: Parser a -> Text -> [a]
getParses p t = fmap fst . filter (T.null . snd) . runParser p $ t

instance Applicative Parser where
    pure a = Parser $ \t -> [(a, t)]
    Parser phi <*> Parser u 
        = Parser $ \t ->
            do (f, t') <- phi t
               (a, t'') <- u t'
               return (f a, t'')
         -- ^ Interestingly, all we need is []'s Monad instance

instance Alternative Parser where
    empty = Parser $ \t -> []
    Parser u <|> Parser v = 
        Parser $ \t -> u t ++ v t
-- many :: Parser a -> Parser [a]
-- some :: Parser a -> Parser [a]

-- many' :: Alternative f => f a -> f [a]
-- many' fa = ((:) <$> fa <*> many' fa) <|> pure []

many'' :: Parser a -> Parser [a]
many'' (Parser u) 
    = Parser $ \t -> 
        do (a, t') <- u t
           case runParser (many'' $ Parser u) t' of
                [] -> return ([a], t')
                list 
                    -> do (as, t'') <- list
                          return $ (a:as, t'')
    

instance Monad Parser where
    return = pure
    Parser u >>= phi 
        = Parser $ \t ->
            do (a, t') <- u t
               runParser (phi a) t'

symbol :: Char -> Parser Char
symbol c = Parser $ \t -> 
    case T.uncons t of
         Nothing -> []
         Just (c', t')
             | c' == c -> [(c, t')]
             | otherwise -> []

string :: Text -> Parser Text
string s = Parser $ \t -> 
    case T.stripPrefix s t of
         Nothing -> []
         Just t' -> [(s, t')]

char :: Parser Char
char = Parser $ \t -> 
    case T.uncons t of
         Nothing -> []
         Just (c,t') -> [(c,t')]
-- ^ char === satisfy (const True)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \t -> 
    case T.uncons t of
         Just (c,t') 
            | p c -> [(c,t')]
         _ -> []
