module Stuff exposing (..)

import Html

type Either a b 
    = Left a 
    | Right b

either : (a -> c) -> (b -> c) -> Either a b -> c
either f g u = 
    case u of 
      Left a -> f a
      Right b -> g b

bindEither : Either a b -> (b -> Either a c) -> Either a c
bindEither u f = 
    case u of
      Left a -> Left a
      Right b -> f b

apEither : Either err (a -> b) -> Either err a -> Either err b
apEither u v = 
    case (u,v) of
      (Left err, _) -> Left err
      (Right _, Left err) -> Left err
      (Right f, Right a) -> Right (f a)
