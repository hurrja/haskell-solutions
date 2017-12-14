{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity { runIdentity :: a }

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

newtype Comp f g a = Comp (f (g a)) -- alternative form, no named field

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose fga) = Compose $ (fmap . fmap) h fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ pure $ pure x
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) = undefined
  -- (Compose fgh) <*> (Compose fgx) = Compose $ (<*>) ((<*>) fgh) fgx
