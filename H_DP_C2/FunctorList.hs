module FunctorList where
import Control.Applicative

data List a = Empty 
              | Cons a (List a)
              deriving (Show, Eq, Ord)

{- 
La función `map` en Haskell está diseñada para trabajar con listas estándar de Haskell, representadas como `[a]`. En tu caso, estás intentando usar `map` con una estructura de datos personalizada `List a` que has definido. 

Por otro lado, `fmap` es una función que se define en la clase de tipos `Functor`. Cuando defines una instancia de `Functor` para tu tipo de datos personalizado `List a`, puedes usar `fmap` para aplicar una función a los elementos de tu `List a`.

Por lo tanto, `fmap (+1) (Cons 4 Empty)` funciona porque has definido una instancia de `Functor` para `List a`, pero `map (+1) (Cons 4 Empty)` no funciona porque `map` espera una lista estándar de Haskell `[a]`, no tu tipo de datos personalizado `List a`.
 -}
instance Functor List where
    -- fmap :: (a -> b) -> List a -> List b
    fmap _ Empty = Empty
    fmap f (Cons a xs) = Cons (f a) (fmap f xs)


instance Applicative List where
    -- Funcion pura y aplicativa
    -- (a -> b ) es una funcion que toma un valor de tipo a y devuelve un valor de tipo b
    -- pure :: a -> List a
    pure a = Cons a Empty
    -- (<*>) :: List (a -> b) -> List a -> List b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)
      where
        -- append :: List a -> List a -> List a
        append Empty ys = ys
        append (Cons a as) ys = Cons a (append as ys)

instance Alternative List where
    -- empty :: List a
    empty = Empty

    -- (<|>) :: List a -> List a -> List a
    Empty <|> ys = ys
    ys <|> Empty = ys
    -- xs <|> ys = append xs ys 
    -- append :: List a -> List a -> List a
    -- append Empty ys = ys
    (Cons x xs) <|> ys = Cons x (xs <|> ys) -- Cons x (append xs ys) es lo mismo que Cons x xs <|> ys

-- Maybe
-- 

example :: Maybe Int
example = Just 10 <|> Just 2 <|> Just 6 <|> Nothing
-- ghci > example 
-- Just 10

-- Applicative nos ayuda a aplicar funciones a valores encapsulados en un contexto, como Maybe, List, etc.
-- Alternative nos ayuda a trabajar con valores que pueden tener múltiples opciones, como Maybe, List, etc.
-- Functor nos ayuda a aplicar funciones a valores encapsulados en un contexto, como Maybe, List, etc.

{- append :: List a -> List a -> List a
append Empty xs = xs
append (Cons a as) ys = Cons a (append as ys) -}

{- 

class Functor nos permite definir las instancia
de la clase Functor para nuestro tipo de dato List

Es un empaquetador de tipos de datos
f a -> f b

 -}


{-

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)  



-}




