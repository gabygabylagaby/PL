module FunctorList where

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


{- 

class Functor nos permite definir las instancia
de la clase Functor para nuestro tipo de dato List

Es un empaquetador de tipos de datos
f a -> f b



 -}