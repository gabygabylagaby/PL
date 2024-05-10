module Applicative where

{- 
 En java todo hereda de Object, en Haskell todo es un Functor y todo lo hereda Kind
type constructor es un tipo de dato que toma un tipo y devuelve un tipo
Higher kinded types es un tipo de dato que toma un tipo de tipo y devuelve un tipo
High Order Function es una función de alto nivel que toma una función como parametro y devuelve una función :
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

En react seria un componente de orden superior es Asincrono.
Manejo de errores en Haskell es el either a y b, right es un valor que tiene que ser desempaquetado y left es un error.

-- Para que le mostre x3 

 -}


data Traffic = Red
              | Yellow
              | Green


instance Eq Traffic where
    -- (==) :: Traffic -> Traffic -> Bool si definimos asi no es necesario definir (/=)
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False
    --(/=) :: Treffic -> Treffic -> Bool
    x /= y = not (x == y)

{- 
3 leyes:
1. fmap id = id   -> Ley de identidad -> Llamas a fmap con la función identidad, el resultado es el mismo que el valor original
2. fmap (f . g) = fmap f . fmap g -> Ley de composición -> Llamas a fmap con la composición de dos funciones, es lo mismo que llamar a fmap con una función y luego llamar a fmap con la otra función
3. fmap f . pure = pure . f  -> Ley de Preservacion de la estructura -> Llamas a fmap con una función que envuelve un valor en un contexto, es lo mismo que envolver el valor en el contexto y luego llamar a fmap con la función
 -}


-- Ley 1

-- fmap id Red
-- Red
-- id Red
-- Red
{- 
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

 -}

-- Ley 2
{- 
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

 -}

-- Ley 3

---------- Example 1 de todo esto

data CMaybe a = CNothing 
                | CJust Int a deriving (Show)


instance Functor CMaybe where
    fmap _ CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)
    {- Ejemplo ghci
    fmap (++ "ha")(CJust 0 "ho") 
    fmap (++ "he")(fmap (++ "ha")(CJust 0 "ho"))
    fmap (++ "blah" ) CNothing -}



