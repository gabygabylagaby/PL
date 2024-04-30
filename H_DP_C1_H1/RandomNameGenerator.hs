module RandomNamePick where

import System.IO
import System.Random
import Control.Monad (liftM)

{- 
A solucionar:
1. correr el programa y ver que no se asigna una casa a cada estudiante : 
-> ghci RandomNameGenerator.hs 
-> GHCi no puede cargar los módulos Data.Text y Data.Text.IO porque pertenecen al paquete text-2.0.2, y está oculto.
->  :set -package text
-> :load RandomNameGenerator.hs
2. Modificar la logica de estudiantes por casa para que se asigne una casa a cada estudiante, si esta comentado con '-' es nulo, no existe ese estudiante
3. Saber a cuantos estudiantes puede almacenar una casa:
-> La lista puede crecer o caso contrario, tomar en cuenta total de estudiantes
-> Total de casas = 4 lo tengo que mostrar en el programa
-> estudiantesPorCasa = totalEstudiantes / totalCasas
-> Casa llena, generar de nuevo la casa
-}

type StudentName = String
type LastName = String
type ResultLimit = (Int, Int) 

data SchoolHouseCount = SchoolHouseCount {
    griffindorCount :: Int,
    hufflepuffCount :: Int,
    ravenclawCount :: Int,
    slytherinCount :: Int
    } deriving (Show)

data Student = Student{
    lastName::LastName,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw 
                    | Slytherin 
                    deriving (Eq, Show, Ord, Read)

numberOfConstructors :: Int
numberOfConstructors = length [Griffindor, Hufflepuff, Ravenclaw, Slytherin]

readFileContent :: FilePath -> IO [String]
readFileContent archivo = do
    contenido <- readFile archivo  
    return $ lines contenido  

filterStudents :: [String] -> [Student]
filterStudents lineas = 
    map parseLine $ filter (\linea -> not (null linea) && head linea /= '-') lineas

totalStudents :: FilePath -> IO [Student]
totalStudents archivo = do
    lineas <- readFileContent archivo
    return $ filterStudents lineas

assignHouse :: SchoolHouseCount -> Int -> IO SchoolHouse
assignHouse houseCount studentsPerHouse = do
    let houses = filter (\house -> houseCountForHouse house houseCount < studentsPerHouse) [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    if null houses
        then pickRandom [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
        else pickRandom houses

actualySchoolHouse :: SchoolHouseCount -> Int -> IO (SchoolHouse, SchoolHouseCount)
actualySchoolHouse houseCount studentsPerHouse = do
    house <- assignHouse houseCount studentsPerHouse
    let newCount = updateHouseCount house houseCount
    return (house, newCount)

houseCountForHouse :: SchoolHouse -> SchoolHouseCount -> Int
houseCountForHouse house houseCount =
    case house of
        Griffindor -> griffindorCount houseCount
        Hufflepuff -> hufflepuffCount houseCount
        Ravenclaw -> ravenclawCount houseCount
        Slytherin -> slytherinCount houseCount

    
updateHouseCount :: SchoolHouse -> SchoolHouseCount -> SchoolHouseCount
updateHouseCount house houseCount =
    case house of
        Griffindor -> houseCount { griffindorCount = griffindorCount houseCount + 1 }
        Hufflepuff -> houseCount { hufflepuffCount = hufflepuffCount houseCount + 1 }
        Ravenclaw -> houseCount { ravenclawCount = ravenclawCount houseCount + 1 }
        Slytherin -> houseCount { slytherinCount = slytherinCount houseCount + 1 }

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1) 
    return (xs !! idx) 

split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student{lastName=lastName, name=name}

studentsList :: [Student] -> SchoolHouseCount -> Int -> IO SchoolHouseCount
studentsList [] houseCount _ = return houseCount -- Caso base
studentsList (x:xs) houseCount studentsPerHouse = do  -- Caso recursivo
    (schoolHouse, newHouseCount) <- actualySchoolHouse houseCount studentsPerHouse -- Asignar casa
    putStrLn $ show (x, schoolHouse) -- Imprimir estudiante y casa asignada
    studentsList xs newHouseCount studentsPerHouse -- Llamada recursiva

division :: Int -> Int -> Int
division x y = x `div` y

modulo :: Int -> Int -> Int
modulo x y = x `mod` y

resultLimitForHouse :: Int -> Int -> ResultLimit
resultLimitForHouse x y = (division x y, modulo x y)

main :: IO()
main = do
    students <- totalStudents "/home/gabysan/Documents/sudgoafs/Programming-Languages/H_DP_C1_H1/list.txt"
    let totalStudentsCount = length students
        (studentsPerHouse, remainder) = resultLimitForHouse totalStudentsCount numberOfConstructors
        initialHouseCount = SchoolHouseCount 0 0 0 0 -- Conteo inicial para cada casa
    putStrLn $ "El número total de estudiantes es: " ++ show totalStudentsCount
    putStrLn $ "El número de estudiantes por casa es: " ++ show studentsPerHouse
    putStrLn $ "Sobrante: " ++ show remainder
    finalHouseCount <- studentsList students initialHouseCount studentsPerHouse
    putStrLn "Cantidad de estudiantes asignados a cada casa:"
    putStrLn $ "Griffindor: " ++ show (griffindorCount finalHouseCount)
    putStrLn $ "Hufflepuff: " ++ show (hufflepuffCount finalHouseCount)
    putStrLn $ "Ravenclaw: " ++ show (ravenclawCount finalHouseCount)
    putStrLn $ "Slytherin: " ++ show (slytherinCount finalHouseCount)