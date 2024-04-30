module RandomNamePick where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

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

data SchoolHouseCount = SchoolHouseCount {
    griffindorCount :: Int,
    hufflepuffCount :: Int,
    ravenclawCount :: Int,
    slytherinCount :: Int
    } deriving (Show)

data Student = Student{
    lastName::String,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw 
                    | Slytherin 
                    deriving (Eq, Show, Ord, Read)


numberOfConstructors :: Int
numberOfConstructors = length [Griffindor, Hufflepuff, Ravenclaw, Slytherin]

totalStudents :: FilePath -> IO [Student]
totalStudents archivo = do
    contenido <- readFile archivo  -- Leer el contenido del archivo
    let lineas = lines contenido  -- Separar el contenido en líneas
        estudiantes = map parseLine $ filter (\linea -> not (null linea) && head linea /= '-') lineas  -- Filtrar solo las líneas que contienen nombres de estudiantes
    return estudiantes


assignSchoolHouse :: SchoolHouseCount -> IO (SchoolHouse, SchoolHouseCount)
assignSchoolHouse houseCount = do
    house <- pickRandom [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    let newCount = updateHouseCount house houseCount
    return (house, newCount)

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

studentsList :: [Student] -> SchoolHouseCount -> IO SchoolHouseCount
studentsList [] houseCount = return houseCount
studentsList (x:xs) houseCount = do 
    (schoolHouse, newHouseCount) <- assignSchoolHouse houseCount
    putStrLn $ show (x, schoolHouse)
    studentsList xs newHouseCount



studentsPerHouse :: Int -> Int -> Int
studentsPerHouse totalStudents maxStudentsPerHouse = min (totalStudents `div` numberOfConstructors) maxStudentsPerHouse

main :: IO()
main = do
    students <- totalStudents "/home/gabysan/Desktop/Lenguaje de Programacion/H_DP_C1/list.txt"
    let totalStudentsCount = length students
        maxStudentsPerHouse = 10 -- Puedes cambiar este valor según el límite máximo deseado
        initialHouseCount = SchoolHouseCount 0 0 0 0 -- Conteo inicial para cada casa
    putStrLn $ "El número total de estudiantes es: " ++ show totalStudentsCount
    putStrLn $ "El número de estudiantes por casa es: " ++ show (studentsPerHouse totalStudentsCount maxStudentsPerHouse)
    finalHouseCount <- studentsList students initialHouseCount
    putStrLn "Cantidad de estudiantes asignados a cada casa:"
    putStrLn $ "Griffindor: " ++ show (griffindorCount finalHouseCount)
    putStrLn $ "Hufflepuff: " ++ show (hufflepuffCount finalHouseCount)
    putStrLn $ "Ravenclaw: " ++ show (ravenclawCount finalHouseCount)
    putStrLn $ "Slytherin: " ++ show (slytherinCount finalHouseCount)