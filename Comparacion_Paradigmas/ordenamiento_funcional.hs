import Data.List (sortBy) 
import Data.Ord (comparing, Down(Down))

type Estudiante = (String, Int)

ordenarEstudiantes :: [Estudiante] -> [Estudiante]
ordenarEstudiantes = sortBy (comparing (Down . snd) <> comparing fst)

estudiantes :: [Estudiante]
estudiantes = 
    [("Ana", 85)
    , ("Luis", 90)
    , ("Carlos", 85)
    , ("Sofia", 92)
    , ("Maria", 90)
    ]

main :: IO ()
main = print $ ordenarEstudiantes estudiantes
