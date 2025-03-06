-- Definición del tipo de datos para una tarea
data Tarea = Tarea { descripcion :: String, completada :: Bool } deriving (Show)

-- Función para agregar una nueva tarea
agregarTarea :: String -> [Tarea] -> [Tarea]
agregarTarea desc tareas = Tarea desc False : tareas

-- Función para marcar una tarea como completada
marcarCompletada :: Int -> [Tarea] -> [Tarea]
marcarCompletada indice tareas =
  let (inicio, (Tarea desc _):fin) = splitAt indice tareas
  in inicio ++ [Tarea desc True] ++ fin

-- Función para filtrar tareas por estado
filtrarTareas :: Bool -> [Tarea] -> [Tarea]
filtrarTareas estado = filter (\tarea -> completada tarea == estado)

-- Función para mostrar todas las tareas
mostrarTareas :: [Tarea] -> IO ()
mostrarTareas tareas = mapM_ (\tarea -> putStrLn $ descripcion tarea ++ " - " ++ estado tarea) tareas
  where estado tarea = if completada tarea then "[Completada]" else "[Pendiente]"


main :: IO ()
main = do
  let tareasIniciales = []
  let tareas1 = agregarTarea "Aprender Haskell" tareasIniciales
  let tareas2 = agregarTarea "Hacer el parcial de PC" tareas1
  let tareas3 = agregarTarea "Hacer ejercicio" tareas2
  let tareas4 = marcarCompletada 1 tareas3  -- Marcar "Escribir un blog" como completada

  putStrLn "Todas las tareas:"
  mostrarTareas tareas4

  putStrLn "\nTareas pendientes:"
  mostrarTareas (filtrarTareas False tareas4)

  putStrLn "\nTareas completadas:"
  mostrarTareas (filtrarTareas True tareas4)