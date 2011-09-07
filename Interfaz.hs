module Interfaz where

import Char
import Maybe
import IO

import Tablero
import Othello

inicial :: Juego
inicial = J Blanco tableroInicial

-- calcula la mejor a n niveles
-- de profundidad
proximo :: Int -> Juego -> Jugada
proximo nivel juego = mejorJugada valuacionOthello (podar nivel (arbolDeJugadas juego))

realizarMejorJugada :: Int -> Juego -> Juego
realizarMejorJugada nivel tab = fromJust $ mover (proximo nivel tab) tab

data TipoJugador = Humano | Maquina Int

jugar :: TipoJugador -> TipoJugador -> Juego -> IO ()
jugar jugB jugN juego = jugarConAnt jugB jugN juego juego

jugarConAnt :: TipoJugador -> TipoJugador -> Juego -> Juego -> IO ()
jugarConAnt jugB jugN juegoAnt juego@(J turno tablero) = do
  putStr $ mostrarJ juegoAnt juego
  let gan = ganador juego in
    if isNothing gan
     then do
       juego' <- proxJuego (jugador turno) juego
       jugarConAnt jugB jugN juego juego'
     else do
       putStr "\n~~ Final del juego! ~~\n"
       putStr $ "Gana el " ++ show (fromJust gan) ++ "\n"     
  where
    jugador Blanco = jugB
    jugador Negro = jugN

proxJuego :: TipoJugador -> Juego -> IO Juego
proxJuego Humano juego@(J turno _) = do
  putStr $ "Introducir jugada (" ++ show turno ++ "):\n"
  putStr "Jugadas posibles: "
  putStr (movsPosibles juego)
  putStr $ "\n> "
  lupper <- getLine
  let l = normalizar lupper in
    if not (entradaValida l)
      then do
        putStr $ "Entrada invalida.\n"
        proxJuego Humano juego 
      else
        let mov = if l == "paso" then Paso else (M (pos l))
            res = mover mov juego in
          if isNothing res
            then do
              putStr $ "Jugada invalida.\n"
              proxJuego Humano juego 
            else
              return $ fromJust res
  where
    movsPosibles juego = join ", " $
                         map (showmov . fst) $
                         jugadasPosibles juego
    pos l = (l !! 0, ord (l !! 1) - ord '0')
    entradaValida l = l == "paso" || (length l == 2 &&
                      (l !! 0) `elem` ['a'..'h'] &&
                      (l !! 1) `elem` ['1'..'8'])
proxJuego (Maquina nivel) juego@(J turno _) = do
  putStr $ "Pensando... (" ++ show turno ++ ")\n"
  let mov = proximo nivel juego
      res = mover mov juego in do
    seq mov $ putStr $ "JUGADA: " ++ showmov mov ++ "\n"
    if isNothing res
      then do
        putStr $ "La maquina eligio una jugada invalida.\n"
        putStr $ "Seguramente es un error.\n"
        putStr $ "Jugada elegida: " ++ showmov mov
        return $ error "[juego invalido]"
      else
        return $ fromJust res

join _   [] = "<nada>"
join sep xs = foldr1 (\x r -> x ++ sep ++ r) $ xs

showmov (M (col, fil)) = normalizar ([col] ++ show fil)
showmov Paso = "Paso"
normalizar = filter (/= ' ') . map toLower

mostrarJ (J turno0 tablero0) (J turno tablero) =
  "\n--Juega el " ++ show turno ++ "--\n" ++ mostrarT tablero0 tablero

mostrarT (T tablero0) (T tablero) = 
    "   a b c d e f g h  \n" ++
    "  ----------------- \n" ++
    concatMap showFil [8,7..1] ++
    "  ----------------- \n" ++
    "   a b c d e f g h  \n"
  where
    showFil fil = show fil ++ " " ++
                  concatMap (flip showCol fil) ['a'..'h'] ++ lprp 'i' fil ++ " " ++
                  show fil ++ "\n"
    showCol col fil = lprp col fil ++ p (tablero0 (col,fil)) (tablero (col,fil))
    lprp col fil
      | isNothing (tablero0 (col, fil)) &&
        not (isNothing (tablero (col, fil))) = "("
      | col > 'a' &&
        isNothing (tablero0 (colAnt, fil)) &&
        not (isNothing (tablero (colAnt, fil))) = ")"
      | otherwise = "|"
      where
        colAnt = chr (ord col - 1)
    p (Just _) Nothing         = "."
    p _ Nothing                = " "
    p _ (Just Blanco) = "B"
    p _ (Just Negro)   = "N"

