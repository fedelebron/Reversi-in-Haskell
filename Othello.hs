{-# LANGUAGE ScopedTypeVariables #-}

module Othello where

import Data.Maybe
import Tablero
import Data.Char
import Data.List

data Juego = J Color Tablero
data Jugada = M Posicion | Paso
  deriving (Show, Eq)

data Arbol a = Nodo a [Arbol a] deriving Show
type ArbolJugadas = Arbol ([Jugada], Juego)

type Valuacion = Juego -> Double

instance Show Juego where
  show (J turno tablero) = "\n--Juega el " ++ show turno ++ "--\n" ++ show tablero

arbolDeJugadas :: Juego -> ArbolJugadas
arbolDeJugadas j = Nodo ([], j) $ zipWith agmov movs hijos
  where agmov m (Nodo (ms, r) hs) = Nodo ((m:ms), r) (map (agmov m) hs)
        movsJuegos = jugadasPosibles j
        movs = map fst movsJuegos
        hijos = map (arbolDeJugadas . snd) movsJuegos

esInvalida :: Jugada -> Juego -> Bool
esInvalida Paso j = any (flip esValida j) (map fst $ jugadasSinPaso j)
esInvalida (M p) (J c t@(T f)) = (f p /= Nothing) ||
                                  (not $ enRango p) ||
                                  (null $ posicionesAInvertir p t')
    where
        t' = poner p c t

esValida :: Jugada -> Juego -> Bool
esValida = (not .) . esInvalida 

aplicarJugada :: Jugada -> Juego -> Juego -- asume valida
aplicarJugada Paso (J c t) = J (cambiarColor c) t
aplicarJugada (M p) (J c t) = J (cambiarColor c) (ponerPieza p c t)
    where
        ponerPieza :: Posicion -> Color -> Tablero -> Tablero
        ponerPieza p c t = invertirTodas (posicionesAInvertir p t') t'
            where
                t' :: Tablero
                t' = poner p c t

jugar :: Jugada -> Juego -> Maybe Juego
jugar jugada juego = if esInvalida jugada juego then Nothing
                     else Just (aplicarJugada jugada juego)

jugadasSinPaso :: Juego -> [(Jugada, Juego)]
jugadasSinPaso juego = [(m, aplicarJugada m juego) |  fila <- [1..8], 
                                                columna <- ['a'..'h'], 
                                                let m = M (columna, fila), 
                                                esValida m juego]

jugadasPosibles :: Juego -> [(Jugada, Juego)]
jugadasPosibles juego = if esInvalida Paso juego 
                        then jugadasSinPaso juego
                        else [(Paso, juego)]

foldArbol :: (a -> [b] -> b) -> Arbol a -> b
foldArbol f = g where
    g (Nodo x xs) = f x (map (foldArbol f) xs)


podar :: Int -> Arbol a -> Arbol a
podar = flip $ foldArbol podar'
    where
        podar' :: a -> [Int -> Arbol a] -> Int -> Arbol a
        podar' x fs k = Nodo x xs
            where
                xs  | k == 0 = []
                    | otherwise = map ($ (k-1)) fs

minimax :: Valuacion -> ArbolJugadas -> (Double, [Jugada])
minimax f = foldArbol g
    where
        g :: ([Jugada], Juego) -> [(Double, [Jugada])] -> (Double, [Jugada])
        g (plays, game) xs | null xs || abs valor == 1 = (valor, plays)
                           | otherwise = selectMaxMinus
            where                
                valor :: Double
                valor = f game
                maxMinusDouble :: Double
                maxMinusDouble = maximum $ map (negate . fst) xs
                selectMaxMinus :: (Double, [Jugada])
                selectMaxMinus = head $ filter (( == maxMinusDouble) . fst) xs

                
mejorJugada :: Valuacion -> ArbolJugadas -> Jugada
mejorJugada = ((head . snd) .) . minimax

contar :: Color -> Juego -> Int
contar col (J _ t) = length [Just col == contenido (c, i) t | 
                                    c <- ['a'..'h'], 
                                    i <- [1..8]]


ganador :: Juego -> Maybe Color
ganador j | elTieneQuePasar && yoDespuesPaso = Just colorDominante
          | otherwise = Nothing
    where
        tieneQuePasar (Just g) = any ((== Paso) . fst) (jugadasPosibles g)
        elTieneQuePasar = tieneQuePasar (Just j)
        Just j' = jugar Paso j
        yoDespuesPaso = tieneQuePasar (jugar Paso j')
        colorDominante = if (contar Negro j) > (contar Blanco j) 
                         then Negro 
                         else Blanco
                         
                         
valuacionOthello :: Valuacion
valuacionOthello j@(J c t)  | ganador j == Just c = 1
							| ganador j == Nothing = (fromIntegral 2) * (m / tot) - fromIntegral 1
							| otherwise = -1
	where 
		m = fromIntegral $ contar c j
		tot = m + (fromIntegral $ contar (cambiarColor c) j)