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
esInvalida Paso _ = False
esInvalida (M p) (J c t@(T f)) = (f p /= Nothing) ||
                                  (not $ enRango p) ||
                                  (null $ posicionesAInvertir p t')
    where
        t' = poner p c t

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

jugadasPosibles :: Juego -> [(Jugada, Juego)]
jugadasPosibles juego = (Paso, juego):jugadasReales
    where
        jugadasReales :: [(Jugada, Juego)]
        jugadasReales = unjust jugadas
        jugadas :: [(Jugada, Maybe Juego)]
        jugadas = [(m, jugar m juego) | fila <- [1..8], columna <- ['a'..'h'], let m = M (columna, fila)]
        unjust :: [(a, Maybe b)] -> [(a, b)]
    	unjust [] = []
        unjust ((x, Nothing):xs) = unjust xs
    	unjust ((x, Just y):xs) = (x, y):unjust xs
    	-- unjust == mapMaybe . uncurry $ fmap . (,)

foldArbol :: (a -> [b] -> b) -> Arbol a -> b
foldArbol f = g where
    g (Nodo x xs) = f x (map (foldArbol f) xs)
    
podarFoldable :: Int -> a -> [Arbol a] -> Arbol a
podarFoldable k x arboles | k == 0 = Nodo x []
                          | otherwise = Nodo x mapeados
    where
        mapeados = map (foldArbol . podarFoldable $ k-1) arboles
                
podar :: Arbol a -> Int -> Arbol a
podar n k = foldArbol (podarFoldable k) n -- flip $ foldArbol . podarFoldable

minimax :: Valuacion -> ArbolJugadas -> (Double, [Jugada])
{- si llego al final, es que o no puedo hacer nada, o no quiero hacer nada (i.e. paso) -}
{- tambien puede ser que este tablero sea definitivamente ganador o perdedor, en cuyo
caso lo devuelvo sin evaluar a sus hijos. -}
minimax f (Nodo (chain, game) xs)   | null xs || abs valor == 1 = (valor, chain)
                                    | valor_esta >= valor_resto = jugando_esta
                                    | otherwise = jugando_el_resto
    where
        valor :: Double
        valor = f game
        jugando_esta :: (Double, [Jugada])
        jugando_esta@(valor_esta, _) = minimax (negate . f) (head xs)
        el_resto :: ArbolJugadas
        el_resto = Nodo (chain, game) (tail xs)
        jugando_el_resto :: (Double, [Jugada])
        jugando_el_resto@(valor_resto, _) = minimax f el_resto
   
   
{- version con foldArbol, revisar. Tengo que multiplicar los doubles por -1 en algún lado.  -}   
minimax' :: Valuacion -> ArbolJugadas -> (Double, [Jugada])
minimax' f = foldArbol g
    where
        g :: ([Jugada], Juego) -> [(Double, [Jugada])] -> (Double, [Jugada])
        g (plays, game) xs | null xs || abs valor == 1 = (valor, plays)
                           | this_value >= other_value = option
                           | otherwise = other_option
            where                
                valor :: Double
                valor = f game
                option :: (Double, [Jugada])
                option = head xs
                this_value :: Double
                this_value = fst option
                this_chain :: [Jugada]
                this_chain = snd option
                other_option = g (plays, game) (tail xs)
                other_value = fst other_option
                other_chain = snd other_option
                
mejorJugada :: Valuacion -> ArbolJugadas -> Jugada
mejorJugada = ((head . snd) .) . minimax -- o minimax'        