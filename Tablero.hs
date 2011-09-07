--{# LANGUAGE ExplicitForAll, UnicodeSyntax #}
module Tablero where

import Data.Char
import Data.Maybe

data Color = Blanco | Negro deriving (Show, Eq, Ord)
type Posicion = (Char, Int) 
data Tablero = T (Posicion -> Maybe Color)

instance Show Tablero where
  show (T tablero) = 
      "   a b c d e f g h  \n" ++
      "  ----------------- \n" ++
      concatMap showFil [8,7..1] ++
      "  ----------------- \n" ++
      "   a b c d e f g h  \n"
    where
      showFil fil = show fil ++ " " ++
                    concatMap (showCol fil) ['a'..'h'] ++ "| " ++
                    show fil ++ "\n"
      showCol fil col = "|" ++ p (tablero (col,fil))
      p Nothing = " "
      p (Just Blanco) = "B"
      p (Just Negro) = "N"

vacio :: Tablero
vacio = T emptyResponse
    where
        emptyResponse :: Posicion -> Maybe Color
        emptyResponse = const Nothing
        
tableroInicial :: Tablero
tableroInicial = T initialResponse
    where
        initialResponse :: Posicion -> Maybe Color
        initialResponse pos = case pos of
            ('d', 4) -> Just Negro
            ('e', 5) -> Just Negro
            ('e', 4) -> Just Blanco
            ('d', 5) -> Just Blanco
            otherwise -> Nothing

cambiarColor :: Color -> Color
cambiarColor Negro = Blanco
cambiarColor Blanco = Negro
            
contenido :: Posicion -> Tablero -> Maybe Color
contenido = flip funcionTablero
    where
        funcionTablero :: Tablero -> Posicion -> Maybe Color
        funcionTablero (T f) = f  

poner :: Posicion -> Color -> Tablero -> Tablero
poner p c (T f) = T f'
    where
        f' :: Posicion -> Maybe Color
        f' pos = if pos == p then Just c
                 else f pos
            
desplazarFila :: Int -> Posicion -> Posicion      
desplazarFila x (c, f) = (c, f+x)

desplazarColumna :: Int -> Posicion -> Posicion
desplazarColumna x (c, f) = (chr $ ord c + x, f)

enRango :: Posicion -> Bool
enRango (columna, fila) = c <= h && a <= c && fila <= 8 && 0 <= fila
    where
        c = ord columna
        h = ord 'h'
        a = ord 'a'
        
generar :: Posicion -> (Posicion -> Posicion) -> [Posicion]
generar = (takeWhile enRango .) . flip iterate

posicionesAInvertir :: Posicion -> Tablero -> [Posicion]
posicionesAInvertir p (T f) = direcciones >>= inversiones p
    where
        direcciones :: [Posicion -> Posicion]
        direcciones = [desplazarFila x . desplazarColumna y | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]                

        inversiones :: Posicion -> (Posicion -> Posicion) -> [Posicion]
        inversiones p dir | enemigas == todo = []
                          | f (dir (last enemigas)) == miColor = enemigas
                          | otherwise = []
            where                
                miColor :: Maybe Color
                miColor = f p
                
                todo :: [Posicion]
                todo = generar (dir p) dir
                
                enemigas :: [Posicion]
                enemigas = takeWhile esEnemiga todo
                    where
                        esEnemiga :: Posicion -> Bool
                        esEnemiga x = (f x == fmap cambiarColor miColor)

invertirTodas :: [Posicion] -> Tablero -> Tablero
invertirTodas = flip $ foldr invertir
    where
        invertir :: Posicion -> Tablero -> Tablero
        invertir p (T f) = T f'
            where
                f' :: Posicion -> Maybe Color
                f' q =  (if q == p then fmap cambiarColor else id) $ f q