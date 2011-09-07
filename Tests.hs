import Tablero
import Othello
import HUnit
import Maybe

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [ 
	"triviales" ~: testsTriviales,
	"tablero" ~: testsTablero
	]

testsTriviales = test [
	1 ~=? 1,
	2 ~=? 2,
	[1,2] ~=? [1,2],
	[1,2] ~~? [2,1]
	]
	
testsTablero = test [
	_N ~=? fromJust (contenido d_4 tableroInicial),
	_N ~=? fromJust (contenido e_5 tableroInicial),
	_B ~=? fromJust (contenido e_4 tableroInicial),
	_B ~=? fromJust (contenido d_5 tableroInicial),
	Nothing ~=? contenido a_1 tableroInicial
	]

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

(~~) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Bool
expected ~~ actual = (sort expected) == (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

-- constantes para que los tests sean más legibles		
_N = Negro
_B = Blanco
a_1 = ('a',1::Int)
b_1 = ('b',1::Int)
c_1 = ('c',1::Int)
d_1 = ('d',1::Int)
e_1 = ('e',1::Int)
f_1 = ('f',1::Int)
g_1 = ('g',1::Int)
h_1 = ('h',1::Int)

a_2 = ('a',2::Int)
b_2 = ('b',2::Int)
c_2 = ('c',2::Int)
d_2 = ('d',2::Int)
e_2 = ('e',2::Int)
f_2 = ('f',2::Int)
g_2 = ('g',2::Int)
h_2 = ('h',2::Int)

a_3 = ('a',3::Int)
b_3 = ('b',3::Int)
c_3 = ('c',3::Int)
d_3 = ('d',3::Int)
e_3 = ('e',3::Int)
f_3 = ('f',3::Int)
g_3 = ('g',3::Int)
h_3 = ('h',3::Int)

a_4 = ('a',4::Int)
b_4 = ('b',4::Int)
c_4 = ('c',4::Int)
d_4 = ('d',4::Int)
e_4 = ('e',4::Int)
f_4 = ('f',4::Int)
g_4 = ('g',4::Int)
h_4 = ('h',4::Int)

a_5 = ('a',5::Int)
b_5 = ('b',5::Int)
c_5 = ('c',5::Int)
d_5 = ('d',5::Int)
e_5 = ('e',5::Int)
f_5 = ('f',5::Int)
g_5 = ('g',5::Int)
h_5 = ('h',5::Int)

a_6 = ('a',6::Int)
b_6 = ('b',6::Int)
c_6 = ('c',6::Int)
d_6 = ('d',6::Int)
e_6 = ('e',6::Int)
f_6 = ('f',6::Int)
g_6 = ('g',6::Int)
h_6 = ('h',6::Int)

a_7 = ('a',7::Int)
b_7 = ('b',7::Int)
c_7 = ('c',7::Int)
d_7 = ('d',7::Int)
e_7 = ('e',7::Int)
f_7 = ('f',7::Int)
g_7 = ('g',7::Int)
h_7 = ('h',7::Int)

a_8 = ('a',8::Int)
b_8 = ('b',8::Int)
c_8 = ('c',8::Int)
d_8 = ('d',8::Int)
e_8 = ('e',8::Int)
f_8 = ('f',8::Int)
g_8 = ('g',8::Int)
h_8 = ('h',8::Int)