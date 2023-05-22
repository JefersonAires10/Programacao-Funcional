-- IDENTIFICAÇÃO

atividade = 7

nome = "Jeferson Aires de Sousa"

matricula = "540428"

-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]


-- IMPLEMENTAR

-- Instância de Show que permite 
-- imprimir um polinômio

instance Show Poly where
   show (Poly []) = ""
   show (Poly [c]) = show c
   show (Poly (c:cs)) = showTerm c ++ "x^" ++ show (length cs) ++ " + " ++ show (Poly cs)
     where
       showTerm c
         | c == 0 = ""
         | c == 1 = "x"
         | c == -1 = "-x"
         | otherwise = show c ++ "x"

-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float
avalPoly (Poly []) _ = 0
avalPoly (Poly (c:cs)) x = c + x * avalPoly (Poly cs) x
-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0
