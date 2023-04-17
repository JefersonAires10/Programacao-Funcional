-- ATIVIDADE 
atividade = 4

-- IDENTIFICAÇÃO
matricula = "540428" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Jeferson Aires de Sousa" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Implementar função que receba uma lista
-- ou string de entrada e retorne uma outra 
-- equivalente sem repetiições de elementos,

unique :: Eq s => [s] -> [s]
unique [] = []
unique (x:xs) = x : unique (filter (/=x) xs)


-- Exemplos:

-- >> unique "a1abaa1123b"
-- "a1b23"
-- >> unique [2,1,1,3,3,1,1,3,2
-- [2,1,3]]

-- Obs: (1) Note que a ordem relativa das chaves
-- remanescentes se preserva. (2) Se existir uma função em
-- Haskell que faça a mesma coisa, não deve ser usada. 


-- 2

-- Construa função que remova o valor mínimo de uma lista.
parcialDelete'min :: (Ord a) => [a] -> [a]
parcialDelete'min [] = []
parcialDelete'min (x:xs) = if x == minimum (x:xs) then xs else x : delete'min xs -- implemente aqui

delete'min :: (Ord a) => [a] -> [a]
delete'min xs = parcialDelete'min xs -- implemente aqui

-- Exemplos,

-- >> delete'min [1,3,2,5]
-- [3,2,5]
-- >> delete'min [7,3,2,5,6]
-- [7,3, 5,6]

-- Obs: (1) Se o valor mínimo se repetir
-- então somente a primeira aparição deve 
-- ser removida. (2) Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não deve se utilizada
