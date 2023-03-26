-- IDENTIFICAÇÃO
matricula = "540428" -- coloque a matricula aqui entre as aspas

-- Nome
nome = "Jeferson Aires de Sousa" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 2

-- Esta atividade visa construir uma 
-- função que determine os n primeitos números primos

-- Construa as funções a seguir,

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [n | n <- [2..x], x `mod` n == 0] -- mude aqui

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = length (divisores x) == 1 -- mude aqui

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = take n (filter eprimo [2..]) -- mude aqui