-- IDENTIFICAÇÃO
matricula = "540428" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Jeferson Aires de Sousa" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.
parcialStrip :: [Char] -> [Char]
parcialStrip (x:xs) = if x == ' ' then strip xs else x:xs
parcialStrip xs = xs

strip :: [Char] -> [Char]
strip xs = reverse (parcialStrip (reverse (parcialStrip xs)))

-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs = (takeWhile (/= ' ') xs, dropWhile (/= ' ') xs)
--popWord xs = ("", "") -- implemente aqui


-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

splitStr :: [Char] -> [[Char]]
splitStr xs = if xs == "" then [] else (fst (popWord xs)) : splitStr (strip (snd (popWord xs)))