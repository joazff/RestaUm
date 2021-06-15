module RestaUm where

import System.IO

type GBoard = [[Char]]
gBoard :: GBoard
gBoard = [[' ',' ','*','*','*',' ',' '],
          [' ',' ','*','*','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          ['*','*','*',' ','*','*','*'],
          ['*','*','*','*','*','*','*'],
          [' ',' ','*','*','*',' ',' '],
          [' ',' ','*','*','*',' ',' ']]

tBoard :: GBoard
tBoard = [[' ',' ','*',' ','*',' ',' '],
          [' ',' ','*',' ','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          ['*',' ','*',' ','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          [' ',' ','*',' ','*',' ',' '],
          [' ',' ','*','*','*',' ',' ']]          
     

-- gArr (get array): recebe uma posicao (p) e uma lista (vetor) e devolve o elemento
-- na posição p do vetor

gArr :: Int -> [t] -> t
gArr p (x:xs) 
 | p == 0 = x
 | otherwise = gArr (p-1) xs 

-- uArr (update array): recebe uma posição (p), um novo valor (v), e uma lista (vetor) e devolve um
-- novo vetor com o valor v na posição p, substituindo o valor que estava nessa posição 

uArr :: Int -> a -> [a] -> [a]
uArr p v [] = []
uArr p v (x:xs)
 | p > 0 = [x] ++ uArr (p-1) v xs
 | p == 0 = [v] ++ uArr (p-1) v xs 
 | otherwise = [x] ++ uArr p v xs

-- Agora, usando as operações anteriores, podemos criar funções para acessar o tabuleiro, como 
-- se ele fosse uma matriz:

-- gPos (get position) recebe linha (l), coluna (c) (não precisa validar) e um tabuleiro. Devolve o elemento na posicao
-- tabuleiro[l,c]. Usar gArr na implementação

gPos :: (Int,Int) -> [[a]] -> a
gPos (a,b) (x:xs) 
 | a == 0 = gArr b x 
 | otherwise = gPos ((a-1),b) xs

-- uPos (update position):  recebe uma posição no tabuleiro (linha e coluna), um novo valor e um tabuleiro. Devolve 
-- o tabuleiro modificado com o novo valor na posiçao l x c

uPos :: (Int,Int) ->  a -> [[a]] -> [[a]] 
uPos (a,b) v (x:xs)
 | a == 0 = uArr b v x : xs
 | otherwise = x : uPos ((a-1),b) v xs 

-- isValidPos: recebe uma linha e uma coluna e retorna um booleano dizendo
-- se essa posição é uma posição válida no tabuleiro. Uma posição válida é uma
-- posição dentro da cruz do tabuleiro de inicio de jogo. por exemplo, (0,0) e (-4,-20) são
-- posições inválidas, (3,3), (6,3) e (3,6) são posições válidas 
-- Exemplos:
-- *RestaUm> isValidPos (3,3)
-- True
-- *RestaUm> isValidPos (-4,20)
-- False

isValidPos :: (Int,Int) -> Bool
isValidPos (a,b) 
 | ((a > 0) && (b > 0)) && ((a <= 7) && (b <= 7)) = True
 | otherwise = False

-- moves: essa função recebe uma linha e uma coluna, e calcula os quatro movimentos possíveis partindo
-- dessa linha e coluna
-- Para uma determinada posição, existem quatro movimentos: para cima, para baixo, esquerda
-- e direita. Cada movimento, envolve duas posições. Por exemplo:
--                              (1,3)
--                              (2,3) 
--                                ^
--                                | 
--               (3,1) (3,2) <- (3,3) -> (3,4) (3,5)
--                                |
--                              (4,3)
--                              (5,3)
-- 
-- *RestaUm> moves 3 3 
-- [[(3,2),(3,1)],[(3,4),(3,5)],[(4,3),(5,3)],[(2,3),(1,3)]]

-- A função moves, não verifica se as coordenadas recebidas ou geradas são válidas, apenas
-- faz o cálculo dos movimentos:
--
-- *RestaUm> moves 0 0
-- [[(0,-1),(0,-2)],[(0,1),(0,2)],[(1,0),(2,0)],[(-1,0),(-2,0)]]
--

moves :: Int -> Int -> [[(Int,Int)]] 
moves a b = [[ (a,(b-1)) , (a,(b-2)) ] , [ (a,(b+1)) , (a,(b+2)) ] , [ ((a+1),b) , ((a+2),b) ] ,  [((a-1),b) , ((a-2),b) ]]

-- isValidPosMove, recebe um movimento (lista de duas posições no tabuleiro), e verifica se as
-- posições nesse movimento são válidas. Aqui dá pra usar duas aplicações da função isValidPos.

-- *RestaUm> isValidPosMove [(4,3),(5,3)]
-- True
-- *RestaUm> isValidPosMove [(0,-1),(0,-2)]
-- False

isValidPosMove :: [(Int,Int)] -> Bool
isValidPosMove (x:xs) = isValidPos x 

-- isEmpty: recebe uma linha, uma coluna e um tabuleiro e diz se a posição correspondente
-- está vazia (contém espaço) ou não. As posições recebidas como entrada são sempre válidas

-- *RestaUm> isEmpty (4,3) gBoard 
-- False
-- *RestaUm> isEmpty (3,3) gBoard 
-- True

isEmpty :: (Int,Int) -> GBoard -> Bool
isEmpty (a,b) xs
 | gPos (a,b) xs ==' '= True
 | otherwise = False

 -- isValidMove: Recebe o tabuleiro corrente do jogo e um movimento (lista com duas posições no tabuleiro)
-- e verifica se o movimento pode ser feito no tabuleiro, ou seja, a primeira posição deve conter uma peça
-- e a segunda deve estar vazia. Aqui pode-se usar a função isEmpty definida anteriormente.
-- Essa função não precisa validar nenhuma das posições.

-- *RestaUm> isValidMove gBoard [(2,3),(3,3)]
-- True
-- *RestaUm> isValidMove gBoard [(2,2),(2,1)]
-- False


isValidMove :: GBoard -> [(Int,Int)] -> Bool 
isValidMove xs [(a,b),(c,d)]
     | (not(isEmpty (a,b) xs) && isEmpty (c,d) xs) && not(a < 2 || a > 4 || b < 2 || b > 4 || c < 2 || c > 4 || d < 2 || d > 4) = True
     | otherwise = False


-- validMoves
-- Recebe um tabuleiro, uma posição e gera uma lista de movimentos válidos dentro do tabuleiro.
-- Essa função pode ser implementada usando moves para gerar todos os movimentos possíveis, logo
-- após, a lista resultante pode ser filtrada, primeiramente usando isValidPosMove, e depois usando
-- isValidMove. Se a posição de entrada da função não for uma posição válida no tabueiro, validMoves
-- retorna uma lista vazia

-- *RestaUm> validMoves gBoard (0,0)
-- []
-- *RestaUm> validMoves gBoard (1,3)
-- [[(2,3),(3,3)]]

validMoves :: GBoard -> (Int, Int) -> [[(Int,Int)]]
validMoves xs (a,b) = filter2 xs (filter1 (moves a b))
     where
          filter1 xs = filter isValidPosMove xs
          filter2 xs ys = filter (isValidMove xs) ys

--- recebe uma posiçao inicial, os movimentos a serem feitos partindo dessa posição,
--- o tabuleiro do jogo, e devolve o tabuleiro modificado com esse movimento, ou seja
-- a posição de partida deve ser esvaziada, a primeira posição do movimento deve
-- ser esvaziada e a segunda posição do movimento deve agora conter uma peça. As posições
-- recebidas e o movimento são sempre válidos
-- 
-- move (1,3) [(2,3),(3,3)] gBoard 
--["  ***  ",
-- "  * *  ",
-- "*** ***",
-- "*******",
-- "*******",
-- "  ***  ",
-- "  ***  "]


move :: (Int,Int) -> [(Int,Int)] -> GBoard -> GBoard    
move (a,b) xs ys = uPos (a,b) ' ' (uPos (head xs) ' ' (uPos (last xs) '*' ys))


-- genTabPositions, recebe um tabuleiro e gera uma matriz com todas as posições
-- desse tabuleiro. O tabuleiro de entrada só é usado para se saber quantas linhas
-- e colunas devem ser geradas. Essa função pode ser quebrada em outras funções, por exemplo,
-- para gerar as linhas

-- *RestaUm> genTabPositions gBoard 
-- [[(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6)],
-- [(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6)],
-- [(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6)],
-- [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6)],
-- [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6)],
-- [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6)],
-- [(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]]
--
--

genTabPositions :: GBoard -> [[(Int,Int)]]
genTabPositions [] = []
genTabPositions xs = genTabHelp xs (0,0) 

genTabHelp :: GBoard -> (Int,Int) -> [[(Int,Int)]]
genTabHelp [] (a,b) = []
genTabHelp (x:xs) (a,b) = [genTabLinhas x (a,b) 1] ++ genTabHelp xs ((a+1),b) 

genTabLinhas :: [Char] -> (Int,Int) -> Int -> [(Int,Int)]
genTabLinhas xs (a,b) z
 | ((length xs) > z) = [(a,b)] ++ genTabLinhas xs ((a),(b+1)) (z+1)
 | otherwise = [(a,b)]  

-- canMove
-- Essa função é usada para vericar se é final de jogo, ou seja se alguma peça ainda pode ser movida
-- Pode ser implementada da seguinte forma: gera o tabuleiro de posições, e para cada uma
-- das posições, tenta aplicar a função validMoves. Se validMoves gerar uma lista vazia
-- para todas as posições do tabuleiro, então significa que é final de jogo, ou seja, 
-- canMove retorna False. Se para pelo menos uma das posições a função validMoves
-- gerar uma sequência de jogo, então canMove retorna true

-- *RestaUm> canMove gBoard 
-- True
-- *RestaUm> canMove tBoard 
-- False

canMove :: GBoard -> Bool
canMove xs 
 | canMoveHelp xs (concatena (genTabPositions xs)) == [] = False
 | otherwise = True  

canMoveHelp :: GBoard -> [(Int,Int)] -> [[(Int,Int)]]
canMoveHelp  ys [] = []
canMoveHelp  ys (x:xs) 
 | (isEmpty x ys == False) = validMoves ys x ++ canMoveHelp ys xs
 | otherwise = canMoveHelp ys xs


concatena :: [[t]]-> [t]
concatena (x:xs) = foldr (++) [] [x] ++ foldr (++) [] xs

-- restaUm, recebe o tabuleiro de jogo e verifica se existe apenas uma peça, ou não
-- 
-- *RestaUm> restaUm gBoard 
-- False

restaUm :: GBoard -> Bool
restaUm xs  
 | (restaUmHelp (concatena xs)) == 1 = True
 | otherwise = False  

restaUmHelp :: [Char] -> Int
restaUmHelp [] = 0
restaUmHelp (x:xs) 
 | (x == '*') = 1 + restaUmHelp xs 
 | otherwise = restaUmHelp xs 

-- Recebe o tabuleiro do jogo e devolve uma string que é a representação visual desse tabuleiro
-- Usar como referencia de implementacao o video sobre tabela de vendas (Aula 06)


printBoard :: GBoard -> String
printBoard [] = "\n \n"
printBoard (x:xs) = ( "\n" ++ x) ++ printBoard xs

main :: IO ()
main = do
   gameLoop gBoard

gameLoop :: GBoard -> IO ()
gameLoop gb = do
   if (not (canMove gb))
   then do
        if (restaUm gb) then do
	                     putStr (printBoard gb)
	                     print "Voce Venceu!!!"
	                else do
			     putStr (printBoard gb)
			     print "Nao existem pecas para mover! Voce perdeu!!"
   else do			
      putStr (printBoard gb)
      print "Qual peca voce quer mover?"
      putStr "Digite uma linha: "
      l <- getLine
      putStr "Digite uma coluna: "
      c <- getLine
      let linha = read l
      let coluna = read c
      if isValidPos (linha,coluna)
      then case validMoves  gb (linha, coluna) of
           []  -> do 
	          print "NAO Eh POSSIVEL MOVER ESTA PECA"
	          gameLoop gb
           [l] ->do
	    --  print (show l)
	        gameLoop (move (linha,coluna) l gb)
           l   -> opcoes linha coluna l gb
      else do
           print "!!!!!POSICAO INVALIDA!!!!!"
	   gameLoop gb

opcoes linha coluna l gb = do
            print "Essa peca pode se mover para:"
	    resp <- printOpcoes 1 l
	    if (resp>=1 && resp <= length l)
	    then gameLoop (move (linha,coluna) (gArr (resp-1) l) gb)
	    else do
	         print "Opcao Invalida!!"
		 opcoes linha coluna l gb

printOpcoes n [] = do
                 putStr "Digite a opção: "
		 op <- getLine
		 let opcao = read op
		 return opcao

printOpcoes n (x:xs) = do
                     print (show n ++ ". "++ (show ((head . tail) x)))
		     printOpcoes (n+1) xs