module Teste where


somatorioElemLista([]) = 0
somatorioElemLista(c:r)
 | c >= 0 = c + somatorioElemLista r
 | otherwise  = somatorioElemLista r

quadradoElemLista([]) = []
quadradoElemLista(c:r)
 | c >= 0 = c^2 : quadradoElemLista r
 | otherwise = quadradoElemLista r

maiorDaLista([],num) = num
maiorDaLista(c:r,num)
 | num < c = maiorDaLista(r,c)
 | otherwise = maiorDaLista(r,num)

multDe3Lista([]) = []
multDe3Lista(c:r)
 | mod c 3 == 0 = c : multDe3Lista r
 | otherwise = multDe3Lista r

produtoPosImpaLista([]) = 1
produtoPosImpaLista([a]) = a
produtoPosImpaLista(a:b:r) = a * produtoPosImpaLista r

somatorioMult5Lista([]) = 0
somatorioMult5Lista(c:r)
 | mod c 5 == 0 = c + somatorioMult5Lista r
 | otherwise = somatorioMult5Lista r

principal(c:r) = do
					putStr "------------------------------------------- \n"
					putStr "|------------Escolha a opção:-------------| \n"
					putStr "------------------------------------------- \n"
					putStr "|1 - Somatorio dos elementos da Lista     | \n"
					putStr "|2 - Quadrado dos elementos da Lista      | \n"
					putStr "|3 - Maior elemento da Lista              | \n"
					putStr "|4 - Múltiplos de 3 da Lista              | \n"
					putStr "|5 - Produto das posições impares da Lista| \n"
					putStr "|6 - Somatorio dos múltiplos de 5 da Lista| \n"
					putStr "|0 - Sair do sistema                      | \n"
					putStr "------------------------------------------- \n"
					n <- getLine
					
					if(read n  == 1)
						then do
								putStr "Somatorio dos elementos da Lista: "
								print (somatorioElemLista(c:r))
								principal(c:r)
					else if(read n == 2)
						then do
								putStr "Quadrado dos elementos da Lista: "
								print (quadradoElemLista(c:r))
								principal(c:r)
					else if(read n == 3)
						then do
								putStr "Maior elemento da Lista: "
								print (maiorDaLista(c:r,0))
								principal(c:r)
					else if(read n == 4)
						then do
								putStr "Múltiplos de 3 da Lista: "
								print (multDe3Lista(c:r))
								principal(c:r)
					else if(read n == 5)
						then do
								putStr "Produto das posições impares da Lista: "
								print (produtoPosImpaLista(c:r))
								principal(c:r)
					else if(read n == 6)
						then do
								putStr "Somatorio dos múltiplos de 5 da Lista: "
								print (somatorioMult5Lista(c:r))
								principal(c:r)
					else if(read n < 0 || read n > 6)
						then do
								putStr "Opção Invalida!!! \n"
								putStr "Tente Novamente!!! \n"
								principal(c:r)					
					else putStr "Saindo do Sistema... \n"
