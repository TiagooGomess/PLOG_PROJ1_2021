# PLOG_PROJ1_2021

First Project of PLOG (MIEIC 3rd year / 1st semester)

## Regras do Jogo

![Rules](https://github.com/TiagooGomess/PLOG_PROJ1_2021/blob/main/rules.jpg)

### Feito

* Menu inicial
* How to Play
* Criar tabuleiro com números random (0, 1 ou 2, que simbolizam as pirâmides branca, preta e verde, respetivamente); neste momento, o número de vezes que as peças aparecem no tabuleiro está correto
* Fazer print do tabuleiro (funciona para qualquer matriz n x n)
* ~~Neste momento, o nosso tabuleiro é formado por uma matriz bidimensional 6 x 6 (6 listas de 6 elementos)~~; no entanto, temos que fazer com que seja representado por uma lista de listas de listas: dentro de cada célula da matriz que temos atualmente, teremos que criar uma lista, que representa uma stack de pirâmides; no início, dentro de cada célula da matriz 6 x 6, teremos que ter uma matriz com uma lista com apenas um elemento (0, 1 ou 2); posteriormente, quando forem adicionadas peças a uma stack de pirâmides, adicionamos a representação dessa peça à lista (que ficará na cabeça da lista)
* Com esta alteração da representação do tabuleiro, teremos também que mudar a nossa função printBoard: ~~neste momento, imprime cada célula da matriz 6 x 6~~; no entanto, quando tivermos uma matriz 6 x 6 em que cada célula seja representada por uma lista, não poderemos imprimir a lista toda, pelo que devemos imprimir apenas a cabeça da lista, que representa o jogador que no momento controla a stack (caso a cabeça da lista seja um 0 ou 1)
* Também seria importante que o jogador conseguisse saber quantas pirâmides verdes tem cada stack, para saber se a stack lhe trará mais ou menos pontos, mas acho que não seria muito fácil imprimir isso dentro do tabuleiro de jogo; ~~uma opção seria dar a opção ao jogador para saber qual a composição de cada stack (número de peças verdes, brancas e pretas), sendo perguntado o número da linha e da coluna da stack que quer obter essa informação;~~ **o número de peças verdes em cada stack aparece em frente à 'cor' da stack** 

### A fazer antes da primeira entrega

* Fazer função que retorna o número da pirâmide de uma célula do tabuleira, dada a linha e coluna
* Mudar o nome das funções de acordo com o que está no enunciado
* Implementar o estado do jogo
  - Jogador atual
  - Número de pontos de cada jogador
  - Determinar quando o jogo termina 
* Fazer o README para a primeira entrega

