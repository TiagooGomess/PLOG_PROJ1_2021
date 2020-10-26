# Greener

This is a Prolog implementation of the game [Greener](https://nestorgames.com/#green_detail), a capturing game for 2 players, where both must capture the same colour.
This implementation is made by the group Greener_5 that consists of

- João Renato da Costa Pinto (up201705547)
- Tiago Gonçalves Gomes (up201705547)

## Game Rules

**Definitions:**

- A stack is either  one pyramid or several pyramids stacked on top of each other.
- A stack is controlled by the colour of the topmost pyramid. So a ‘Black’ stack is a stack of any height with a black pyramid on top, and so on...
- The game is made for 2 players (Black and White players)
- The board size is 6x6
- There are 9 Black Pyramids, 9 White and 18 Green
- Randomly place all the pyramids in the board (1 per cell)
- Each player has an allocated colour (Black or White), while green is neutral
- Black always starts and players alternate turns during the game until both players pass in succession
- On your turn you **must** make one capture if possible, otherwise you **pass** the turn
- Stacks capture other stacks that are on the same row or column and with no other stacks in between them, stacks cannot be split
- You can capture stacks of any colour (even your own)
- The game ends when both players pass in succession and wins the player with the most green pyramids captured (being part of stacks they control)
- In case of a tie, the player with the highest stack wins, if the tie persists, play again

[**Rule Book**](https://nestorgames.com/rulebooks/GREENGREENERGREENEST_EN.pdf)

## Game Implementation

### Game State Representation

The board is represented by a list of lists of lists, where the latest is the representation of a stack of pieces. Each piece colour has and associated number(0 for white, 1 for black and 2 for green). The player turn is also represented by a number, similar to the pieces, 0 for white and 1 for black.
 Since the pieces are always in the board there is no more information to be kept.
 Examples of game states in Prolog :

- Initial State


	[	[[2],[0],[2],[1],[0],[2]],
		[[0],[0],[2],[1],[2],[0]],
		[[1],[1],[2],[1],[2],[2]],
		[[1],[2],[2],[2],[0],[2]],
		[[1],[0],[2],[1],[2],[2]],
		[[1],[0],[2],[2],[0],[2]]	] , 1

- Intermediate State

[	[[1,1,0],[],[],[2],[0],[2]],
		[[0],[0],[2],[1],[2],[0]],
		[[1],[1],[2],[1],[2],[2]],
		[[1,2,2,2],[],[],[],[0],[2]],
		[[2],[0],[2],[1],[2],[2]],
		[[1],[0],[2],[2],[0],[2]]	] , 0
        
- Final State

[	[[1,1,0,2,0,2],[],[],[],[],[]],
		[[],[],[],[1,2,0,0,2,0],[],[]],
		[[],[1,1,2,1,2,2],[],[],[],[]],
		[,[],[], [1,2,2,2,2,0,2],[],[],[]],
		[[],[],[],[],[],[1,2,2,2,0]],
		[[],[],[],[],[0,2,2,2,0,1],[]]	] , 0
        
### Game State Visualization

We print a board on screen with letters and numbers to indicate position, and in each cell we represent the color of the head of the list (piece on top of the stack) and next to it a number that represents the score associated with the stack ( number of green pieces in the stack) ( Não sei se eles querem descrição exata das funções ou só isto)
