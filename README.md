# Greener

This is a Prolog implementation of the game [Greener](https://nestorgames.com/#green_detail), a capturing game for 2 players, where both must capture the same colour.
This implementation is made by the group Greener_5 that consists of

- João Renato da Costa Pinto (up201705547)
- Tiago Gonçalves Gomes (up201806658)

## How to run the game

1. In SICStus Prolog, consult the file 'play.pl' in the src directory(File -> Consult... -> src/play.pl; or in the sicstus terminal: `[play].`)
2. Enter the command `play.`

## Game Rules

**Definitions:**

- A stack is either  one pyramid or several pyramids stacked on top of each other.
- A stack is controlled by the colour of the topmost pyramid. So a ‘Black’ stack is a stack of any height with a black pyramid on top, and so on...

**Rules:**

- The game is made for 2 players (Black and White players)
- The board size is 6x6
- There are 9 Black Pyramids, 9 White and 18 Green
- Randomly place all the pyramids in the board (1 per cell)
- Each player has an allocated colour (Black or White), while green is neutral
- Black always starts and players alternate turns during the game until both players pass in succession
- On your turn you **must** make one capture if possible, otherwise you **pass** the turn
- Stacks capture other stacks that are on the same row or column and with no other stacks in between them; stacks cannot be split
- You can capture stacks of any colour (even your own)
- The game ends when both players pass in succession and wins the player with the most green pyramids captured (being part of stacks they control)
- In case of a tie, the player with the highest stack wins, if the tie persists, play again

[**Rule Book**](https://nestorgames.com/rulebooks/GREENGREENERGREENEST_EN.pdf)

## Game Modes

Our implementation of Greener currently has PvP, PvCPU and CPUvsCPU, where the bots have 3 levels of difficulty, the easy mode, where the bot makes moves randomly, a hard mode where the bot makes the greedy choice, and the dumb mode, where the bot makes the worst move available.

[Game Modes](images/game_modes.png) [Difficulty](images/bots_difficulty.png)



## Game Logic Implementation

### Game State Representation

The board is represented by a list of lists of lists, where the latest is the representation of a stack of pieces. Each piece colour has and associated number(0 for white, 1 for black and 2 for green; 3 for no piece in the cell). The player turn is also represented by a number, similar to the pieces, 0 for white and 1 for black.
 Since the pieces are always in the board there is no more information to be kept.
 Examples of game states in Prolog :

- Initial State


	[	[[2],[0],[2],[1],[0],[2]],</br>
		[[0],[0],[2],[1],[2],[0]],</br>
		[[1],[1],[2],[1],[2],[2]],</br>
		[[1],[2],[2],[2],[0],[2]],</br>
		[[1],[0],[2],[1],[2],[2]],</br>
		[[1],[0],[2],[2],[0],[2]]	] , 1

- Intermediate State

	[	[[1,1,0],[3],[3],[2],[0],[2]],</br>
		[[0],[0],[2],[1],[2],[0]],</br>
		[[1],[1],[2],[1],[2],[2]],</br>
		[[1,2,2,2],[3],[3],[3],[0],[2]],</br>
		[[2],[0],[2],[1],[2],[2]],</br>
		[[1],[0],[2],[2],[0],[2]]	] , 0
        
- Final State

	[	[[1,1,0,2,0,2],[3],[3],[3],[3],[3]],</br>
		[[3],[3],[3],[1,2,0,0,2,0],[3],[3]],</br>
		[[3],[1,1,2,1,2,2],[3],[3],[3],[3]],</br>
		[[3],[3], [1,2,2,2,2,0,2],[3],[3],[3]],</br>
		[[3],[3],[3],[3],[3],[1,2,2,2,0]],</br>
		[[3],[3],[3],[3],[0,2,2,2,0,1],[3]]	] , 0
        
### Game State Visualization

We print a board on screen with letters and numbers to indicate position, and in each cell we represent the color of the head of the list (piece on top of the stack ['W','B','G'], or no piece [  ]) and next to it a number that represents the score associated with the stack ( number of green pieces in the stack)

#### Initial State:

![Initial State](images/initial_board.png)

#### Intermediate State:

![Intermediate State](/images/intermediate_board.png)

#### Final State:

![Final State](images/final_board.png)

### Valid Moves List

When a bot is playing we get all the possible moves, with our **valid_moves** predicate that uses the [findall/3](https://www.swi-prolog.org/pldoc/man?predicate=findall%2f3) predicate of Prolog with a template of  [RowFrom, ColumnFrom, RowTo, ColumnTo] and getting a random move that is a valid move,in other words, where the cell selected has a stack controlled by the current player, if that stack can capture other stacks, if the move is orthogonal and there are no other stacks in between the selected stack and the destination, and if the destinatio is not a empty cell.

(print de codigo necessario??)

### Move Validation

After checking if the current player has any move available, if not, the **turn automatically passes**, if available, we ask the player to select a Piece to move, after that we check if that stack is controlled by the current player and if it has any available moves, if so, we require a destination stack to the player where again we check if the move is valid( if that stack can capture other stacks, if the move is orthogonal and there are no other stacks in between the selected stack and the destination, and if the destinatio is not a empty cell).

### Move Execution

To move a piece to another cell of the board, after its validation, done  we use our **move** predicate, that moves the stack of the origin cell to the destination cell and appends to it the stack that was previously there, clearing the origin cell.