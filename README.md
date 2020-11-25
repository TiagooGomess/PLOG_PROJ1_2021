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
- The board size is 6x6 or 9x9
- There are 9 Black Pyramids, 9 White and 18 Green for 6x6 or 20,20,41 for 9x9
- Randomly place all the pyramids in the board (1 per cell)
- Each player has an allocated colour (Black or White), while green is neutral
- Black always starts and players alternate turns during the game until both players pass in succession
- On your turn you **must** make one capture if possible, otherwise you **pass** the turn
- Stacks capture other stacks that are on the same row or column and with no other stacks in between them; stacks cannot be split
- You can capture stacks of any colour (even your own)
- The game ends when both players pass in succession and wins the player with the most green pyramids captured (being part of stacks they control)
- In case of a tie, the player with the highest stack wins, if the tie persists, play again

[**Rule Book**](https://nestorgames.com/rulebooks/GREENGREENERGREENEST_EN.pdf)

## Menus

Our Main Menu is composed by the Play option which leads to Game Mode Menu, the How to Play option which gives the Rules to the player and a EXIT option.

![Main Menu](images/main_menu.png)

### Game Modes

Our implementation of Greener currently has PvP, PvCPU and CPUvsCPU, where the bots have 3 levels of difficulty, the easy mode, a hard mode and the dumb mode.
If Computer vs Computer, the player can select all different combinations of bots difficulty, (easy Vs easy, Hard vs Hard, Hard vs Dumb, etc...)
After player type selection, we go to the board Size Menu Where we can choose between 6x6 or 9x9 boards.
For demonstration porposes, or fast game experiences, we also implemented different sleep times for the bots,the player can choose between 0, 0.1 , 0.5, 1, 2 or 5 seconds of sleep.

![Game Modes](images/game_modes.png) ![Difficulty](images/bots_difficulty.png)

![Bot Levels](images/bot_levels.png)

### Play Again

After agame ends we show a menu where we invite the player to play again or to exit

![Play Again](images/play_again.png)

## Game Logic Implementation

### Game State Representation

The board is represented by a list of lists of lists, where the latest is the representation of a stack of pieces. Each piece colour has and associated number(0 for white, 1 for black and 2 for green; 3 for no piece in the cell). The player turn is also represented by a number, similar to the pieces, 0 for white and 1 for black.
 Since the pieces are always in the board there is no more information to be kept.
 Examples of game states in Prolog for 6x6 :

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

Examples for 6x6 boards:

#### Initial State

![Initial State](images/initial_board.png)

#### Intermediate State

![Intermediate State](/images/intermediate_board.png)

#### Final State

![Final State](images/final_board.png)

Example for 9x9:

#### Final State 9x9

![Final State 9x9](images/final_9x9.png)

### Valid Moves List

When a bot is playing we get all the possible moves, with our **valid_moves** predicate that uses the findall/3 predicate of Prolog with a template of  [RowFrom, ColumnFrom, RowTo, ColumnTo] and getting a random move that is a valid move,in other words, where the cell selected has a stack controlled by the current player, if that stack can capture other stacks, if the move is orthogonal and there are no other stacks in between the selected stack and the destination, and if the destinatio is not a empty cell.



### Move Validation

After checking if the current player has any move available, if not, the **turn automatically passes**, if available, we ask the player to select a Piece to move, after that we check if that stack is controlled by the current player and if it has any available moves, if so, we require a destination stack to the player where again we check if the move is valid( if that stack can capture other stacks, if the move is orthogonal and there are no other stacks in between the selected stack and the destination, and if the destinatio is not a empty cell).

### Move Execution

To move a piece to another cell of the board, after its validation, done  we use our **move** predicate, that moves the stack of the origin cell to the destination cell and appends to it the stack that was previously there, clearing the origin cell.

### Game Ending

The game **ends after both players pass the turn successively**, which we check every game loop, then the **game_over** predicate is called where we display the game over message and check the winner of the game, adding the ammount of green pieces on the stacks controlled by each player, using the highest stack if the game was tied, we display the points and ask if the player wants to play another round.

### Board Evaluation

To evaluate the state of the game, how many points each player has, so we can display this information to the players we use the **value** predicate that receives the board and the player and for each row counts the points that player has adding them, in other words, counts the occurrences of green pieces in stacks controlled by the player.
In case of tie, we go check the heigth of the stacks, the player with the highest stack wins or they tie the game if they have the same size.

### Bots Moves

When bots are at play their plays obviosly depend on their difficulty. so we have choose_move that leads receives the mode and behaves accordingly.
The Easy bot, gets all the valid moves available with the valid_moves predicate and simply chooses one at random.
The Hard bot after getting all the valid moves, scores all of them, and chooses the one that gives him the bigger score increase (greedy).
And finally the Dumb bot makes the same process but reverses the scored moves so he chooses the one that gives him the smallest ammount of points possible.

## Conclusion

The development of this game was very interesting for us. At first, it was a very bumpy ride, where we had doubts about pretty much every single feature we tried to add because we are both new to Logic Programming paradigm, but eventually we actually started to enjoy it, and we got to learn a lot of different things as well as getting a introduction to Artificial intelligence, another very interesting subject.
Overall it was a very fun experience with a lot of learning involved!

## Known Issues

There are **no known issues** for the game at this time!

## RoadMap

For future improvements of the game, enhancing the AI of the bots is the way, trying to look for the subsequent plays and scoring them, searching for the most efficient plays rather than only analyzing the immediate play.

## Bibliography

* [Sicstus Manual](https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html/)
* StackOverflow
