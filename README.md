# PloyBot

This README file contains essential information about a game implemented in Haskell, the 7 main functions of the bot used in the game and how to execute and test the bot.

## Ploy Description

Ploy is an abstract strategy board game for two or four players, played on a 9x9 board with a set of 15 pieces (2-handed) or 9 pieces (4-handed and partnership games) per player. Pieces have various horizontal, vertical or diagonal moves somewhat like chess pieces, except directions of movement are limited; pieces change directions of movement by "rotating". Object of the game is to capture the opponent's Commander (analogous to the king in chess), or all of his other pieces.

## Ploy-webserver

The game can be executed on webserver and you can play against the CPU. The more detailed explanation and some necesssary commands for execution can be found in a specific README.md file in this dir.

## Test&Build

### Test Commands
- Run all tests: ```stack test ```
- Run validation tests: ```stack test ploy:validate ```
- Run unit tests: ```stack test ploy:units ```
- Run grading tests: ```stack test ploy:grading ```
- Run tests with coverage: ```stack test --coverage ... ```

### Building Commands
```shell
- stack build
- stack clean
```

## src

### Function 1: `validateFEN`

This function validates a given Forsyth-Edwards Notation (FEN) representation, checking if it represents a valid board configuration.

### Function 2: `buildBoard`

This function constructs the board using a given FEN representation.

### Function 3: `line`

This function returns the line between two given positions.

### Function 4: `gameFinished`

This function checks whether the game has ended. It considers the number of pieces on the board and specific conditions to determine the result.

### Function 5: `isValidMove`

This function checks whether a given move is valid. It examines the movement of the piece and the other pieces on the board.

### Function 6: `possibleMoves` (not finished yet)

This function returns the possible moves from a given position for a specific piece.

### Function 7: `listMoves` (not finished yet)

This function returns all possible moves for a specific player's current state.


