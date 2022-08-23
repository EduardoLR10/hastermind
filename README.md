# Hastermind

Hastermind, the mastermind game written in Haskell, was created by <a href= https://github.com/EduardoLR10> Eduardo Lemos Rocha</a>, <a href=https://github.com/Nothnbutthefacts>Nothing But the Facts</a>, and <a href=https://github.com/blockthrasher>Justin Cummins, A.K.A Blockthrasher</a>, as the final Haskell project for <a href=https://education.emurgo.io/>Emurgo Academy</a>, and the first <a href=https://www.dapp360.io/>DApp360 Blockchain Developer Workforce</a> Cohort.
It follows typical <a href=https://en.wikipedia.org/wiki/Mastermind_(board_game)>Mastermind</a> gampelay. 

<h2>Hastermind Gameplay</h2>

In Hastermind a Master (player) creates a secret code (a series of colors). </br>
The Master can choose how many colors the secret will have and how many rounds the players will be allowed to guess the secret code. Players then try to guess the colors (in order) or the secret code. The Master then marks Black for the correct color in the correct position, White for the correct color in the incorrect position, and None, for neither correct color nor position. If a player guesses all correct colors in the correct positions in the number of allotted rounds they win the game. Otherwise, the Master wins.

<h2>How to play</h2>

1. Select the number of players.
2. Enter player names.
3. Select which player will be the Master.
4. The Master decides how many colors (slots) the code will have. 
5. the Master must then enter the secret code (colors), and how many rounds will be played.
6. Player One enters first guess.
7. The Master enters feedback on Player One's guess. The Master enters Black for one token is both the correct color and in the right position. White for one token is the correct color but in the wrong position.
8. The next player enters their guess.
9. The Master evaluates.
10. This continues as above until a player wins or runs out of attempts.
