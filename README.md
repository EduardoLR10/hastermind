# Hastermind

Hastermind, the mastermind game written in Haskell, was created by <a href= https://github.com/EduardoLR10> Eduardo Lemos Rocha</a>, <a href=https://github.com/Nothnbutthefacts>Nothing But the Facts</a>, and <a href=https://github.com/blockthrasher>Justin Cummins, A.K.A Blockthrasher</a>, as the final haskell project for <a href=https://education.emurgo.io/>Emurgo Accademy</a>, and the first <a href=https://www.dapp360.io/>DApp360 Blockchain Developer Workforce</a> Cohort.
It follows typcial <a href=https://en.wikipedia.org/wiki/Mastermind_(board_game)>Mastermind</a> gamplay. 

<h2>Hastermind Gameplay</h2>

In Hastermind a Master (player) creates a secret code (a series of colors). <br>
The Master can chose how many colors the secret will have and how many rounds the players will be allowed to guess the secred code. 
Players then try to guess the colors (in order) or the secret code. The Master then marks Black for correct collor in correct position, White for correct color in incorrect position and None, for neiter correct color or position. 
If a player guesses all correct colors in the correct positions in the number of alloted rounds they win the game. 
Otherwise, the Master wins.

<h2>How to play</h2>

1. Select number of players. </br>
2. Select number of rounds.</br>
3. Select number of players. </br>
4. Enter player names.</br>
5. Enter Code Master name.</br>
6. Mastermind (player) must enter the secret code, and how many colors will be available.</br>
7. Player One must enter first guess. </br>
8. Mastermind enters feedback on Player One's guess. Enter Black for: One token is both correct color, and in right position. Enter White for: One token is correct color, but wrong position. </br>
9. Next player enter guess. </br>
10. Mastermind evaluates. </br>
11. Continue as above untill a player wins or runs out of attempts. 
