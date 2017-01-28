#Simulation - learning scala the fun way
I'm using this game simulation to teach myself to code in scala.
Check it out if you're new to scala and 
you may find something interesting to you. 
I'm trying to code idiomatically, but i'm not quite there yet.  In fact one thing I've discovered 
is that given how performance dependent the simulations are, I've had to move away
from idiomatic scala in order to achieve performance goals.  while loops rather than
for comprehensions, just as an example.  


The simulation is based on an [IOS game](https://itunes.apple.com/us/app/1010!/id911793120?mt=8) 
that I've wasted too much time playing (thanks, Kevin). So I thought I'd teach 
the computer to play the game and see if I can make it get a higher score than I do 
because it will make fewer mistakes than I make.  (mission accomplished)

The game is kind of a reverse-tetris.  Where three pieces are provided and then
you have to choose which piece to play where - and in which order.  The goal is
to continually clear lines based on piece placement so that as you get more pieces
you can continue to play (that's the tetris-like capability)


Stats:
* right now my own top score playing the IOS game is 24,594  
* simulation top score:  4,475,123!!! (beats the top human score, I've been told)
* long term goal - simulation never loses

Also - feel free to contribute. 

It would be cool if you wrote your own simulation to see if you can beat my top score.

Enjoy

