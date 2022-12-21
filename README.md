# MCTS

To build:

`stack build`

Might take a while the first time due to some packages needing to be installed.

To run the demo:

`stack exec MCTS-exe <iter> <rollout> <seed>`

The parameters are as follows:
- `<iter>`: the number of MCTS iterations to do per turn
- `<rollout>`: the number of simulations to run at each game tree leaf (run in parallel!)
- `<seed>`: the seed for the random number generator used in simulation