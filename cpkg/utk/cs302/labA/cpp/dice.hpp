#include "main.hpp"

class Dice {
public:
  void add_dice(string characters);
  vector <int> *solve(string word);

private:
  vector <string> dice;
};

