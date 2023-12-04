#include "utils.hpp"

vector<int> getNumbersGeneric(const string &card, int offset) {
  auto nums = utils::split(card, ':')[1];
  auto playing = utils::split(nums, '|')[offset];
  auto playerNumbers = utils::split(playing, ' ');
  vector<int> numbers;
  for (auto number : playerNumbers) {
    if (number.size() != 0) {
      numbers.push_back(stoi(number));
    }
  }
  return numbers;
}

vector<int> getWinningNumbers(const string &card) {
  return getNumbersGeneric(card, 0);
}

vector<int> getNumbers(const string &card) {
  return getNumbersGeneric(card, 1);
}

int getNumberOfWinningNumbers(const string &card) {
  auto winningNumbers = getWinningNumbers(card);
  auto numbers = getNumbers(card);
  int i = 0;
  for (auto number : numbers) {
    i += find(winningNumbers.begin(), winningNumbers.end(), number) != winningNumbers.end();
  }
  return i;
}

void part1(const vector<string> &inputs) {
  int total = 0;
  for (auto card : inputs) {
    int i = getNumberOfWinningNumbers(card) - 1;
    if (i >= 0) total += (1 << i);
  }
  cout << total << "\n";
  assert(total == 23941);
}

void part2(const vector<string> &inputs) {
  int total = 0;
  int copies[inputs.size()];
  for (size_t i = 0; i < inputs.size(); ++i)
    copies[i] = 1;
  
  for (size_t i = 0; i < inputs.size(); ++i) {
    int winning = getNumberOfWinningNumbers(inputs[i]);
    total += copies[i];
    for (int j = 1; j <= winning; ++j) copies[i + j] += copies[i];
  }
  cout << total << "\n";
  assert(total == 5571760);
}

int main() {
  vector<string> inputs = utils::input();
  part1(inputs);
  part2(inputs);
}
