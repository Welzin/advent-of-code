#include "utils.hpp"

vector<int> getSequence(const string &input) {
  return utils::mapv(utils::split(input, ' '), [](const string& s) -> int { return stoi(s); });
}

int run(const vector<int> &sequence, function<int(const vector<int>&,int)> fun) {
  if (all_of(sequence.begin(), sequence.end(), [](int x) { return x == 0; })) {
    return 0;
  }
  vector<int> sequenceBis(sequence.size() - 1);
  for (size_t i = 0; i < sequence.size() - 1; ++i) {
    sequenceBis[i] = sequence[i + 1] - sequence[i];
  }
  return fun(sequence, run(sequenceBis, fun));
}

void part1(const vector<string> &inputs) {
  int total = 0;
  for (const string &input : inputs) {
    vector<int> sequence = getSequence(input);
    total += run(sequence, [](const vector<int> &seq, int res) {
      return seq[seq.size() - 1] + res;
    }); 
  }
  cout << total << "\n";
}

void part2(const vector<string> &inputs) {
  int total = 0;
  for (const string &input : inputs) {
    vector<int> sequence = getSequence(input);
    total += run(sequence, [](const vector<int> &seq, int res) {
      return seq[0] - res;
    });
  }
  cout << total << "\n";
}

int main() {
  vector<string> inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::microseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::microseconds>(start, "Part 2 took: ", "us");
}
