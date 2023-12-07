#include "utils.hpp"

bool isNumber(const string &s) {
  return !s.empty() && all_of(s.begin(), s.end(), ::isdigit);
}

pair<vector<int>, vector<int>> parse(const vector<string> &inputs) {
  auto spl0 = utils::split(inputs[0], ' ');
  auto spl1 = utils::split(inputs[1], ' ');
  vector<int> times;
  vector<int> records;
  for (auto s : spl0) {
    if (isNumber(s)) {
      times.push_back(stoi(s));
    }
  }
  for (auto s : spl1) {
    if (isNumber(s)) {
      records.push_back(stoi(s));
    }
  }
  return make_pair(times, records);
}

pair<size_t, size_t> mergeInputs(const vector<string> &inputs) {
  auto spl0 = utils::split(inputs[0], ' ');
  auto spl1 = utils::split(inputs[1], ' ');
  string time;
  string record;
  for (auto s : spl0) {
    if (isNumber(s)) {
      time += s;
    }
  }
  for (auto s : spl1) {
    if (isNumber(s)) {
      record += s;
    }
  }
  return make_pair(stol(time), stol(record));
}

size_t getNumberOfResults(size_t time, size_t record) {
  size_t r1 = (time + sqrt(time * time - 4 * (1 + record))) / 2;
  if (r1 * (time - r1) <= record) r1--;
  size_t r2 = (time - sqrt(time * time - 4 * (1 + record))) / 2;
  if (r2 * (time - r2) <= record) r2++;
  return r1 - r2 + 1;
}

void part1(const vector<string> &inputs) {
  auto [times, records] = parse(inputs);
  size_t result = 1;
  for (size_t i = 0; i < times.size(); ++i) {
    result *= getNumberOfResults(times[i], records[i]);
  }
  cout << result << "\n";
  assert(result == 800280);
}

void part2(const vector<string> &inputs) {
  auto [time, record] = mergeInputs(inputs);
  cout << getNumberOfResults(time, record) << "\n";
  assert(getNumberOfResults(time, record) == 45128024);
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
