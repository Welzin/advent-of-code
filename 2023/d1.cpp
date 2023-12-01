#include "utils.hpp"

using namespace std;

struct PositionPair {
  int pos;
  int val;
};

pair<PositionPair,PositionPair> findFirstAndLastNum(const string &s) {
  vector<size_t> indices = utils::filter_index<string,char>(s, [](char c) -> bool { 
    int alt = c - '0'; return alt >= 0 && alt < 10;
  });
  size_t last = indices.size() - 1;
  return make_pair((PositionPair){.pos = indices[0], .val = s[indices[0]] - '0'}, 
		   (PositionPair){.pos = indices[last], .val = s[indices[last]] - '0'});
}

int convertFirstAndLast(const pair<PositionPair, PositionPair>& p) {
  return p.first.val * 10 + p.second.val;
}

void part1(const vector<string> &input) {
  int total = 0;
  for (auto s : input) {
    auto result = findFirstAndLastNum(s);
    total += convertFirstAndLast(result);
  }
  cout << "Part1: " << total << "\n";
  assert(total == 55029);
}

PositionPair mapWord(const string &s) {
  map<string, int> map_ = {
      {"one", 1}, {"two", 2},   {"three", 3}, {"four", 4}, {"five", 5},
      {"six", 6}, {"seven", 7}, {"eight", 8}, {"nine", 9},
  };
  for (int i = s.size() - 1; i >= 0; --i) {
    string alt = s.substr(i, s.length() - i);
    if (map_.find(alt) != map_.end()) {
      return (PositionPair) { .pos = i, .val = map_[alt] };
    }
  }
  return (PositionPair) { .pos = -1, .val = -1 };
}

pair<PositionPair,PositionPair> findFirstAndLastString(const string& s) {
  string w = "";
  PositionPair first = (PositionPair){.pos = s.size(), .val=-1};
  PositionPair second = (PositionPair) {.pos = -1, .val=-1};
  for (char c : s) {
    w += c;
    auto p = mapWord(w);
    if (p.val != -1) {
      if (first.val == -1) first = p;
      second = p;
    }
  }
  return make_pair(first, second);
}

void part2(const vector<string> &input) {
  int total = 0;
  for (auto s : input) {
    auto numbers = findFirstAndLastNum(s);
    auto instrings = findFirstAndLastString(s);
    int fst = numbers.first.pos < instrings.first.pos ? numbers.first.val : instrings.first.val;
    int snd = numbers.second.pos > instrings.second.pos ? numbers.second.val :
  instrings.second.val;
    total += fst*10 + snd;
  }
  cout << "Part2: " << total << "\n";
  assert(total == 55686);
}

int main() {
  vector<string> inputs = utils::input();
  part1(inputs);
  part2(inputs);
}
