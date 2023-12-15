#include "utils.hpp"

size_t aochash(const string &s) {
  size_t h = 0;
  for (char c : s) {
    h += c;
    h *= 17;
    h %= 256;
  }
  return h;
}

void part1(const vector<string> &inputs) {
  size_t total = 0;
  for (auto s : inputs) {
    total += aochash(s);
  }
  cout << total << "\n";
}

vector<pair<string,int>> hashmap[256];

void updateMap(int h, const string &key, int val) {
  auto itErase = hashmap[h].end();
  for (auto it = hashmap[h].begin(); it != hashmap[h].end(); ++it) {
    if (it->first == key) {
      if (val == 0) itErase = it;
      else {
        it->second = val;
        return;
      }
    }
  }
  if (itErase != hashmap[h].end()) {
    hashmap[h].erase(itErase);
  } else if (val != 0) {
    hashmap[h].push_back(make_pair(key, val));
  }
}

void part2(const vector<string> &inputs) {
  
  for (auto s : inputs) {
    string v;
    if (s[s.size() - 1] == '-') {
      v = s.substr(0, s.size() - 1);
    } else {
      v = s.substr(0, s.size() - 2);
    }

    if (s[s.size() - 1] == '-') {
      updateMap(aochash(v), v, 0);
    } else {
      updateMap(aochash(v), v, s[s.size() - 1] - '0');
    }
  }
  size_t total = 0;
  for (int i = 0; i < 256; ++i) {
    for (size_t j = 0; j < hashmap[i].size(); ++j) {
      total += (i + 1) * (j + 1) * (hashmap[i][j].second);
    }
  }
  cout << total << "\n";
}

int main() {
  vector<string> inputs = utils::input();
  inputs = utils::split(inputs[0], ',');
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::microseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::microseconds>(start, "Part 2 took: ", "us");
}
