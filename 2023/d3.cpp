#include "utils.hpp"

bool isNumber(char c) {
  int alt = c - '0';
  return alt >= 0 && alt <= 9;
}

pair<int,int> neighbourhoodHasSymbol(const vector<string> &inputs, int i, int ed, int st,
			    function<bool(char)> pred) {
  int h = inputs.size();
  int w = inputs[0].size();

  for (int k = i - 1; k <= i + 1; ++k)
    for (int l = st - 1; l <= ed + 1; ++l)
      if (k >= 0 && k < h && l >= 0 && l < w)
	if (pred(inputs[k][l]))
	  return make_pair(k, l);
  return make_pair(-1, -1);
}

void run(const vector<string> &inputs, function<void(const string&, int, int)> update) {
  for (size_t i = 0; i < inputs.size(); ++i) {
    string word = "";
    for (size_t j = 0; j < inputs[i].size(); ++j) {
      if (isNumber(inputs[i][j])) word += inputs[i][j];
      else if (word.size() != 0) {
	update(word, i, j);
	word = "";
      }
    }
    if (word.size() != 0) update(word, i, inputs[i].size());
  }
}

void part1(const vector<string> &inputs) {
  int total = 0;

  auto addIfSymbol = [&](const string& word, int i, int j) -> void {
    auto p = neighbourhoodHasSymbol(inputs, i, j-1, j-word.size(), [](char c) {
      return !(isNumber(c) || c == '.');
    });
    if (p.first != -1 && p.second != -1) {
      total += stoi(word);
    }
  };

  run(inputs, addIfSymbol);

  cout << total << "\n";
  assert(total == 528799);
}

void part2(const vector<string> &inputs) {
  size_t total = 0;
  // Dynamic allocation is needed as otherwise I can't capture this by reference in the
  // lambda below with g++...
  int **gears = (int**)malloc(inputs.size()*sizeof(int*));
  for (size_t i = 0; i < inputs.size(); ++i) {
    gears[i] = (int*)malloc(inputs[i].size()*sizeof(int));
    for (size_t j = 0; j < inputs[0].size(); ++j) {
      gears[i][j] = -1;
    }
  }

  auto addIfGear = [&](const string &word, int i, int j){
    auto p = neighbourhoodHasSymbol(inputs, i, j-1, j-word.size(), [](char c){return c=='*';});
    if (p.first != -1 && p.second != -1) {
      if (gears[p.first][p.second] != -1) {
	total += (size_t)(gears[p.first][p.second]*stoi(word));
      } else {
	gears[p.first][p.second] = stoi(word);
      }
    }
  };

  run(inputs, addIfGear);
  
  cout << total << "\n";
  assert(total == 84907174);
}

int main() {
  vector<string> inputs = utils::input();
  part1(inputs);
  part2(inputs);
}
