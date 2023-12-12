#include "utils.hpp"

#define N 150

using Graph = vector<vector<size_t>>;
using PI = pair<size_t, size_t>;
size_t rowSize;
Graph graph;
vector<size_t> longRows;
vector<size_t> longColumns;

size_t getIdx(size_t i, size_t j) {
  return i * rowSize + j;
}

vector<size_t> parse(const vector<string> &inputs) {
  for (size_t i = 0; i < inputs.size(); ++i) {
     if (all_of(inputs[i].begin(), inputs[i].end(),
               [](char c) { return c == '.'; })) {
       longRows.push_back(i);
    }
  }
  for (int i = inputs[0].size()-1; i >= 0; --i) {
    if (all_of(inputs.begin(), inputs.end(),
               [i](auto s) { return s[i] == '.'; })) {
      longColumns.push_back(i);
    }
  }
  rowSize = inputs[0].size();
  vector<size_t> galaxies;
  for (size_t i = 0; i < inputs.size(); ++i) {
    for (size_t j = 0; j < inputs[i].size(); ++j) {
      vector<size_t> neighbours;
      if (i > 0) neighbours.push_back(getIdx(i-1,j));
      if (j > 0) neighbours.push_back(getIdx(i,j-1));
      if (i < inputs.size()-1) neighbours.push_back(getIdx(i+1,j));
      if (j < inputs[i].size()-1) neighbours.push_back(getIdx(i,j+1));
      if (inputs[i][j] == '#') galaxies.push_back(graph.size());
      graph.push_back(neighbours);
    }
  }
  return galaxies;
}

bool longRowContains(size_t i) {
  return find(longRows.begin(), longRows.end(), i) != longRows.end();
}

bool longColsContains(size_t i) {
  return find(longColumns.begin(), longColumns.end(), i) != longColumns.end();
}

size_t weight(size_t u, size_t v, size_t extra_weigth) {
  size_t a = u / rowSize, b = u % rowSize;
  size_t x = v / rowSize, y = v % rowSize;
  size_t w = 0;
  if (a != x && longRowContains(x)) w += extra_weigth;
  if (b != y && longColsContains(y)) w += extra_weigth;
  if (w == 0) w = 1;
  return w;
}

vector<size_t> dijkstra(size_t s, size_t extra_weigth) {
  priority_queue<PI, vector<PI>, greater<PI>> P;
  vector<size_t> dists(graph.size(), (size_t)-1);
  dists[s] = 0;
  P.push(make_pair(0, s));
  while (!P.empty()) {
    size_t u = P.top().second; P.pop();
    for (auto neigh : graph[u]) {
      size_t alt = dists[u] + weight(u, neigh, extra_weigth);
      if (dists[neigh] > alt) {
        dists[neigh] = alt;
	P.push(make_pair(dists[neigh], neigh));
      }
    }
  }
  return dists;
}

void part1(const vector<size_t> &galaxies) {
  size_t total = 0;
  for (size_t i = 0; i < galaxies.size(); ++i) {
    vector<size_t> dists = dijkstra(galaxies[i], 2);
    for (size_t j = i + 1; j < galaxies.size(); ++j) {
      total += dists[galaxies[j]];
    }
  }
  cout << total << "\n";
}

void part2(const vector<size_t> &galaxies) {
  size_t total = 0;
  for (size_t i = 0; i < galaxies.size(); ++i) {
    vector<size_t> dists = dijkstra(galaxies[i], 1000000);
    for (size_t j = i + 1; j < galaxies.size(); ++j) {
      total += dists[galaxies[j]];
    }
  }
  cout << total << "\n";  
}

int main() {
  vector<string> inputs = utils::input();
  vector<size_t> galaxies = parse(inputs);
  auto start = chrono::high_resolution_clock::now();
  part1(galaxies);
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2(galaxies);
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
