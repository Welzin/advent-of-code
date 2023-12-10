#include "utils.hpp"

#define START 'L'

using Point = pair<size_t,size_t>;
using Graph = map<Point, vector<Point>>;

Point north(size_t i, size_t j) { return make_pair(i - 1, j); }
Point south(size_t i, size_t j) { return make_pair(i + 1, j); }
Point east(size_t i, size_t j) { return make_pair(i, j + 1); }
Point west(size_t i, size_t j) { return make_pair(i, j - 1); }

void setNeighbours(Graph &g, char c, size_t i, size_t j) {
  Point p = make_pair(i, j);
  g[p].clear();
  switch (c) {
  case '|':
    g[p].push_back(north(i, j));
    g[p].push_back(south(i, j));
    break;
  case '-':
    g[p].push_back(east(i, j));
    g[p].push_back(west(i, j));
    break;
  case 'L':
    g[p].push_back(north(i, j));
    g[p].push_back(east(i, j));
    break;
  case 'J':
    g[p].push_back(north(i, j));
    g[p].push_back(west(i, j));
    break;
  case '7':
    g[p].push_back(south(i, j));
    g[p].push_back(west(i, j));
    break;
  case 'F':
    g[p].push_back(south(i, j));
    g[p].push_back(east(i, j));
    break;
  }
}

pair<Graph, Point> parse(const vector<string> &inputs) {
  Point start;
  Graph g;
  for (size_t i = 0; i < inputs.size(); ++i) {
    for (size_t j = 0; j < inputs[i].size(); ++j) {
      if (inputs[i][j] == 'S') start = make_pair(i, j);
      setNeighbours(g, inputs[i][j], i, j);
    }
  }
  return make_pair(g, start);
}

size_t dists[150][150];

size_t BFS(const Graph &graph, const Point &s) {
  queue<Point> q;
  for (size_t i = 0; i < 150; ++i)
      for (size_t j = 0; j < 150; ++j)
        dists[i][j] = (size_t)-1;
  dists[s.first][s.second] = 0;
  q.push(s);
  size_t maxDist = 0;
  while (!q.empty()) {
    Point p = q.front(); q.pop();
    for (auto neigh : graph.at(p)) {
      size_t alt = dists[p.first][p.second];
      if (dists[neigh.first][neigh.second] > alt + 1) {
        dists[neigh.first][neigh.second] = alt + 1;
        if (alt + 1 > maxDist)
          maxDist = alt + 1;
	q.push(neigh);
      }
    }
  }
  return maxDist;
}

void part1(const vector<string> &inputs) {
  auto [graph, start] = parse(inputs);
  setNeighbours(graph, START, start.first, start.second);
  cout << BFS(graph, start) << "\n";
}

void part2(vector<string> inputs) {
  auto [graph, start] = parse(inputs);
  setNeighbours(graph, START, start.first, start.second);

  inputs[start.first][start.second] = START;

  char lastCol = '\0';
  char lastRow = '\0';
  size_t pipesColumns[150][150];
  size_t pipesRows[150][150];
  for (size_t i = 0; i < inputs.size(); ++i) {
    for (size_t j = 0; j < inputs[i].size(); ++j) {
      if (j == 0) {
        pipesColumns[i][j] = (dists[i][j] != (size_t)-1);
	lastCol = inputs[i][j];
      } else {
	if (dists[i][j] == (size_t)-1 || inputs[i][j] == '-' ||
            (inputs[i][j] == '7' && lastCol == 'L') ||
	    (inputs[i][j] == 'J' && lastCol == 'F')) {
          pipesColumns[i][j] = pipesColumns[i][j - 1];
	} else {
          pipesColumns[i][j] = pipesColumns[i][j - 1] + 1;
        }
      	if (inputs[i][j] != '-') lastCol = inputs[i][j];
      }     
    }
  }
  for (size_t i = 0; i < inputs[0].size(); ++i) {
    for (size_t j = 0; j < inputs.size(); ++j) {
      if (j == 0) {
        pipesRows[j][i] = (dists[j][i] != (size_t)-1);
	lastRow = inputs[j][i];
      }
      if (dists[j][i] == (size_t)-1 || inputs[j][i] == '|' ||
	  (inputs[j][i] == 'J' && lastRow == 'F') ||
	  (inputs[j][i] == 'L' && lastRow == '7')) {
	pipesRows[j][i] = pipesRows[j - 1][i];
      } else {
	pipesRows[j][i] = pipesRows[j - 1][i] + 1;
      }
      if (inputs[j][i] != '|') lastRow = inputs[j][i];
    }
  }
  
  size_t total = 0;
  for (size_t i = 0; i < inputs.size(); ++i) {
    for (size_t j = 0; j < inputs[i].size(); ++j) {
      total += (dists[i][j] == (size_t)-1) && (pipesColumns[i][j] & 1) && (pipesRows[i][j] & 1);
    } 
  }
  cout << total << "\n";
}

int main() {
  vector<string> inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "us");
}
