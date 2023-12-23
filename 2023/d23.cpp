#include "utils.hpp"

int dx[] = { -1, 0, 1, 0 };
int dy[] = { 0, -1, 0, 1 };
int h; int w;
using Grid = const vector<string> &;

struct hash_pair {
  template <class T1, class T2>
  size_t operator()(const pair<T1, T2>& p) const {
    auto hash1 = hash<T1>{}(p.first);
    auto hash2 = hash<T2>{}(p.second);
 
    if (hash1 != hash2) {
      return hash1 ^ hash2;              
    }
         
    return hash1;
  }
};

vector<utils::PI> points;
map<utils::PI, vector<pair<int, utils::PI>>> graph;

size_t countNeighbours(Grid grid, int x, int y) {
  size_t count = 0;
  for (int i = 0; i < 4; ++i) {
    int a = x + dx[i];
    int b = y + dy[i];
    count += (a >= 0 && a < h && b >= 0 && b < w && grid[a][b] != '#');
  }
  return count;
}

map<utils::PI, utils::Int<INT_MIN>> DFS(Grid grid, utils::LL x, utils::LL y, int currentDist,
			unordered_map<utils::PI, utils::Bool, hash_pair> &seen) {
  auto p = make_pair(x, y);
  map<utils::PI, utils::Int<INT_MIN>> pointsFound;
  if (seen[p] || !(x >= 0 && x < h && y >= 0 && y < w) || grid[x][y] == '#') return {};
  if (find(points.begin(), points.end(), p) != points.end()) {
    pointsFound[p] = currentDist;
    return pointsFound;
  }
  seen[p] = true;
  
  for (int i = 0; i < 4; ++i) {
    auto aux = DFS(grid, x + dx[i], y + dy[i], currentDist + 1, seen);
    for (const auto &p : aux) {
      if (p.second.n > pointsFound[p.first].n) pointsFound[p.first] = p.second;
    }
  }
  return pointsFound;
}

void pretreatment(Grid grid) {
  for (int i = 0; i < h; ++i) {
    for (int j = 0; j < w; ++j) {
      if (grid[i][j] != '#' && countNeighbours(grid, i, j) > 2) {
	points.push_back(make_pair(i, j));
      }
    }
  }

  points.push_back(make_pair(0, 1));
  points.push_back(make_pair(h - 1, w - 2));
  
  for (auto point : points) {
    unordered_map<utils::PI, utils::Bool, hash_pair> seen;
    seen[point] = true;
    for (int i = 0; i < 4; ++i) {
      auto pointsFound = DFS(grid, point.first + dx[i], point.second + dy[i], 1, seen);
      for (auto [p, d] : pointsFound) {
	graph[point].push_back(make_pair(d, p));
      }
    }
  }
}

size_t DFS(Grid grid, const utils::PI &p, size_t currentDist,
           unordered_map<utils::PI, utils::Bool, hash_pair> seen) {
  if (!(p.first >= 0 && p.first < h && p.second >= 0 && p.second < w)) return 0;
  if (grid[p.first][p.second] == '#') return 0;
  if (p.first == h - 1 && p.second == w - 2) return currentDist;
  if (seen[p]) return 0;
  seen[p] = true;
  
  switch (grid[p.first][p.second]) {
  case '^':
    return DFS(grid, make_pair(p.first - 1, p.second), currentDist + 1, seen);
  case '>':
    return DFS(grid, make_pair(p.first, p.second + 1), currentDist + 1, seen);
  case 'v':
    return DFS(grid, make_pair(p.first + 1, p.second), currentDist + 1, seen);
  case '<':
    return DFS(grid, make_pair(p.first, p.second - 1), currentDist + 1, seen);
  default: {
    size_t m = 0;
    for (int i = 0; i < 4; ++i) {
      m = max(m, DFS(grid, make_pair(p.first + dx[i], p.second + dy[i]),
		     currentDist + 1, seen));
    }
    return m;
  }
  }
}

size_t run(Grid grid, int x, int y) {
  unordered_map<utils::PI, utils::Bool, hash_pair> seen;
  return DFS(grid, make_pair(x, y), 0, seen);
}

void part1(Grid grid) {
  cout << run(grid, 0, 1) << "\n";
}

size_t DFS(const utils::PI &p, size_t currentDist, unordered_map<utils::PI, utils::Bool, hash_pair> seen) {
  if (seen[p]) return 0;
  if (p.first == h - 1 && p.second == w - 2) return currentDist;
  seen[p] = true;

  size_t m = 0;
  for (auto neigh : graph[p]) {
    m = max(DFS(neigh.second, currentDist + neigh.first, seen), m);
  }
  return m;
}

void part2() {
  unordered_map<utils::PI, utils::Bool, hash_pair> seen;
  cout << DFS(make_pair(0, 1), 0, seen) << "\n";
}

int main() {
  auto inputs = utils::input();
  h = inputs.size(); w = inputs[0].size();
  pretreatment(inputs);
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2();
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
