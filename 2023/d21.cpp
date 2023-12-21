#include "utils.hpp"

using PI = pair<int,int>;
struct DefaultToFalse {
  bool b = false;
  operator bool() { return b; }
  void operator =(bool oth) { b = oth; }
};
struct DefaultToInf {
  int i = INT_MAX;
  operator int() { return i; }
  void operator =(int oth) { i = oth; }
  bool operator !=(int oth) { return i != oth; }
};

PI start(const vector<string> &grid) {
  for (size_t i = 0; i < grid.size(); ++i) {
    for (size_t j = 0; j < grid[i].size(); ++j) {
      if (grid[i][j] == 'S') return make_pair(i, j);
    }
  }
  return make_pair(-1, -1);
}

int dx[] = { 1, 0, -1, 0 };
int dy[] = { 0, 1, 0, -1 };

size_t run(const vector<string> &grid, int x, int y, int steps, bool part2) {
  int n = grid.size();
  int m = grid[0].size();
  queue<PI> q;
  q.push(make_pair(x, y));
  while (steps--) {
    queue<PI> nextQueue;
    map<PI, DefaultToFalse> localSeen;
    while (!q.empty()) {
      auto [i, j] = q.front(); q.pop();
      for (int k = 0; k < 4; ++k) {
	int a = i + dx[k];
	int b = j + dy[k];
	if (!part2 && (a < 0 || a >= n || b < 0 || b >= m)) continue;
	
	if (grid[utils::mod(a, n)][utils::mod(b, m)] != '#') {
	  auto p = make_pair(a, b);
	  if (!localSeen[p]) {
	    localSeen[p] = true;
	    nextQueue.push(p);
	  }
	}
      }
    }
    q = nextQueue;
  }

  return q.size();
}

void part1(const vector<string> &grid) {
  auto [x, y] = start(grid);
  cout << run(grid, x, y, 64, false) << "\n";
}

// Lagrange quadratic interpolation simplified using a0 = 0, a1 = 1 and a2 = 2.
tuple<int,int,int> interpolate(const tuple<double,double,double> &points) {
  int a = get<0>(points) / 2 - get<1>(points) + get<2>(points) / 2;
  int b = (get<0>(points) * -3) / 2 + get<1>(points) * 2 - get<2>(points) / 2;
  int c = get<0>(points);
  return make_tuple(a, b, c);
}

void part2(const vector<string> &grid) {
  size_t k = grid.size() / 2;
  size_t n = grid.size();
  auto [x, y] = start(grid);
  auto points = make_tuple(run(grid, x, y, k, true), run(grid, x, y, k + n, true),
			   run(grid, x, y, k + 2 * n, true));
  auto [a, b, c] = interpolate(points);
  cout << "Interpolated: " << a << "x^2 + " << b << "x + " << c << "\n";
  n = (26501365 / grid.size());
  cout << a*n*n + b*n + c << "\n";
}

int main() {
  auto inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
