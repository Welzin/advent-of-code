#include "utils.hpp"

enum Dir {
  N = 0, W, S, E,
};

using PI = pair<int, size_t>;
using Graph = map<int, PI[4]>;
Graph graph;
size_t h;
size_t w;

int getIdx(size_t i, size_t j) {
  return i * w + j;
}

void parse(const vector<string> &inputs) {
  for (size_t i = 0; i < h; ++i) {
    for (size_t j = 0; j < w; ++j) {
      int v = getIdx(i, j);
      if (i > 0) graph[v][Dir::N] = make_pair(getIdx(i - 1, j), inputs[i - 1][j] - '0');
      if (j > 0) graph[v][Dir::W] = make_pair(getIdx(i, j - 1), inputs[i][j - 1] - '0');
      if (i < h - 1) graph[v][Dir::S] = make_pair(getIdx(i + 1, j), inputs[i + 1][j] - '0');
      if (j < w - 1) graph[v][Dir::E] = make_pair(getIdx(i, j + 1), inputs[i][j + 1] - '0');
    }
  }
}

struct PInfos {
  size_t d;
  int v;
  Dir dir;
  size_t k;
  PInfos(int d, int v, Dir dir, int k) : d(d), v(v), dir(dir), k(k) {}
};
bool operator>(const PInfos &a, const PInfos &b) {
  return a.d > b.d || (a.d == b.d && a.v > b.v);
}

bool reverse(Dir a, Dir b) {
  return (a + 2) % 4 == b;
}

size_t dijsktra(size_t minMoves, size_t maxMoves, bool part2) {
  priority_queue<PInfos, vector<PInfos>, greater<PInfos>> P;
  size_t dists[w * h][4][maxMoves+1];
  memset(dists, INT_MAX, sizeof dists);
  P.push(PInfos(0, 0, Dir::N, 0));

  while (!P.empty()) {
    PInfos p = P.top(); P.pop();
    if (dists[p.v][p.dir][p.k] <= p.d) continue;
    if (p.v == getIdx(h - 1, w - 1)) {
      if (!part2 || (part2 && p.k >= minMoves)) return p.d;
    }
    dists[p.v][p.dir][p.k] = p.d;
    for (auto d : { Dir::N, Dir::W, Dir::S, Dir::E }) {
      if (p.v != 0 && reverse(d, p.dir)) continue;
      int u = graph[p.v][d].first; size_t weigth = graph[p.v][d].second;
      size_t alt = p.d + weigth;
      size_t altC = ((p.dir == d) * p.k) + 1;
      bool isValid = false;
      if (!part2 && altC <= maxMoves) isValid = true;
      if (part2 && (p.k >= minMoves || d == p.dir || p.v == 0) && altC <= maxMoves)
	isValid = true;
      if (isValid && dists[u][d][altC] > alt) {
	P.push(PInfos(alt, u, (Dir)d, altC));
      }
    }
  }

  return (size_t)-1;
}

int main() {
  auto inputs = utils::input();
  h = inputs.size(); w = inputs[0].size();
  parse(inputs);
  auto start = chrono::high_resolution_clock::now();
  cout << dijsktra(0, 3, false) << "\n";
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  cout << dijsktra(4, 10, true) << "\n";
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
