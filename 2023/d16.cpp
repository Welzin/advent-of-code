#include "utils.hpp"

#define N 115

bool seen[N][N][4];
size_t h, w;

enum Dir {
  Up = 0, Down, Left, Right,
};

pair<size_t, size_t> next(size_t x, size_t y, Dir dir) {
  switch (dir) {
  case Dir::Up:
    return make_pair((x == 0 ? x : x - 1), y);
  case Dir::Down:
    return make_pair((x == h - 1 ? x : x + 1), y);
  case Dir::Left:
    return make_pair(x, (y == 0 ? y : y - 1));
  case Dir::Right:
    return make_pair(x, (y == w - 1 ? y : y + 1));
  };
  return make_pair((size_t)-1, (size_t)-1);
}

void run(const vector<string>& field, size_t x, size_t y, Dir dir) {
  if (seen[x][y][dir]) return;
  seen[x][y][dir] = true;
  switch (field[x][y]) {
  case '.': {
    auto [a, b] = next(x, y, dir);
    run(field, a, b, dir);
    break;
  }
  case '/': {
    if (dir == Dir::Right) {
      auto [a, b] = next(x, y, Dir::Up);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Up);
    } else if (dir == Dir::Left) {
      auto [a, b] = next(x, y, Dir::Down);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Down);
    } else if (dir == Dir::Up) {
      auto [a, b] = next(x, y, Dir::Right);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Right);
    } else {
      auto [a, b] = next(x, y, Dir::Left);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Left);
    }
    break;
  }
  case '\\': {
    if (dir == Dir::Right) {
      auto [a, b] = next(x, y, Dir::Down);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Down);
    } else if (dir == Dir::Left) {
      auto [a, b] = next(x, y, Dir::Up);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Up);
    } else if (dir == Dir::Up) {
      auto [a, b] = next(x, y, Dir::Left);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Left);
    } else {
      auto [a, b] = next(x, y, Dir::Right);
      if (a == x && b == y) return;
      run(field, a, b, Dir::Right);
    }
    break;
  }
  case '|': {
    if (dir == Dir::Up || dir == Dir::Down) {
      auto [a, b] = next(x, y, dir);
      run(field, a, b, dir);
    } else {
      auto [a, b] = next(x, y, Dir::Up);
      auto [c, d] = next(x, y, Dir::Down);
      run(field, a, b, Dir::Up);
      run(field, c, d, Dir::Down);
    }
    break;
  }
  case '-': {
    if (dir == Dir::Left || dir == Dir::Right) {
      auto [a, b] = next(x, y, dir);
      run(field, a, b, dir);
    } else {
      auto [a, b] = next(x, y, Dir::Left);
      auto [c, d] = next(x, y, Dir::Right);
      run(field, a, b, Dir::Left);
      run(field, c, d, Dir::Right);
    }
    break;
  }
  }
}

size_t computeTotalEnergised() {
  size_t total = 0;
  for (size_t i = 0; i < h; ++i) {
    for (size_t j = 0; j < w; ++j) {
      bool found = false;
      for (size_t k = 0; k < 4; ++k) {
	found = found || seen[i][j][k];
      }
      total += found;
    }
  }
  return total;
}

void part1(const vector<string> &inputs) {
  run(inputs, 0, 0, Dir::Right);
  cout << computeTotalEnergised() << "\n";
}

size_t runAux(const vector<string> &inputs, size_t i, size_t j, Dir dir) {
  memset(seen, 0, sizeof(seen));
  run(inputs, i, j, dir);
  return computeTotalEnergised();
}

void part2(const vector<string> &inputs) {
  size_t m = runAux(inputs, 0, 0, Dir::Right);
  m = max(runAux(inputs, 0, 0, Dir::Down), m);
  m = max(runAux(inputs, 0, w, Dir::Left), m);
  m = max(runAux(inputs, 0, w, Dir::Down), m);
  for (size_t j = 1; j < w - 1; ++j) {
    m = max(m, runAux(inputs, 0, j, Dir::Down));
    m = max(m, runAux(inputs, h - 1, j, Dir::Up));
  }
  for (size_t i = 1; i < h - 1; ++i) {
    m = max(m, runAux(inputs, i, 0, Dir::Right));
    m = max(m, runAux(inputs, i, w - 1, Dir::Left));
  }
  cout << m << "\n";
}

int main() {
  auto inputs = utils::input();
  h = inputs.size(); w = inputs[0].size();
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::microseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::microseconds>(start, "Part 2 took: ", "us");
}
