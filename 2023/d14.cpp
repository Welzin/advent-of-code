#include "utils.hpp"

#define N 105

enum class Tile {
  Empty = 0,
  RoundedRock,
  SquareRock,
};

enum class Dir {
  North, South, East, West,
};

Tile field[N][N];
size_t w;
size_t h;

void parse(const vector<string> &inputs) {
  w = inputs[0].size();
  h = inputs.size();
  for (size_t i = 0; i < inputs.size(); ++i) {
    for (size_t j = 0; j < inputs[i].size(); ++j) {
      if (inputs[i][j] == 'O') field[i][j] = Tile::RoundedRock;
      else if (inputs[i][j] == '#') field[i][j] = Tile::SquareRock;
      else field[i][j] = Tile::Empty;
    }
  }
}

void tiltNorth() {
  for (size_t j = 0; j < N; ++j) {
    for (size_t i = 1; i < N; ++i) {
      if (field[i][j] == Tile::RoundedRock) {
	while (i > 0 && field[i - 1][j] == Tile::Empty) {
	  field[i - 1][j] = Tile::RoundedRock;
	  field[i][j] = Tile::Empty;
	  i--;
	}
      }
    }
  }
}

void tiltSouth() {
  for (size_t j = 0; j < N; ++j) {
    for (int i = h - 1; i >= 0; --i) {
      if (field[i][j] == Tile::RoundedRock) {
	while (i < h - 1 && field[i + 1][j] == Tile::Empty) {
	  field[i + 1][j] = Tile::RoundedRock;
	  field[i][j] = Tile::Empty;
	  i++;
	}
      }
    }
  }
}

void tiltWest() {
  for (size_t i = 0; i < N; ++i) {
    for (size_t j = 1; j < N; ++j) {
      if (field[i][j] == Tile::RoundedRock) {
	while (j > 0 && field[i][j - 1] == Tile::Empty) {
	  field[i][j - 1] = Tile::RoundedRock;
	  field[i][j] = Tile::Empty;
	  j--;
	}
      }
    }
  }
}

void tiltEast() {
  for (size_t i = 0; i < N; ++i) {
    for (int j = w - 1; j >= 0; --j) {
      if (field[i][j] == Tile::RoundedRock) {
	while (j < w - 1 && field[i][j + 1] == Tile::Empty) {
	  field[i][j + 1] = Tile::RoundedRock;
	  field[i][j] = Tile::Empty;
	  j++;
	}
      }
    }
  }
}

void tilt(Dir direction) {
  switch (direction) {
  case Dir::North: tiltNorth(); break;
  case Dir::South: tiltSouth(); break;
  case Dir::East: tiltEast(); break;
  case Dir::West: tiltWest(); break;
  }
}

size_t northLoad() {
  size_t total = 0;
  for (size_t i = 0; i < N; ++i) {
    for (size_t j = 0; j < N; ++j) {
      if (field[i][j] == Tile::RoundedRock) {
	total += h - i;
      }
    }
  }
  return total;
}

void part1() {
  tilt(Dir::North);
  cout << northLoad() << "\n";
}

vector<vector<vector<Tile>>> saved;

void record() {
  vector<vector<Tile>> c(h);
  for (size_t i = 0; i < h; ++i) {
    c[i] = vector<Tile>(w);
    for (size_t j = 0; j < w; ++j) {
      c[i][j] = field[i][j];
    }
  }
  saved.push_back(c);
}

size_t appears() {
  for (int k = saved.size() - 1; k >= 0; k--) {
    for (size_t i = 0; i < h; ++i) {
      for (size_t j = 0; j < w; ++j) {
	if (saved[k][i][j] != field[i][j]) goto next;
      }
    }
    return saved.size() - k;
  next:
    ;
  }
  return 0;
}

void part2() {
  size_t t = 1000000000;
  size_t loop = 0;
  size_t i;
  for (i = 0; i < t; ++i) {
    if (i > 0) tilt(Dir::North);
    tilt(Dir::West);
    tilt(Dir::South);
    tilt(Dir::East);
    loop = appears();
    record();
    if (loop > 0) break;
  }
  size_t k = floor((double)(1000000000 - i) / (double)loop);
  for (size_t j = i + loop * k + 1; j < 1000000000; ++j) {
    tilt(Dir::North);
    tilt(Dir::West);
    tilt(Dir::South);
    tilt(Dir::East);
  }
  cout << northLoad() << "\n";
}

int main() {
  vector<string> inputs = utils::input();
  parse(inputs);
  auto start = chrono::high_resolution_clock::now();
  part1();
  utils::display_time<chrono::microseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  part2();
  utils::display_time<chrono::microseconds>(start, "Part 2 took: ", "us");
}
