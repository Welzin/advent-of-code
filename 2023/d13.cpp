#include "utils.hpp"

#define N 50

size_t matrix[N][N];

void fillMatrix(const vector<string> &inputs) {
  memset(matrix, 0, sizeof(matrix));
  for (size_t i = 0; i < inputs.size(); ++i) {
    for (size_t j = 0; j < inputs[i].size(); ++j) {
      matrix[i][j] = inputs[i][j] == '#';
    }
  }
}

void transpose() {
  size_t transposed[N][N];
  for (size_t i = 0; i < N; ++i) {
    for (size_t j = 0; j < N; ++j) {
      transposed[i][j] = matrix[j][i];
    }
  }
  for (size_t i = 0; i < N; ++i) {
    for (size_t j = 0; j < N; ++j) {
      matrix[i][j] = transposed[i][j];
    }
  }
}

size_t distance(size_t i, size_t j, size_t m) {
  size_t dist = 0;
  for (size_t k = 0; k < m; ++k) {
    dist += matrix[i][k] != matrix[j][k];
  }
  return dist;
}

size_t countRows(size_t n, size_t m, int expectedReflection) {
  for (size_t i = 0; i < n - 1; ++i) {
    if (distance(i, i + 1, m) <= expectedReflection) {
      int total = distance(i, i + 1, m);
      for (int j = i - 1; j >= 0; --j) {
        int oth = 2 * i - j + 1;
	if (oth < n) total += distance(j, oth, m);
	else break;
      }
      if (total == expectedReflection) return i + 1;
    }
  }
  return -1;
}

size_t run(size_t n, size_t m, int expectedReflection) {
  size_t rows = 0, cols = 0;
  size_t x = countRows(n, m, expectedReflection);
  if (x != (size_t)-1) rows = x;
  transpose();
  size_t y = countRows(m, n, expectedReflection);
  if (y != (size_t)-1) cols = y;
  return rows * 100 + cols;
}

void aoc13(const vector<string> &inputs, int expectedReflection) {
  vector<string> current;
  size_t total = 0;
  for (auto input : inputs) {
    if (input[0] == '@') {
      fillMatrix(current);
      total += run(current.size(), current[0].size(), expectedReflection);
      current.clear();
    } else {
      current.push_back(input);
    }
  }
  cout << total << "\n";
}

int main() {
  vector<string> inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  aoc13(inputs, 0);
  utils::display_time<chrono::microseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  aoc13(inputs, 1);
  utils::display_time<chrono::microseconds>(start, "Part 2 took: ", "us");
}
