#include "utils.hpp"

using PI = pair<int,int>;

PI directionToInt(char c) {
  switch (c) {
  case 'L': return make_pair(0, -1);
  case 'R': return make_pair(0, 1);
  case 'U': return make_pair(-1, 0);
  case 'D': return make_pair(1, 0);
  }
  return make_pair(0, 0);
}

pair<vector<PI>, int> parse(const vector<string> &inputs, bool part2) {
  vector<PI> points;
  char dirs[] = { 'R', 'D', 'L', 'U' };
  int x = 0, y = 0;
  points.push_back(make_pair(x, y));
  int borderLength = 0;
  for (auto s : inputs) {
    auto spl = utils::split(s, ' ');
    int n; int i; int j;
    if (!part2) {
      auto [a, b] = directionToInt(spl[0][0]);
      i = a; j = b;
      n = stoi(spl[1]);
    } else {
      char dir = dirs[spl[2][spl[2].size()-2] - '0'];
      string oth = spl[2].substr(2, spl[2].size()-4);
      stringstream ss;
      ss << hex << oth;
      ss >> n;
      auto [a, b] = directionToInt(dir);
      i = a; j = b;
    }
    x += n * i;
    y += n * j;
    points.push_back(make_pair(x, y));

    borderLength += n;
  }
  return make_pair(points, borderLength);
}

// stolen from https://codeforces.com/blog/entry/75044
void print(__int128 x) {
    if (x < 0) {
        putchar('-');
        x = -x;
    }
    if (x > 9) print(x / 10);
    putchar(x % 10 + '0');
}

__int128 computeArea(const vector<PI> &points) {
  __int128 area = 0;
  size_t n = points.size();
  for (size_t i = 1; i < n; ++i) {
    auto [x1, y1] = points[i - 1];
    auto [x2, y2] = points[i];
    area += (__int128)(y1 + y2) * (__int128)(x1 - x2);
  }
  return area < 0 ? -area : area;
}

void run(const vector<string> &inputs, bool part2) {
  auto [points, borderLength] = parse(inputs, part2);
  __int128 area = computeArea(points);
  print(((area + borderLength) / 2) + 1); cout << "\n";
}

int main() {
  auto inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  run(inputs, false);
  utils::display_time<chrono::microseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  run(inputs, true);
  utils::display_time<chrono::microseconds>(start, "Part 2 took: ", "us");
}
