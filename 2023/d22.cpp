#include "utils.hpp"

struct Cube {
  int x1, y1, z1;
  int x2, y2, z2;
  Cube(int x1, int y1, int z1, int x2, int y2, int z2)
    : x1(min(x1, x2)), y1(min(y1, y2)), z1(min(z1, z2)), x2(max(x1, x2)), y2(max(y1, y2)),
      z2(max(z1, z2)) {}
};
ostream& operator<<(ostream &out, const Cube &c) {
  out << "Cube(" << c.x1 << " " << c.y1 << " " << c.z1 << ", " << c.x2 << " " <<
    c.y2 << " " << c.z2 << ")";
  return out;
}

vector<Cube> cubes;

tuple<int, int, int> mapPoints(const string &s) {
  auto res = utils::mapv(utils::split(s, ','), [](const string&s) { return stoi(s); });
  return make_tuple(res[0], res[1], res[2]);
}
    

void parse(const vector<string> &inputs) {
  cubes.clear();
  for (auto input : inputs) {
    auto sted = utils::split(input, '~');
    auto [x1, y1, z1] = mapPoints(sted[0]);
    auto [x2, y2, z2] = mapPoints(sted[1]);
    cubes.push_back(Cube(x1, y1, z1, x2, y2, z2));
  }
}

bool overlap(int a, int b, int x, int y) {
  return (a <= x && x <= b) || (a <= y && y <= b) ||
    (x <= a && a <= y) || (x <= b && b <= y);
}

bool overlapX(Cube c1, Cube c2) {
  return overlap(c1.x1, c1.x2, c2.x1, c2.x2);
}

bool overlapY(Cube c1, Cube c2) {
  return overlap(c1.y1, c1.y2, c2.y1, c2.y2);
}

bool overlapDown(Cube c1, Cube c2) {
  return overlapX(c1, c2) && overlapY(c1, c2);
}

// Move towards the given direction if possible.
// It should be properly sorted so that we treat only the right ones first.
void fall() {
  for (size_t i = 0; i < cubes.size(); ++i) {
    // Search for one cube that would collide (ground if there are none)
    int z = 1;
    for (int j = i - 1; j >= 0; --j) {
      if (overlapDown(cubes[i], cubes[j])) {
	z = max(cubes[j].z2 + 1, z);
      }
    }
    int diff = max(cubes[i].z1, cubes[i].z2) - min(cubes[i].z1, cubes[i].z2);
    cubes[i].z1 = z;
    cubes[i].z2 = z + diff;
  }
}

size_t countOverlapDown(size_t j) {
  size_t count = 0;
  for (size_t i = 0; i < cubes.size(); ++i) {
    if (i == j || cubes[i].z2 != cubes[j].z1 - 1) continue;
    count += overlapDown(cubes[j], cubes[i]);
  }
  return count;
}

void part1() {
  // Treat the ones that are close to the ground first.
  sort(cubes.begin(), cubes.end(), [](const Cube &c1, const Cube &c2) {
    return c1.z1 < c2.z1;
  });

  // Make them fall down.
  fall();

  sort(cubes.begin(), cubes.end(), [](const Cube &c1, const Cube &c2) {
    return c1.z1 < c2.z1;
  });

  size_t safeFalls = 0;
  for (size_t i = 0; i < cubes.size(); ++i) {
    bool ok = true;
    for (size_t j = 0; j < cubes.size(); ++j) {
      if (j == i || cubes[j].z1 != cubes[i].z2 + 1) continue;
      if (overlapDown(cubes[i], cubes[j]) && countOverlapDown(j) == 1) {
	ok = false;
        break;
      }
    }
    safeFalls += ok;
  }

  cout << safeFalls << "\n";
}

void part2() {
  vector<vector<int>> graph(cubes.size());
  for (size_t i = 0; i < cubes.size(); ++i) {
    for (size_t j = 0; j < cubes.size(); ++j) {
      if (j == i || cubes[i].z1 - 1 != cubes[j].z2) continue;
      if (overlapDown(cubes[i], cubes[j])) {
        graph[i].push_back(j);
      }
    }
  }

  size_t res = 0;

  for (size_t i = 0; i < cubes.size(); ++i) {
    map<int, utils::Bool> moves;
    moves[i] = true;
    for (size_t j = 0; j < cubes.size(); ++j) {
      if (graph[j].size() && all_of(graph[j].begin(), graph[j].end(),
				    [&moves](int k) { return moves[k]; })) {
	moves[j] = true;
	++res;
      }
    }
  }  
  cout << res << "\n";
}

int main() {
  auto inputs = utils::input();
  parse(inputs);
  auto start = chrono::high_resolution_clock::now();
  part1();
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2();
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
