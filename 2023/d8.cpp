#include "utils.hpp"

pair<pair<queue<int>, map<string,int>>, vector<vector<string>>> parse(const vector<string> &inputs) {
  string dirs = inputs[0];
  queue<int> realDirs;
  for (char c : dirs) realDirs.push(c == 'R');

  vector<vector<string>> dests;
  map<string, int> mapper;
  for (size_t i = 1; i < inputs.size(); ++i) {
    auto left = utils::split(inputs[i], " = ")[0];
    mapper[left] = i - 1;
  }
  for (size_t i = 1; i < inputs.size(); ++i) {
    auto right = utils::split(inputs[i], " = ")[1];
    auto destinations = utils::split(right, ", ");
    destinations[0].erase(0, 1);
    destinations[1].erase(destinations[1].size() - 1, 1);
    dests.push_back({ destinations[0], destinations[1] });
  }
  return make_pair(make_pair(realDirs, mapper), dests);
}

size_t run(string current, const vector<vector<string>> &graph,
           const map<string, int> &mapper, queue<int> directions,
           function<bool(string)> finish) {
  size_t step = 0;
  while (!finish(current)) {
    int dir = directions.front();
    directions.pop();
    current = graph.at(mapper.at(current)).at(dir);
    directions.push(dir);
    ++step;
  }
  return step;
}

void part1(const vector<string> &inputs) {
  auto [dirsMapper, graph] = parse(inputs);
  auto [directions, mapper] = dirsMapper;
  size_t result = run("AAA", graph, mapper, directions, [](string c) { return c == "ZZZ"; });
  cout << result << "\n";
  assert(result == 21797);
}

size_t lcm(const vector<size_t> &steps) {
  size_t r = steps[0];
  for (size_t i = 1; i < steps.size(); ++i) {
    r = (r*steps[i])/gcd(r, steps[i]);
  }
  return r;
}

void part2(const vector<string> &inputs) {
  auto [dirsMapper, graph] = parse(inputs);
  auto [directions, mapper] = dirsMapper;

  vector<string> current;
  for (const auto &p : mapper) {
    if (p.first[2] == 'A') current.push_back(p.first);
  }

  vector<size_t> steps;
  for (string c : current) {
    steps.push_back(run(c, graph, mapper, directions, [](string c) { return c[2] == 'Z'; }));
  }

  cout << lcm(steps) << "\n";
  assert(lcm(steps) == 23977527174353);
}

int main() {
  vector<string> inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
