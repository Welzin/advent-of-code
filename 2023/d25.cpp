#include "utils.hpp"

vector<string> points;
map<string, vector<string>> graph;
bool dotMode;

void parse(const vector<string> &inputs) {
  set<string> pts;
  map<string, int> pointsToInt;
  for (auto input : inputs) {
    auto spl = utils::split(input, ": ");
    for (auto s : utils::split(spl[1], ' ')) {
      graph[spl[0]].push_back(s);
      graph[s].push_back(spl[0]);
      pts.insert(spl[0]);
      pts.insert(s);
    }
  }
  points = vector<string>(pts.begin(), pts.end());
}

size_t DFS(string s, map<string, utils::Bool> &seen) {
  if (seen[s]) return 0;
  seen[s] = true;

  size_t sum = 1;
  for (auto u : graph[s]) {
    sum += DFS(u, seen);
  }
  return sum;
}

pair<string,string> e1, e2, e3;

void part1() {
  if (dotMode) {
    ofstream out;
    out.open("d25.dot");
    out << "graph d25 {\n";
    for (size_t i = 0; i < points.size(); ++i) {
      out << points[i] << " -- { ";
      for (auto p : graph[points[i]]) {
	out << p << " ";
      }
      out << "}\n";
    }
    out << "}\n";
    out.close();
    system("cluster d25.dot -C2 d25.dot > d25-clustered.dot");
    system("dot -Tpdf -Ksfdp d25-clustered.dot > d25.pdf");
    system("xdg-open d25.pdf");
  } else {
    auto erasingLam = [&](string a, string b) {
      graph[a].erase(find(graph[a].begin(), graph[a].end(), b));
      graph[b].erase(find(graph[b].begin(), graph[b].end(), a));
    };
    erasingLam(e1.first, e1.second);
    erasingLam(e2.first, e2.second);
    erasingLam(e3.first, e3.second);

    map<string, utils::Bool> seen;
    size_t total = 1;
    for (size_t i = 0; i < points.size(); ++i) {
      if (!seen[points[i]]) {
	total *= DFS(points[i], seen);
      }
    }
    cout << total << "\n";
  }
}

int main(int argc, char **argv) {
  dotMode = argv[1][0] == '0';
  if (argc > 2) {
    e1 = make_pair(argv[2], argv[3]);
    e2 = make_pair(argv[4], argv[5]);
    e3 = make_pair(argv[6], argv[7]);
  }
  auto inputs = utils::input();
  parse(inputs);
  auto start = chrono::high_resolution_clock::now();
  part1();
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
}
