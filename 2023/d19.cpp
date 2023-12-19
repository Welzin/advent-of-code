#include "utils.hpp"

using PI = pair<int,int>;

map<string, function<string(int,int,int,int)>> workflows;
map<string,string> workflowsString;

struct XMAS { int x; int m; int a; int s; };

void addWorkflow(const string &line) {
  regex keyConds("([a-z]+)(.+)");
  smatch m;
  regex_match(line, m, keyConds);
  string key = m[1];
  string val = m[2];
  val.erase(val.begin());
  val.erase(prev(val.end()));
  workflowsString[key] = val;
  auto fun = [val](int x, int m, int a, int s) {
    int values[] = { x, m, a, s };
    int i = 0;
    while (val[i + 1] == '>' || val[i + 1] == '<') {
      int toTest = -1;
      switch (val[i]) {
      case 'x': toTest = 0; break;
      case 'm': toTest = 1; break;
      case 'a': toTest = 2; break;
      case 's': toTest = 3; break;
      }
      string next = "";
      int ifTrue = 0;
      for (size_t j = i + 2; j < val.size(); ++j) {
        if (val[j] == ':') {
	  ifTrue = j + 1;
	  break;
        } else {
	  next += string(1, val[j]);
	}
      }
      if (toTest != -1) {
        if ((val[i + 1] == '>' && values[toTest] > stoi(next)) ||
            (val[i + 1] == '<' && values[toTest] < stoi(next))) {
	  next = "";
          for (size_t k = ifTrue; k < val.size(); ++k) {
	    if (val[k] == ',') break;
	    next += string(1, val[k]);
	  }
	  return next;
        } 
      }
      for (size_t j = i; j < val.size(); ++j) {
        if (val[j] == ',') {
	  i = j + 1;
	  break;
	}
      }
    }
    return val.substr(i);
  };
  workflows[key] = fun;
}

XMAS parseXMAS(string line) {
  line.erase(line.begin());
  line.erase(prev(line.end()));
  auto spl = utils::split(line, ',');
  XMAS xmas;
  for (auto s : spl) {
    auto kv = utils::split(s, '=');
    switch (kv[0][0]) {
    case 'x': xmas.x = stoi(kv[1]); break;
    case 'm': xmas.m = stoi(kv[1]); break;
    case 'a': xmas.a = stoi(kv[1]); break;
    case 's': xmas.s = stoi(kv[1]); break;
    }
  }
  return xmas;
}

vector<XMAS> parse() {
  vector<XMAS> lines;
  string line;
  bool isWorkflow = true;
  while (!cin.eof()) {
    getline(cin, line);
    if (!line.size()) isWorkflow = false;
    else if (isWorkflow) {
      addWorkflow(line);
    } else {
      lines.push_back(parseXMAS(line));
    }
  }
  return lines;
}

size_t solve(const XMAS &xmas) {
  string current = "in";
  while (current != "A" && current != "R") {
    current = workflows[current](xmas.x, xmas.m, xmas.a, xmas.s);
  }
  if (current == "A") return xmas.x + xmas.m + xmas.a + xmas.s;
  return 0;
}

void part1(const vector<XMAS> &vxmas) {
  size_t total = 0;
  for (auto xmas : vxmas) {
    total += solve(xmas);
  }
  cout << total << "\n";
}

using Constraint = map<char, PI>;

Constraint make() {
  PI cond = make_pair(0, 4001);
  return {{'x', cond}, {'m', cond}, {'a', cond}, {'s', cond}};
}

Constraint updateConstraint(Constraint c, string cond) {
  int val = stoi(cond.substr(3));
  if (cond[2] != '=') val = stoi(cond.substr(2));
  int a = c[cond[0]].first;
  int b = c[cond[0]].second;
  switch (cond[1]) {
  case '<': {
    if (cond[2] == '=')
      c[cond[0]] = make_pair(a, min(b, val + 1));
    else 
      c[cond[0]] = make_pair(a, min(b, val));
    break;
  }
  case '>': {
    if (cond[2] == '=')
      c[cond[0]] = make_pair(max(a, val - 1), b);
    else 
      c[cond[0]] = make_pair(max(a, val), b);
    break;
  }
  }
  return c;
}

string reverse(string cond) {
  char c = cond[1];
  cond.erase(1, 1);
  switch (c) {
  case '<': cond.insert(1, ">="); return cond;
  case '>': cond.insert(1, "<="); return cond;
  }
  return cond;
}

vector<Constraint> buildPaths(const string &current, Constraint path) {
  if (current == "A") return { path };
  if (current == "R") return {};
  string conds = workflowsString[current];
  auto spl = utils::split(conds, ',');
  vector<Constraint> paths;
  for (size_t i = 0; i < spl.size(); ++i) {
    vector<Constraint> pathsAux;
    if (i == spl.size() - 1) {
      pathsAux = buildPaths(spl[i], path);
    } else {
      auto condKey = utils::split(spl[i], ':');
      pathsAux = buildPaths(condKey[1], updateConstraint(path, condKey[0]));
      path = updateConstraint(path, reverse(condKey[0]));
    }
    paths.insert(paths.end(), pathsAux.begin(), pathsAux.end());
  }
  return paths;
}

void part2() {
  vector<Constraint> pathsToA = buildPaths("in", make());
  size_t total = 0;
  for (auto path : pathsToA) {
    size_t curr = 1;
    for (auto p : path) {
      curr *= p.second.second - 1 - p.second.first;
    }
    total += curr;
  }
  cout << total << "\n";
}

int main() {
  auto vxmas = parse();
  auto start = chrono::high_resolution_clock::now();
  part1(vxmas);
  utils::display_time<chrono::microseconds>(start, "Part 1 took: ", "us");
  start = chrono::high_resolution_clock::now();
  part2();
  utils::display_time<chrono::microseconds>(start, "Part 2 took: ", "us");
}
