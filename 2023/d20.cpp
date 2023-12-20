#include "utils.hpp"

struct DefaultToMinusOne {
  size_t i = (size_t)-1;
  operator size_t() { return i; }
  void operator =(size_t oth) { i = oth; }
};
map<string, DefaultToMinusOne> pointMapper;

enum class Pulse {
  Low,
  High,
  None,
};

string pulseToString(Pulse pulse) {
  if (pulse == Pulse::Low) return "Pulse::Low";
  else if (pulse == Pulse::High) return "Pulse::High";
  else return "Pulse::None";
}

class Node {
public:
  virtual void receive(Pulse, size_t) = 0;
  virtual Pulse send() = 0;
  virtual void reset() = 0;
};

class FlipFlop : public Node {
private:
  bool _status;
  bool _send;

  Pulse getPulseToSend() {
    return _status ? Pulse::High : Pulse::Low;
  }
public:
  FlipFlop() : _status(false), _send(false) {};

  void receive(Pulse pulse, size_t) {
    if (pulse == Pulse::Low) {
      _status = !_status;
      _send = true;
    }
  }

  Pulse send() {
    if (_send) {
      _send = false;
      return getPulseToSend();
    }
    return Pulse::None;
  }
  
  void reset() { _status = false; _send = false; }
};

class Conjunction : public Node {
private:
  map<size_t, Pulse> _predecessorsPulses;

public:
  Conjunction() : _predecessorsPulses() {};

  void initPred(const vector<size_t> &preds) {
    for (size_t i : preds)
      _predecessorsPulses[i] = Pulse::Low;
  }

  void receive(Pulse pulse, size_t idx) {
    if (pulse != Pulse::None) {
      _predecessorsPulses[idx] = pulse;
    }
  }

  Pulse send() {
    return all_of(_predecessorsPulses.begin(), _predecessorsPulses.end(),
                  [](pair<size_t, Pulse> p) { return p.second == Pulse::High; })
               ? Pulse::Low
               : Pulse::High;
  }

  void reset() {
    for (auto &p : _predecessorsPulses) p.second = Pulse::Low;
  }

  map<size_t, Pulse> predecessorsPulses() { return _predecessorsPulses; }
};

class Broadcast : public Node {
private:
  Pulse _lastReceived;

public:
  Broadcast() : _lastReceived(Pulse::None) {}

  void receive(Pulse pulse, size_t) {
    if (pulse != Pulse::None) _lastReceived = pulse;
  }

  Pulse send() { return _lastReceived; }

  void reset() { _lastReceived = Pulse::None; }
};

class EmptyNode : public Node {
public:
  EmptyNode() {}
  void receive(Pulse, size_t) {}
  Pulse send() { return Pulse::None; }
  void reset() {}
};

vector<unique_ptr<Node>> points;
vector<vector<size_t>> graph;

string getName(string s) {
  if (s[0] == '%' || s[0] == '&') return s.substr(1);
  return s;
}

void parseGraph(const vector<string> &inputs) {
  map<size_t,vector<size_t>> preds;
  bool isConj[inputs.size()];
  // First sweep to populate points.
  for (auto input : inputs) {
    auto spl = utils::split(input, " -> ");
    string name;
    if (spl[0] == "broadcaster") {
      name = spl[0];
      points.push_back(make_unique<Broadcast>());
    } else if (spl[0][0] == '%') {
      name = spl[0].substr(1);
      points.push_back(make_unique<FlipFlop>());
    } else if (spl[0][0] == '&') {
      name = spl[0].substr(1);
      isConj[points.size()] = true;
      points.push_back(make_unique<Conjunction>());
    }
    pointMapper[name] = points.size() - 1;
  }

  graph.resize(pointMapper.size());
  // Second sweep to populate graph.
  for (size_t i = 0; i < inputs.size(); ++i) {
    auto spl = utils::split(inputs[i], " -> ");
    string name = getName(spl[0]);
    for (auto neighbour : utils::split(spl[1], ", ")) {
      size_t dst = pointMapper[neighbour];
      if (dst == (size_t)-1) {
	pointMapper[neighbour] = points.size();
	points.push_back(make_unique<EmptyNode>());
	dst = pointMapper[neighbour];
      }
      if (isConj[dst]) preds[dst].push_back(pointMapper[name]);
      graph[i].push_back(pointMapper[neighbour]);
    }
  }

  // Third sweep to initialize the conjunction thing
  for (auto p : preds) {
    Conjunction *ptr = dynamic_cast<Conjunction*>(points[p.first].get());
    ptr->initPred(p.second);
  }
}

pair<size_t, size_t> run(size_t &lowPulses, size_t &highPulses, const string &target) {
  size_t lowPulsesSentByTarget = 0, highPulsesSentByTarget = 0;
  queue<pair<size_t, Pulse>> q;
  q.push(make_pair(pointMapper["broadcaster"], Pulse::Low));
  ++lowPulses;
  points[pointMapper["broadcaster"]]->receive(Pulse::Low, 0);
    
  while (!q.empty()) {
    auto [node, pulse] = q.front(); q.pop();
    if (node == pointMapper[target]) {
      if (pulse == Pulse::Low) ++lowPulsesSentByTarget;
      if (pulse == Pulse::High) ++highPulsesSentByTarget;
    }
    if (pulse != Pulse::None) {
      for (auto neighbour : graph[node]) {
	if (pulse == Pulse::Low) ++lowPulses;
	if (pulse == Pulse::High) ++highPulses;
	points[neighbour]->receive(pulse, node);
	q.push(make_pair(neighbour, points[neighbour]->send()));
      }
    }
  }
  return make_pair(lowPulsesSentByTarget, highPulsesSentByTarget);
}

void part1() {
  size_t lowPulses = 0, highPulses = 0;
  int times = 1000;
  while (times--) {
    run(lowPulses, highPulses, "rx");
  }
  cout << lowPulses * highPulses << "\n";
}

void resetPoints() {
  for (size_t i = 0; i < points.size(); ++i) points[i]->reset();
}

size_t checkForCycle(const string &point) {
  resetPoints();
  size_t n = 0;
  vector<pair<size_t, size_t>> record;
  while (true) {
    size_t low, high;
    auto [lo, hi] = run(low, high, point);
    ++n;
    for (size_t i = 0; i < record.size(); ++i) {
      auto [recLo, recHi] = record[i];
      if (lo == recLo && hi == recHi && hi == 1) {
	return n - i - 1;
      }
    }
    record.push_back(make_pair(lo, hi));
  }
}

size_t lcm(const vector<size_t> &v) {
  size_t r = v[0];
  for (size_t i = 1; i < v.size(); ++i) {
    r = (r*v[i])/gcd(r, v[i]);
  }
  return r;
}

void part2() {
  // Check when they cycle "high" only once
  auto vg = checkForCycle("vg");
  auto kp = checkForCycle("kp");
  auto gc = checkForCycle("gc");
  auto tx = checkForCycle("tx");

  cout << lcm({vg, kp, gc, tx}) << "\n";
}

int main() {
  parseGraph(utils::input());
  auto start = chrono::high_resolution_clock::now();
  part1();
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2();
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
