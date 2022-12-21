#include "utils.hpp"

struct Problem {
    unordered_map<string, size_t> flows;
    unordered_map<string, vector<string>> neighbors;
};

struct State {
    size_t totalPressure;
    string currentValve;
    unordered_map<string, bool> seen;
    size_t valvesOpened;
};

vector<string> getCapitalizedWords(const string &s) {
    regex capitalized(R"([A-Z]{2})");
    sregex_iterator matches(s.cbegin(), s.cend(), capitalized);
    sregex_iterator end;

    vector<string> words;
    while (matches != end) {
        words.push_back((*matches)[0]);
        ++matches;
    }
    return words;
}

pair<string, int> findNameAndFlowRate(const string &s) {
    auto spl = split(s, "=");
    return make_pair(getCapitalizedWords(s)[0], stoll(spl[1]));
}

vector<string> findNeighbors(const string &s) {
    return getCapitalizedWords(s);
}

Problem parse(const vector<string> &lines) {
    Problem p;
    for (auto line : lines) {
        // First part with name + flow rate ; Second part with neighbors
        auto spl = split(line, ";");

        // Find name + flow rate of first line
        auto [name, rate] = findNameAndFlowRate(spl[0]);

        if (rate > 0) p.flows[name] = rate;
        p.neighbors[name] = findNeighbors(spl[1]);
    }
    return p;
}

unordered_map<string, unordered_map<string, size_t>> floydWarshall(const Problem &p) {
    unordered_map<string, unordered_map<string, size_t>> dists;
    for (auto [node, neighbors] : p.neighbors) {
        for (auto [n, _] : p.neighbors) {
            dists[node][n] = numeric_limits<int>::max();
        }
        for (auto neigh : neighbors) {
            dists[node][neigh] = 1;
        }
    }
    for (auto [k, _] : p.neighbors) {
        for (auto [i, _] : p.neighbors) {
            for (auto [j, _] : p.neighbors) {
                dists[i][j] = min(dists[i][j], dists[i][k] + dists[k][j]);
            }
        }
    }
    return dists;
}

unordered_map<string, size_t> createMasks(const Problem &p) {
    unordered_map<string, size_t> masks;
    size_t mask = 0;
    for (auto [x, _] : p.flows) {
        masks[x] = (1 << mask);
        mask++;
    }
    return masks;
}

void visit(
    const string &v, int time, size_t state, size_t totalPressure, unordered_map<size_t, size_t> &answer,
    const unordered_map<string, unordered_map<string, size_t>> &dists, const unordered_map<string, size_t> &masks,
    const Problem &p
    ) {
    size_t current = 0;
    if (answer.find(state) != answer.end()) {
        current = answer[state];
    }
    answer[state] = max(current, totalPressure);

    for (auto [u, f] : p.flows) {
        auto newTime = time - (int)dists.at(v).at(u) - 1;
        // Checks if valve is already opened or if there's no time left.
        if ((masks.at(u) & state) || (newTime <= 0)) continue;
        visit(u, newTime, state | masks.at(u), totalPressure + newTime * f, answer, dists, masks, p);
    }
}

unordered_map<size_t, size_t> compute(const Problem &p, size_t time) {
    auto dists = floydWarshall(p);
    auto masks = createMasks(p);

    unordered_map<size_t, size_t> answer;
    visit("AA", time, 0, 0, answer, dists, masks, p);
    return answer;
}

size_t part1(const Problem &p) {
    auto answer = compute(p, 30);
    size_t mx = 0;
    for (auto [_, val] : answer) {
        mx = max(val, mx);
    }
    return mx;
}

size_t part2(const Problem &p) {
    auto answer = compute(p, 26);

    size_t mx = 0;
    for (auto [s1, v1] : answer) {
        for (auto [s2, v2] : answer) {
            if ((s1 & s2) == 0 && v1 + v2 > mx) mx = v1 + v2;
        }
    }
    return mx;
}

int main() {
    auto in = parse(input());
    cout << "Part 1 answer: " << part1(in) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}