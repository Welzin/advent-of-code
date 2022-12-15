#include "utils.hpp"

struct Coord { int x; int y; };
struct Problem { vector<Coord> sensors; vector<Coord> beacons; vector<size_t> dists; };

void coordsFromLine(const string &line, Coord &sensor, Coord &beacon) {
    sensor.x = -1; sensor.y = -1;
    beacon.x = -1; beacon.y = -1;

    regex number(R"(-?[0-9]+)");
    sregex_iterator matches(line.begin(), line.end(), number);
    sregex_iterator end;

    while (matches != end) {
        int coord = stoi((*matches)[0]);
        if (sensor.x == -1) sensor.x = coord;
        else if (sensor.y == -1) sensor.y = coord;
        else if (beacon.x == -1) beacon.x = coord;
        else beacon.y = coord;
        ++matches;
    }
}

size_t dist(const Coord &a, const Coord &b) {
    return abs(a.x - b.x) + abs(a.y - b.y);
}

Problem parse(const vector<string> &lines) {
    Problem problem;
    for (auto line : lines) {
        Coord sensor, beacon;
        coordsFromLine(line, sensor, beacon);
        problem.sensors.push_back(sensor);
        problem.beacons.push_back(beacon);
        problem.dists.push_back(dist(sensor, beacon));
    }

    return problem;
}

size_t part1(const Problem &problem, size_t row) {
    set<int> reachable_cols;
    for (size_t i = 0; i < problem.sensors.size(); ++i) {
        int alt = abs((int)row - problem.sensors[i].y);
        if (alt <= problem.dists[i]) {
            alt = problem.dists[i] - alt;
            for (int j = problem.sensors[i].x - alt; j <= problem.sensors[i].x + alt; ++j) {
                reachable_cols.insert(j);
            }
        }
    }

    for (auto beacon : problem.beacons) {
        if (beacon.y == row && reachable_cols.count(beacon.x)) 
            reachable_cols.erase(beacon.x);
    }

    return reachable_cols.size();
}

using MinMax = pair<int, int>;

size_t part2(const Problem &problem) {
    unordered_map<size_t, vector<MinMax>> minMaxes;
    int totalMax = 4000000;

    for (size_t i = 0; i < problem.sensors.size(); ++i) {
        auto sensor = problem.sensors[i]; auto dist = problem.dists[i];

        minMaxes[sensor.y].push_back(make_pair(sensor.x - dist, sensor.x + dist));
        for (int j = 1; j <= dist; ++j) {
            int alt = dist - j;
            int xmin = max(0, sensor.x - alt);
            int xmax = min(totalMax, sensor.x + alt);
            if (sensor.y - j >= 0) minMaxes[sensor.y - j].push_back(make_pair(xmin, xmax));
            if (sensor.y + j <= totalMax) minMaxes[sensor.y + j].push_back(make_pair(xmin, xmax));
        }
    }

    for (auto p : minMaxes) {
        size_t row = p.first;
        auto mnMxs = p.second;

        sort(mnMxs.begin(), mnMxs.end(), [](auto e1, auto e2) { return e1.first < e2.first; });

        size_t index = 0;
        for (size_t i = 1; i < mnMxs.size(); ++i) {
            if (mnMxs[index].second >= mnMxs[i].first) {
                mnMxs[index].second = max(mnMxs[index].second, mnMxs[i].second);
            } else {
                index++;
                mnMxs[index] = mnMxs[i];
            }
        }

        if (index == 1) {
            return (size_t)(mnMxs[1].first - 1) * 4000000 + row;
        } 
    }

    return 0;
}

int main(int argc, char **argv) {
    auto in = parse(input());
    cout << "Part 1 answer: " << part1(in, stoi(argv[1])) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}