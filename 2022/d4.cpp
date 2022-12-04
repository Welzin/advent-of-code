#include "utils.hpp"

struct Interval {
    Interval() { start = 0; end = 0; }
    Interval(const string &str) {
	auto result = split(str, '-');
	start = stoi(result[0]);
	end = stoi(result[1]);
    }

    inline bool overlaps(const Interval &oth) const { return max(start, oth.start) <= min(end, oth.end); }
    inline bool includes(const Interval &oth) const { return start <= oth.start && oth.end <= end; }

    int start;
    int end;
};

using Assignements = pair<Interval, Interval>;

vector<Assignements> parse(const vector<string> &lines) {
    return mapv(lines, [](const string &line) {
	auto spl = split(line, ',');
	return make_pair(Interval(spl.at(0)), Interval(spl.at(1)));
    });
}

int part1(const vector<Assignements> &intervals) {
    return fold_left(intervals, 0, [](int curr, const Assignements &assignements) {
	auto fst = assignements.first;
	auto snd = assignements.second;
	return curr + (fst.includes(snd) || snd.includes(fst));
    });
}

int part2(const vector<Assignements> &intervals) {
    return fold_left(intervals, 0, [](int curr, const Assignements &assignements) {
	return curr + assignements.first.overlaps(assignements.second);
    });
}

int main() {
    auto in = parse(input());
    cout << "Part 1 answer: " << part1(in) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}

