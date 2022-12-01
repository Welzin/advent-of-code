#include "utils.hpp"

using namespace std;

vector<int> parse(const vector<string> &in) {
    auto start = in.begin(); auto end = in.end();
    auto sep = start;
    vector<int> parsedInput;
    
    while (sep != end) {
	sep = find(start, end, "");
	parsedInput.push_back(
	    sum(
		convert<int>(vector(start, sep), [](const string &s) -> int { return stoi(s); })
	    )
	);
	start = next(sep);
    }

    return parsedInput;
}

int part1(const std::vector<int> &in) {
    return in.back();
}

int part2(const std::vector<int> &in) {
    auto size = in.size();
    return in.at(size - 1) + in.at(size - 2) + in.at(size - 3);
}

int main() {
    auto in_ = parse(input());
    sort(in_.begin(), in_.end());
    cout << "Part 1 answer: " << part1(in_) << "\n";
    cout << "Part 2 answer: " << part2(in_) << "\n";
}
