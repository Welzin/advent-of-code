#include "utils.hpp"

bool is_set(const std::string &str, int start, size_t length) {
    set<char> s;
    for_each(next(str.begin(), start), next(str.begin(), start + length),
	     [&s](char c) -> void { s.insert(c); });
    return s.size() == length;
}

int part1(const string &in) {
    for (size_t i = 0; i <= in.size() - 4; ++i)
	if (is_set(in, i, 4)) return i + 4;
    return -1;
}


int part2(const string &in) {
    for (size_t i = 0; i <= in.size() - 14; ++i)
	if (is_set(in, i, 14)) return i + 14;
    return -1;
    
}

int main() {
    auto in = input()[0];
    cout << "Part 1 answer: " << part1(in) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}
