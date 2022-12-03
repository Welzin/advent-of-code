#include "utils.hpp"

int priority(char c) {
    if (c < 'a') return c - 'A' + 27;
    else return c - 'a' + 1;
}

int part1(const vector<string> &in) {
    return fold_left(in, 0, [](int current, const string &obj) {
	auto cut  = obj.length() / 2;
	auto half = vector(obj.begin(), next(obj.begin(), cut)), other = vector(next(obj.begin(), cut), obj.end());
	auto letter = intersection(half, other)[0];
	return current + priority(letter);
    });
}

int part2(const vector<string> &in) {
    vector<char> transformation;
    for (size_t i = 0; i < in.size() - 2; i += 3) {
	auto s1 = vector(in[i].begin(), in[i].end()), s2 = vector(in[i+1].begin(), in[i+1].end()), s3 = vector(in[i+2].begin(), in[i+2].end());
	transformation.push_back(intersection(intersection(s1, s2), s3)[0]);
    }
    return sum(mapv(transformation, priority));
}

int main() {
    auto in = input();
    cout << "Part 1 answer: " << part1(in) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}
