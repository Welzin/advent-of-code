#include "utils.hpp"

vector<size_t> parse(const vector<string> &in) {
    size_t current = 0;
    stack<size_t> sizes;
    vector<size_t> values;

    auto pop = [&](size_t current) {
	auto next = sizes.top(); sizes.pop();
	values.push_back(current);
	return current + next;
    };
    
    for_each(in.cbegin(), in.cend(), [&](const string &line) {
	auto cmd = split(line, ' ');
	if (cmd[1] == "cd") {
	    if (cmd[2] == "..")
		current = pop(current);
	    else {
		sizes.push(current);
		current = 0;
	    }
	}
	if (cmd[0][0] >= '0' && cmd[0][0] <= '9')
	    current += stoi(cmd[0]);
    });

    while (sizes.size()) current = pop(current);

    return values;
}

size_t part1(const vector<size_t> &sizes) {
    return fold_left(sizes, 0, [](size_t curr, size_t element) {
	return curr + (element <= 100000 ? element : 0);
    });
}

size_t part2(const vector<size_t> &sizes) {
    auto root = max(sizes);
    return min(mapv(sizes, [root](size_t s) {
	return (70000000 - root + s >= 30000000) ? s : (size_t)-1;
    }));
}

int main() {
    auto in = parse(input());
    cout << "Part 1 answer: " << part1(in) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}
