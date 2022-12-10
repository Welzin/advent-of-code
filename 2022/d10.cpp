#include "utils.hpp"

using Register = unordered_map<int, int>;

Register get_register_state(const vector<string> &lines) {
    Register behaviour; int cycle = 1;
    behaviour[0] = 1; behaviour[1] = 1;
    for_each(lines.begin(), lines.end(), [&](const string &line) {
	auto cmd = split(line, ' ');
	if (cmd[0] == "noop") {
	    behaviour[cycle + 1] = behaviour[cycle];
	} else {
	    behaviour[cycle + 1] = behaviour[cycle];
	    behaviour[cycle + 2] = behaviour[cycle] + stoi(cmd[1]);
	    ++cycle;
	}
	++cycle;
    });
    return behaviour;
}

int part1(const vector<string> &lines) {
    Register behaviour = get_register_state(lines); int result = 0;
    for_each(behaviour.begin(), behaviour.end(), [&](auto p) {
	if (contains({ 20, 60, 100, 140, 180, 220 }, p.first))
	    result += p.first * p.second;
    });
    return result;
}

void part2(const vector<string> &lines) {
    Register behaviour = get_register_state(lines);
    vector<string> CRT(6); int vertical_pos = 0;
    for (size_t i = 0; i < behaviour.size() - 2; ++i) {
	size_t horizontal_pos = behaviour[i + 1];
	size_t real_pos = i % 40;
	vertical_pos = i / 40;
	if (horizontal_pos - 1 <= real_pos && real_pos <= horizontal_pos + 1) {
	    CRT[vertical_pos].append("#");
	} else {
	    CRT[vertical_pos].append(".");
	}
    }

    for (auto line : CRT) {
	cout << line << "\n";
    }
}

int main() {
    auto in = input();
    cout << "Part 1 answer: " << part1(in) << "\n";
    part2(in);
}
