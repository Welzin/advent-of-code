#include "utils.hpp"

enum Shape { Rock, Paper, Scissors };

Shape char_to_shape(char x) {
    if (x == 'X' || x == 'A') return Shape::Rock;
    else if (x == 'Y' || x == 'B') return Shape::Paper;
    else return Shape::Scissors;
}

vector<pair<char, char>> parse(const vector<string> &input) {
    vector<pair<char, char>> strats;
    for (const string &str : input)
	strats.push_back(make_pair(str[0], str[2]));
    return strats;
}

int part1(const vector<pair<char, char>> &in) {
  return sum(mapv(in, [](pair<char, char> p) -> int {
	Shape opponent = char_to_shape(p.first), player = char_to_shape(p.second);
        return ((opponent == player) * 3 + (player == (opponent + 1) % 3) * 6) + (player + 1);
    }));
}

int part2(const vector<pair<char, char>> &in) {
    return sum(mapv(in, [](pair<char, char> p) -> int {
	Shape opponent = char_to_shape(p.first);
	switch (p.second) {
	// As modulo operator doesn't work as intended for negative values, have to a case disjunction.
	case 'X': return (opponent == 0) ? 3 : ((opponent - 1) % 3) + 1;
	case 'Y': return opponent + 4;
	case 'Z': return ((opponent + 1) % 3) + 7;
	default: return 0;
	}
    }));
}

int main() {
    auto in_ = parse(input());
    cout << "Part1 answer: " << part1(in_) << "\n";
    cout << "Part2 answer: " << part2(in_) << "\n";
}
