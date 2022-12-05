#include "utils.hpp"

#define N_CRATES 9

using Crate = stack<char>;
struct Move { int n; int from; int to; };
struct Problem { Crate crates[N_CRATES]; vector<Move> moves; };

void get_crates(Problem &problem, size_t &index, const vector<string> &in) {
    vector<char> vecs[N_CRATES];

    auto isCrate = [](const string &line) { return line[0] != 'm'; };
    auto canBeAdded = [](const string &line) { return line[1] != '1'; };
    auto addCrates = [&vecs](const string &line) {
	for (size_t i = 0; i < N_CRATES; ++i) {
	    char c = line[(i * 4) + 1];
	    if (c != ' ') vecs[i].insert(vecs[i].begin(), c);
	}
    };
    
    while (isCrate(in[index])) {
	if (canBeAdded(in[index]))
	    addCrates(in[index]);
	++index;
    }
    for (size_t i = 0; i < N_CRATES; ++i)
	problem.crates[i] = Crate(deque<char>(vecs[i].begin(), vecs[i].end()));
}

void get_moves(Problem &problem, int index, const vector<string> &in) {
    for (size_t i = index; i < in.size(); ++i) {
	auto spl = split(in[i], ' ');
	problem.moves.push_back(Move {
		.n = stoi(spl[1]),
		.from = stoi(spl[3]) - 1,
		.to = stoi(spl[5]) - 1
	    });
    }
}

string crates_to_string(const Problem &problem) {
    string s;
    for (size_t i = 0; i < N_CRATES; ++i)
	s.push_back(problem.crates[i].top());
    return s;
}

Problem parse(const vector<string> &in) {
    size_t index = 0; Problem problem;
    get_crates(problem, index, in);
    get_moves(problem, index, in);
    return problem;
}

string part1(Problem problem) {
    for (Move m : problem.moves) {
	while (m.n--) {
	    problem.crates[m.to].push(problem.crates[m.from].top());
	    problem.crates[m.from].pop();
	}
    }
    return crates_to_string(problem);
}

string part2(Problem problem) {
    for (Move m : problem.moves) {
	vector<char> buffer;
	while (m.n--) {
	    buffer.insert(buffer.begin(), problem.crates[m.from].top());
	    problem.crates[m.from].pop();
	}
	for (char c : buffer) 
	    problem.crates[m.to].push(c);
    }
    return crates_to_string(problem);
}

int main() {
    auto in = parse(input());
    cout << "Answer part 1: " << part1(in) << "\n";
    cout << "Answer part 2: " << part2(in) << "\n";
}
