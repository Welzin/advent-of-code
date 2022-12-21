#include "utils.hpp"

constexpr int64_t UNINITIALISED = numeric_limits<int64_t>::min();
unordered_map<string, int64_t> yells;

unordered_map<string, string> parse(const vector<string> &lines) {
    unordered_map<string, string> missing;
    for (auto line : lines) {
        auto spl = split(line, ": ");

        try {
            yells[spl[0]] = stoll(spl[1]);
        }
        catch (const invalid_argument &) {
            yells[spl[0]] = UNINITIALISED;
            missing[spl[0]] = spl[1];
        }
    }
    return missing;
}

tuple<string, string, char> splitCmd(const string &cmd) {
    string name1 = cmd.substr(0, 4);
    string name2 = cmd.substr(7, 4);
    char op = cmd[5];
    return { name1, name2, op };
}

void compute(const unordered_map<string, string> &computationNeeded, const string &start) {
    if (yells[start] != UNINITIALISED) return;

    auto [name1, name2, op] = splitCmd(computationNeeded.at(start));

    compute(computationNeeded, name1);
    compute(computationNeeded, name2);

    int64_t result = UNINITIALISED;
    switch (op) {
    case '+': 
        result = yells[name1] + yells[name2];
        break;
    case '*': 
        result = yells[name1] * yells[name2];
        break;
    case '-': 
        result = yells[name1] - yells[name2];
        break;
    case '/': 
        result = yells[name1] / yells[name2];
        break;
    }

    yells[start] = result;
}

int64_t part1(const unordered_map<string, string> &computationNeeded) {
    compute(computationNeeded, "root");
    return yells["root"];
}

bool isHumn(const unordered_map<string, string> &computationNeeded, const string &start) {
    if (start == "humn") return true;

    try {
        auto [name1, name2, _] = splitCmd(computationNeeded.at(start));
        return isHumn(computationNeeded, name1) || isHumn(computationNeeded, name2);
    } catch (const out_of_range &) {
        // Pass
    }
    return false;
}

int64_t opInverse(int64_t a, char op, int64_t b, bool name1) {
    switch(op) {
    case '+': return a - b;
    case '-': 
        if (name1) return a + b;
        else return -a + b;
    case '*': return a / b;
    case '/': 
        if (name1) return a * b;
        else return (1/a)*b;
    }
    return UNINITIALISED;
}

int64_t computeInverse(int64_t current, const unordered_map<string, string> &computationNeeded, const string &root) {
    if (root == "humn") return current;

    auto [name1, name2, op] = splitCmd(computationNeeded.at(root));

    if (isHumn(computationNeeded, name1)) {
        return computeInverse(opInverse(current, op, yells[name2], true), computationNeeded, name1);
    } else {
        return computeInverse(opInverse(current, op, yells[name1], false), computationNeeded, name2);
    }
}

int64_t part2(const unordered_map<string, string> &computationNeeded) {
    // Find out which one leads to "humn"
    auto [name1, name2, _2] = splitCmd(computationNeeded.at("root"));

    if (isHumn(computationNeeded, name1)) {
        return computeInverse(yells[name2], computationNeeded, name1);
    } else {
        return computeInverse(yells[name1], computationNeeded, name2);
    }
}

int main() {
    auto in = parse(input());
    cout << "Part 1 answer: " << part1(in) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}