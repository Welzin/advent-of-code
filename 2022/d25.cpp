#include "utils.hpp"

int64_t base5toi(string s) {
    int64_t result = 0;
    for (size_t i = 0; i < s.size(); ++i) {
        int64_t value = 0;
        switch (s[i]) {
        case '1':
            value = 1;
            break;
        case '2':
            value = 2;
            break;
        case '-':
            value = -1;
            break;
        case '=':
            value = -2;
            break;
        }
        result += value * pow(5, s.size() - (i + 1));
    }
    return result;
}

int64_t parse(const vector<string> &lines) {
    int64_t total = 0;
    for (auto line : lines) {
        total += base5toi(line);
    }
    return total;
}

string part1(int64_t decimal) {
    string result = "";
    while (decimal) {
        auto [q, r] = div(decimal + 2, (int64_t)5);
        result.insert(0, string(1, "=-012"[r]));
        decimal = q;
    }
    return result;
}

int main() {
    auto in = parse(input());
    cout << "Part 1 answer: " << part1(in) << "\n";
}