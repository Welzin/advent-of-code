#include "utils.hpp"

class Monkey {
public:
    Monkey(const vector<string> &lines) : _itemsChecked(0) {
	_id = stoi(split(lines[0], ' ')[1]); //< "Monkey x:", stoi ignores ':'.
	for (auto num : split(split(lines[1], ':')[1], ',')) //< "Starting items: x, y, z, ..."
	    _items.push(stoi(num));
	auto op = split(lines[2], '=')[1];
	_operation = [op](size_t x) { //< "Operation : new = old ^ y"
	    auto cmd = split(op, ' ');

	    size_t a = 0, b = 0;
	    if (cmd[1] == "old") 
		a = x;
	    else
		a = stoi(cmd[1]);
	    if (cmd[3] == "old")
		b = x;
	    else
		b = stoi(cmd[3]);

	    if (cmd[2] == "+") return a + b;
	    if (cmd[2] == "*") return a * b;

	    return x;
	};
	_divisibleBy = stoi(split(lines[3], ' ')[5]);
	_ifTrue = stoi(split(lines[4], ' ')[9]);
	_ifFalse = stoi(split(lines[5], ' ')[9]);
    }

    size_t itemsChecked() const { return _itemsChecked; }
    int divisibleBy() const { return _divisibleBy; }

    vector<pair<int, size_t>> doRound(int lcm) {
	vector<pair<int, size_t>> throwTo;
	while (_items.size()) {
	    auto item = _items.top(); _items.pop();
	    size_t worryLevel = _operation(item);
	    if (lcm == -1) worryLevel /= 3;
	    else worryLevel %= lcm;
	    if (worryLevel % _divisibleBy == 0)
		throwTo.push_back(make_pair(_ifTrue, worryLevel));
	    else
		throwTo.push_back(make_pair(_ifFalse, worryLevel));
	    _itemsChecked += 1;
	}
	return throwTo;
    }

    void addItem(size_t worry) { _items.push(worry); }

private:
    stack<size_t> _items; 
    int _id;
    function<size_t(size_t)> _operation;
    int _divisibleBy;
    int _ifTrue;
    int _ifFalse;
    size_t _itemsChecked;
};

vector<Monkey> parse(const vector<string> &lines) {
    vector<Monkey> monkeys;
    vector<string> current;
    
    for (auto line : lines) {
	if (line.find("Monkey") != string::npos && current.size() != 0) {
	    monkeys.push_back(Monkey(current));
	    current = vector<string>();
	}	    
	current.push_back(line);
    }
    monkeys.push_back(current);

    return monkeys;
}

pair<int, int> move(vector<Monkey> &monkeys, int n, int lcm = -1) {
    while (n--) {
	for (Monkey &monkey : monkeys) {
	    auto throwTo = monkey.doRound(lcm);
	    for (auto p : throwTo) {
		monkeys[p.first].addItem(p.second);
	    }
	}
    }

    auto itemsChecked = mapv(monkeys, [](const Monkey &monkey) {
	return monkey.itemsChecked();
    });
    sort(itemsChecked.begin(), itemsChecked.end());
    
    auto len = itemsChecked.size();
    return make_pair(itemsChecked[len-1], itemsChecked[len-2]);
}

pair<int, int> part1(vector<Monkey> monkeys) {
    return move(monkeys, 20);
}

int gcd(int x, int y) {
    int tmp;
    while (y) {
	tmp = y;
	y   = x % y;
	x   = tmp;
    }
    return x;
}

pair<int, int> part2(vector<Monkey> monkeys) {
    size_t lcm = 1;
    for (auto monkey : monkeys) lcm *= monkey.divisibleBy();
    lcm /= fold_left(monkeys, monkeys[0].divisibleBy(), [=](int curr, const Monkey &monkey) {
	return gcd(curr, monkey.divisibleBy());
    });
    return move(monkeys, 10000, lcm);
}

int main() {
    auto in = parse(input());
    auto p1 = part1(in);
    auto p2 = part2(in);
    cout << "Part 1 answer: " << p1.first * p1.second << "\n";
    cout << "Part 2 tentative answer: " << p2.first * p2.second << ", part 2 operation: " << p2.first << " * " << p2.second  << "\n";
}
