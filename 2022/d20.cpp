#include "utils.hpp"

struct Node {
    Node(int v) : value(v), next(nullptr), prec(nullptr) {}

    int64_t value;
    Node* next;
    Node* prec;
};

struct Problem {
    vector<Node*> order;
    Node* file;
};

Problem parse(const vector<string> &input) {
    Problem p;

    Node *last = nullptr;
    for (const string &s : input) {
        auto node = new Node(stoi(s));
        p.order.push_back(node);
        if (last) {
            last->next = node;
            node->prec = last;
        } else {
            p.file = node;
        }
        last = node;
    }
    last->next = p.file;
    p.file->prec = last;

    return p;
}

void advance(Node *node, int64_t len) {
    while (len--) {
        auto a = node->prec, b = node, c = node->next, d = node->next->next;
        a->next = c;
        b->prec = c;
        b->next = d;
        c->prec = a;
        c->next = b;
        d->prec = b;
    }
}

void mix(Problem &encryptedFile) {
    int64_t vecSize = encryptedFile.order.size();

    for (auto node : encryptedFile.order) {
        auto remainder = (node->value % (vecSize - 1));
        if (remainder < 0) {
            remainder += vecSize - 1;
        }
        advance(node, remainder);
    }
}

vector<int64_t> getResult(const Problem &encryptedFile) {
    // Find value 0:
    Node *node = encryptedFile.file;
    while (node->value != 0) node = node->next;

    vector<int64_t> sum;
    for (int i = 0; i <= 3000; ++i) {
        if (i % 1000 == 0) {
            sum.push_back(node->value);
        }
        node = node->next;
    }
    return sum;
}

int part1(Problem encryptedFile) {
    mix(encryptedFile);
    auto res = getResult(encryptedFile);
    for (Node *n : encryptedFile.order) delete n;
    return sum(res);
}

string part2(Problem encryptedFile) {
    for (auto node : encryptedFile.order) {
        node->value *= 811589153;
    }

    for (int i = 0; i < 10; ++i) {
        mix(encryptedFile);
    }

    string s = "";
    for (auto r : getResult(encryptedFile)) {
        if (r != 0) s += to_string(r) + " ";
    }
    return s;
}

int main() {
    auto in = input();
    cout << "Part 1 answer: " << part1(parse(in)) << "\n";
    cout << "Part 2 answer: " << part2(parse(in)) << "\n";
}