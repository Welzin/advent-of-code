#include <bits/stdc++.h>

using namespace std;

/*** Start AoC header ***/
istream& operator>>(istream &is, vector<string> &v) {
    string s; getline(cin, s);
    v.push_back(s);
    return is;
}

vector<string> input() {
    vector<string> lines;
    while (!cin.eof()) {
	cin >> lines;
    }
    return lines;
}

template <typename T, typename U, typename Lambda>
vector<T> convert(const vector<U>& v, Lambda fun) {
    vector<T> res(v.size());
    transform(v.begin(), v.end(), res.begin(), fun);
    return res;
}

int sum(const vector<int> &v) {
    return accumulate(v.begin(), v.end(), 0);
}
/*** End AoC header ***/
