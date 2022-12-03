#include <bits/stdc++.h>

using namespace std;

/*** Start AoC header ***/
istream& operator>>(istream &is, vector<string> &v) {
    string s; getline(cin, s);
    if (s.size()) v.push_back(s);
    return is;
}

vector<string> input() {
    vector<string> lines;
    while (!cin.eof()) {
	cin >> lines;
    }
    return lines;
}

template <typename U, typename Lambda, typename T = vector<typename result_of<Lambda(U)>::type>>
T mapv(const vector<U> &v, Lambda fun) {
    T res(v.size());
    transform(v.begin(), v.end(), res.begin(), fun);
    return res;
}

template <typename U, typename T, typename Lambda>
T fold_left(const vector<U> &v, T init, Lambda fun) {
    T result = init;
    for (const U &element : v) {
	result = fun(result, element);
    }
    return result;
}

int sum(const vector<int> &v) {
    return accumulate(v.begin(), v.end(), 0);
}

template <typename T>
vector<T> intersection(vector<T> v1, vector<T> v2) {
    vector<T> inter;
    sort(v1.begin(), v1.end());
    sort(v2.begin(), v2.end());
    set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(), back_inserter(inter));
    return inter;
}
/*** End AoC header ***/
