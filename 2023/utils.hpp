#include <bits/stdc++.h>

using namespace std;

namespace utils {

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

template <typename T>
T sum(const vector<T> &v) {
    return accumulate(v.begin(), v.end(), 0);
}

template <typename T>
T max(const vector<T> &container) {
    T max = container[0];
    for (auto it = next(container.begin()); it != container.end(); ++it) {
	if (*it > max) max = *it;
    }
    return max;
}

template <typename T>
T min(const vector<T> &container) {
    T min = container[0];
    for (auto it = next(container.begin()); it != container.end(); ++it) {
	if (*it < min) min = *it;
    }
    return min;
}

template <typename T>
vector<T> intersection(vector<T> v1, vector<T> v2) {
    vector<T> inter;
    sort(v1.begin(), v1.end());
    sort(v2.begin(), v2.end());
    set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(), back_inserter(inter));
    return inter;
}

template <typename T>
bool contains(const vector<T>& v, T element) {
    for (const T &e : v) {
	if (e == element) return true;
    }
    return false;
}

template <typename Container, typename T>
vector<size_t> filter_index(const Container &c, function<bool(T)> pred) {
  vector<size_t> indices;
  for (size_t i = 0; i < c.size(); ++i) {
    if (pred(c[i])) {
      indices.push_back(i);
    }
  }
  return indices;
}

vector<string> split(string str, char sep) {
    vector<string> splitStr;
    size_t pos = 0, start = 0;
    while ((pos = str.find(sep, start)) != string::npos) {
	splitStr.push_back(str.substr(start, pos - start));
	start = pos + 1;
    }
    splitStr.push_back(str.substr(start));
    return splitStr;
}

vector<string> split(string str, const string &sep) {
    vector<string> splitStr;
    size_t pos = 0, start = 0;
    while ((pos = str.find(sep, start)) != string::npos) {
	splitStr.push_back(str.substr(start, pos - start));
	start = pos + sep.size();
    }
    splitStr.push_back(str.substr(start));
    return splitStr;
}

  template <typename Duration>
  void display_time(chrono::time_point<chrono::high_resolution_clock> st, const string
		    &msg, const string &timeRepr) {
  auto duration = chrono::duration_cast<Duration>(chrono::high_resolution_clock::now() - st);
  cout << msg << duration.count() << timeRepr << ".\n";
}

/* Positive mod */
int mod(int k, int n) {
  return (k % n + n) % n;
}
};
