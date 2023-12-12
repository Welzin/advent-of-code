#include "utils.hpp"

#define N 200

size_t dp[N][N][N];

pair<string, vector<size_t>> parse(const string &input, bool repeat) {
  auto spl = utils::split(input, ' ');
  vector<size_t> q;
  for (auto num : utils::split(spl[1], ',')) {
    if (num.size() > 0) q.push_back(stol(num));
  }
  if (repeat) {
    vector<size_t> qbis = q;
    string copy = spl[0];
    for (int i = 0; i < 4; ++i) {
      copy += "?" + spl[0];
      for (auto i : q) {
	qbis.push_back(i);
      }
    }
    q = qbis;
    spl[0] = copy;
  }
  while (spl[0][spl[0].size()-1] == '.') spl[0].erase(spl[0].end()-1);
  spl[0] += '.';
  return make_pair(spl[0], q);
}

size_t dyn(const string &s, const vector<size_t>& damage) {
  dp[0][0][0] = 1;
  for (size_t i = 0; i < damage.size(); ++i) {
    for (size_t j = 0; j < s.size(); ++j) {
      for (size_t k = 0; k < s.size(); ++k) {
	if (s[j] == '.' && k == damage[i]) dp[i + 1][j + 1][0] += dp[i][j][k];
	if (s[j] == '.' && k == 0) dp[i][j + 1][0] += dp[i][j][0];
	if (s[j] == '#' && k < damage[i]) dp[i][j + 1][k + 1] += dp[i][j][k];
        if (s[j] == '?' && k == 0) {
          dp[i][j + 1][0] += dp[i][j][k];
	  dp[i][j + 1][1] += dp[i][j][k];
	}
        if (s[j] == '?' && k > 0) {
	  if (k == damage[i]) dp[i + 1][j + 1][0] += dp[i][j][k];
	  else dp[i][j + 1][k + 1] += dp[i][j][k];
	}
      }
    }
  }

  for (size_t j = 0; j < s.size(); ++j) {
    if (s[j] == '.' || s[j] == '?') dp[damage.size()][j + 1][0] += dp[damage.size()][j][0];
  }
  return dp[damage.size()][s.size()][0];
}

void part1(const vector<string> &inputs) {
  size_t total = 0;  
  for (auto input : inputs) {
    auto [springs, damage] = parse(input, false);
    memset(dp, 0, sizeof(dp));
    size_t result = dyn(springs, damage);
    total += result;
  }
  cout << total << "\n";
}

void part2(const vector<string> &inputs) {
  size_t total = 0;  
  for (auto input : inputs) {
    auto [springs, damage] = parse(input, true);
    memset(dp, 0, sizeof(dp));
    size_t result = dyn(springs, damage);
    total += result;
  }
  cout << total << "\n";
}

int main() {
  vector<string> inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  part1(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
