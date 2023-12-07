#include "utils.hpp"

struct Card {
  vector<int> values;
  size_t score;
};

map<char, int> defaultMap = {
    {'T', 10},
    {'J', 11},
    {'Q', 12},
    {'K', 13},
    {'A', 14},
};

vector<Card> parse(const vector<string> &inputs, const map<char,int> m_) {
  vector<Card> cards;
  for (auto input : inputs) {
    auto spl = utils::split(input, ' ');
    Card card;
    card.score = stoll(spl[1]);
    for (char c : spl[0]) {
      int alt = c - '0';
      if (alt >= 0 && alt <= 9)
        card.values.push_back(alt);
      else
	card.values.push_back(m_.at(c));
    }
    cards.push_back(card);
  }
  return cards;
}

bool comp(const Card &c1, const Card &c2) {
  int seenC1[15]; for (int i = 0; i < 15; ++i) seenC1[i] = 0;
  int seenC2[15]; for (int i = 0; i < 15; ++i) seenC2[i] = 0;
  int maxC1 = 0, maxC2 = 0;
  for (int v : c1.values) {
    ++seenC1[v];
    if (v != 1 && seenC1[v] > seenC1[maxC1]) maxC1 = v;
  }
  for (int v : c2.values) {
    ++seenC2[v];
    if (v != 1 && seenC2[v] > seenC2[maxC2]) maxC2 = v;
  }

  seenC1[maxC1] += seenC1[1];
  seenC2[maxC2] += seenC2[1];
  seenC1[1] = 0;
  seenC2[1] = 0;

  vector<pair<int, int>> countC1;
  vector<pair<int, int>> countC2;
  
  for (int i = 0; i < 15; ++i) {
    if (seenC1[i] != 0) {
      countC1.push_back(make_pair(i, seenC1[i]));
    }
    if (seenC2[i] != 0) {
      countC2.push_back(make_pair(i, seenC2[i]));
    }
  }

  sort(countC1.begin(), countC1.end(), [](const auto &p1, const auto &p2) {
    return !(p1.second < p2.second || (p1.second == p2.second && p1.first < p2.first));
  });
  sort(countC2.begin(), countC2.end(), [](const auto &p1, const auto &p2) {
    return !(p1.second < p2.second || (p1.second == p2.second && p1.first < p2.first));
  });

  size_t i = 0;
  while (i < min(countC1.size(), countC2.size()) && countC1[i].second == countC2[i].second) {
    i++;
  }
  
  if (i < min(countC1.size(), countC2.size()) && countC1[i].second != countC2[i].second) {
    return countC1[i].second < countC2[i].second;
  } else {
    i = 0;
    while (c1.values[i] == c2.values[i]) {
      i++;
    }
    return c1.values[i] < c2.values[i];
  }
}

void part1(vector<Card> cards) {
  sort(cards.begin(), cards.end(), comp);
  size_t total = 0;
  for (size_t i = 0; i < cards.size(); ++i) {
    total += (i+1) * cards[i].score;
  }
  cout << total << "\n";
}

void part2(const vector<string> &inputs) {
  map<char, int> cp = defaultMap;
  cp['J'] = 1;
  vector<Card> cards = parse(inputs, cp);
    sort(cards.begin(), cards.end(), comp);
  size_t total = 0;
  for (size_t i = 0; i < cards.size(); ++i) {
    total += (i+1) * cards[i].score;
  }
  cout << total << "\n";
}

int main() {
  vector<string> inputs = utils::input();
  auto start = chrono::high_resolution_clock::now();
  part1(parse(inputs, defaultMap));
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  start = chrono::high_resolution_clock::now();
  part2(inputs);
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
