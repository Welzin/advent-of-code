#include "utils.hpp"

struct RGB {
  int red;
  int green;
  int blue;
};

RGB getRGB(const string &game) {
  RGB out;
  out.red = 0; out.green = 0; out.blue = 0;
  auto moves = utils::split(game, ',');
  for (auto move : moves) {
    int res = stoi(utils::split(move, ' ')[1]);
    if (move.find("red") != string::npos) {
      out.red = res;
    }
    if (move.find("green") != string::npos) {
      out.green = res;
    }
    if (move.find("blue") != string::npos) {
      out.blue = res;
    }
  }
  return out;
}

RGB getMaxRGBFromGames(const string &gameString) {
  auto spl = utils::split(gameString, ':');
  auto games = utils::split(spl[1], ';');
  RGB maxRGB; maxRGB.red = 0; maxRGB.green = 0; maxRGB.blue = 0;
  for (auto game : games) {
    auto rgb = getRGB(game);
    maxRGB.red = max(maxRGB.red, rgb.red);
    maxRGB.green = max(maxRGB.green, rgb.green);
    maxRGB.blue = max(maxRGB.blue, rgb.blue);
  }
  return maxRGB;
}

void part1(const vector<string> &inputs) {
  int total = 0;
  for (const string &in : inputs) {
    auto spl = utils::split(in, ':');
    RGB maxRGB = getMaxRGBFromGames(in);
    if (maxRGB.red <= 12 && maxRGB.green <= 13 && maxRGB.blue <= 14) {
      total += stoi(utils::split(spl[0], ' ')[1]);
    }
  }
  cout << total << "\n";
  assert(total == 2285);
}

void part2(const vector<string> &inputs) {
  int total = 0;
  for (const string &in : inputs) {
    RGB maxRGB = getMaxRGBFromGames(in);
    total += maxRGB.red * maxRGB.green * maxRGB.blue;
  }
  cout << total << "\n";
  assert(total == 77021);
}

int main() {
  vector<string> inputs = utils::input();
  part1(inputs);
  part2(inputs);
}
