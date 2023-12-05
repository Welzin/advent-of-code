#include "utils.hpp"

enum Map {
  SeedToSoil = 0,
  SoilToFertilizer,
  FertilizerToWater,
  WaterToLight,
  LightToTemperature,
  TemperatureToHumidity,
  HumidityToLocation,
};

struct Mapper {
  size_t lo;
  size_t hi;
  size_t st;
  Mapper(size_t lo, size_t hi, size_t st) : lo(lo), hi(hi), st(st) {}
};

vector<size_t> getSeeds(const string &input) {
  auto spl = utils::split(input, ':');
  vector<size_t> seeds;
  for (auto element : utils::split(spl[1], ' ')) {
    if (element.size() > 0) {
      seeds.push_back(stoll(element));
    }
  }
  return seeds;
}

pair<vector<size_t>, map<int, vector<Mapper>>> parse(const vector<string> &inputs) {
  map<int, vector<Mapper>> mapper;

  int currentMap = SeedToSoil;
  for (size_t i = 2; i < inputs.size(); ++i) {
    auto spl = utils::split(inputs[i], ' ');
    if (spl[1][3] == ':') ++currentMap;
    else {
      // st, lo, range
      size_t lo = stoll(spl[1]);
      size_t ra = stoll(spl[2]);
      mapper[currentMap].push_back(Mapper(lo, lo + ra, stoll(spl[0])));
    }
  }

  return make_pair(getSeeds(inputs[0]), mapper);
}

size_t mapSeed(size_t seed, const map<int, vector<Mapper>> &mappers) {
  for (int m = SeedToSoil; m <= HumidityToLocation; ++m) {
    bool found = false;
    size_t i = 0;
    while (!found && i < mappers.at(m).size()) {
      auto mapper = mappers.at(m).at(i);
      if (seed >= mapper.lo && seed < mapper.hi) {
        seed = mapper.st + (seed - mapper.lo);
	found = true;
      }
      ++i;
    }
  }
  return seed;
}

void part1(const vector<string> &inputs) {
  size_t total = (size_t)-1;
  auto [seeds, maps] = parse(inputs);
  for (auto seed : seeds) {
    size_t result = mapSeed(seed, maps);
    total = total < result ? total : result;
  }
  cout << total << "\n";
  assert(total == 389056265);
}

void part2(const vector<string> &inputs) {
  size_t total = (size_t)-1;
  auto [seeds, maps] = parse(inputs);

  for (size_t i = 0; i < seeds.size(); i += 2) {
    cout << "Seed " << i / 2 << "\n";
    for (size_t j = 0; j < seeds[i + 1]; ++j) {
      size_t result = mapSeed(seeds[i] + j, maps);
      total = total < result ? total : result;
    }
  }
  cout << total << "\n";
  assert(total == 137516820);
}

int main() {
  vector<string> inputs = utils::input();
  part1(inputs);
  part2(inputs);
}
