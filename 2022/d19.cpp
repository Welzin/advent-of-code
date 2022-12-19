#include "utils.hpp"

class Ore {
public:
    Ore() : _robots(0), _collected(0) {}
    Ore(size_t robots) : _robots(robots), _collected(0) {}

    bool operator>(const Ore &oth) const {
        return _robots == oth._robots && _collected > oth._collected;
    }

    bool operator==(const Ore &oth) const {
        return _robots == oth._robots && _collected == oth._collected;
    }

    bool operator>=(const Ore &oth) const {
        return *this > oth || *this == oth;
    }

    size_t collected() const { return _collected; }
    size_t robots() const { return _robots; }

    void addCollected(int n) { _collected += n; }
    void addRobot() { _robots += 1; }

private:
    size_t _robots;
    size_t _collected;
};

class Case;

enum class Type {
    Ore = 0, Clay, Obsidian, Geode,
};

class IBlueprint {
public:
    virtual Case next(Case c, Type t) const = 0;
    virtual unordered_map<Type, size_t> costs(Type) const = 0;
};

class Case {
public:
    Case() : _ores({{ Type::Ore, Ore(1) }, { Type::Clay, Ore() }, { Type::Obsidian, Ore() }, { Type::Geode, Ore() } }) {}

    bool operator>(const Case &oth) const {
        return _ores.at(Type::Geode) > oth._ores.at(Type::Geode) && _ores.at(Type::Obsidian) > oth._ores.at(Type::Obsidian) && _ores.at(Type::Clay) > oth._ores.at(Type::Clay) && _ores.at(Type::Ore) > oth._ores.at(Type::Ore);
    }

    bool operator==(const Case &oth) const {
        return _ores.at(Type::Geode) == oth._ores.at(Type::Geode) && _ores.at(Type::Obsidian) == oth._ores.at(Type::Obsidian) &&
            _ores.at(Type::Clay) == oth._ores.at(Type::Clay) && _ores.at(Type::Ore) == oth._ores.at(Type::Ore);
    }

    bool operator>=(const Case &oth) const {
        return _ores.at(Type::Geode) >= oth._ores.at(Type::Geode) && _ores.at(Type::Obsidian) >= oth._ores.at(Type::Obsidian) && _ores.at(Type::Clay) >= oth._ores.at(Type::Clay) && _ores.at(Type::Ore) >= oth._ores.at(Type::Ore);
    }

    const Ore &ore(Type type) const {
        return _ores.at(type);
    }

    Ore &ore(Type type) {
        return _ores[type];
    }

    vector<Case> next(const IBlueprint *bp) const {
        vector<Case> nextCases;
        Case newC = *this;
        for (Type t = Type::Ore; t <= Type::Geode; t = Type((int)t + 1)) {
            newC.ore(t).addCollected(ore(t).robots());
        }
        nextCases.push_back(newC);
        for (Type t = Type::Ore; t <= Type::Geode; t = Type((int)t + 1)) {
            nextCases.push_back(bp->next(*this, t));
        }
        return nextCases;
    }

    void print() const {
        cout << "Case: \n";
        for (Type t = Type::Ore; t <= Type::Geode; t = Type((int)t + 1)) {
            cout << "\t* " << (int)t << ": collected: " << ore(t).collected() << ", robots: " << ore(t).robots() << "\n";
        }
        cout << "\n";
    }

private:
    unordered_map<Type, Ore> _ores;
};

class Blueprint : public IBlueprint {
public:
    Blueprint() : _funs(), _costs() {
        for (Type t = Type::Ore; t <= Type::Geode; t = Type((int)t + 1)) {
            _funs[t] = [](Case c) { return c; };
        }
    }

    void addRobotBlueprint(Type robotType, const unordered_map<Type, size_t> &costs) {
        _costs[robotType] = costs;
        _funs[robotType] = [costs, robotType](Case c) {
            bool canConstruct = true;
            for (auto p : costs) {
                if (c.ore(p.first).collected() < p.second) canConstruct = false;
            }
            if (canConstruct) {
                for (auto p : costs) {
                    c.ore(p.first).addCollected(- (int)p.second);
                }
                c.ore(robotType).addRobot();
            }
            return c;
        };
    }

    unordered_map<Type, size_t> costs(Type robotType) const {
        return _costs.at(robotType);
    }

    Case next(Case c, Type t) const {
        Case newC = _funs.at(t)(c);

        for (Type t = Type::Ore; t <= Type::Geode; t = Type((int)t + 1)) {
            newC.ore(t).addCollected(c.ore(t).robots());
        }        

        return newC;
    }

private:
    unordered_map<Type, function<Case(Case)>> _funs;
    unordered_map<Type, unordered_map<Type, size_t>> _costs;
};

Type toType(const string &str) {
    if (str == "ore") return Type::Ore;
    if (str == "clay") return Type::Clay;
    if (str == "obsidian") return Type::Obsidian;
    return Type::Geode;
}

void addCost(Type type, const string &cost, Blueprint &blueprint) {
    regex number(R"(\d+ [^ ]+)");
    sregex_iterator matches(cost.begin(), cost.end(), number);
    sregex_iterator end;

    unordered_map<Type, size_t> costs;
    while (matches != end) {
        auto vec = split((*matches)[0], ' ');
        costs[toType(vec[1])] = stoi(vec[0]);
        ++matches;
    }

    blueprint.addRobotBlueprint(type, costs);
}

vector<Blueprint> parse(const vector<string> &lines) {
    array<Type, 4> types = { Type::Ore, Type::Clay, Type::Obsidian, Type::Geode };
    vector<Blueprint> blueprints;
    for (auto line : lines) {
        Blueprint blueprint;
        auto costs = split(line, '.');
        for (size_t i = 0; i < costs.size(); ++i) {
            addCost(types[i], costs[i], blueprint);
        }
        blueprints.push_back(blueprint);
    }
    return blueprints;
}

int compute(const Blueprint &bp, const size_t iterations) {
    vector<Case> cases[iterations + 1];
    cases[0] = { Case() };

    unordered_map<Type, size_t> maxNeeded;
    for (Type t = Type::Ore; t <= Type::Geode; t = Type((int)t + 1)) {
        maxNeeded[t] = 0;
    }
    for (Type t = Type::Ore; t <= Type::Geode; t = Type((int)t + 1)) {
        auto costs = bp.costs(t);
        
        for (Type t1 = Type::Ore; t1 <= Type::Geode; t1 = Type((int)t1 + 1)) {
            maxNeeded[t1] = max(maxNeeded[t1], costs[t1]);
        }
    }

    for (size_t i = 1; i <= iterations; ++i) {
        vector<Case> result;
        for (auto case_ : cases[i - 1]) {
            auto alt = case_.next(&bp);
            result.insert(result.end(), alt.begin(), alt.end());
        }

        // Prune bad results:
        //  - same amount with less robots
        //  - max num of robots reached (when the max resources needed to build a robot caps)
        //  - resources can not be spent before the last iteration
        for (size_t j = 0; j < result.size(); ++j) {
            bool keep = true;

            // Too much robots, no need to make more of it
            for (Type t = Type::Ore; t < Type::Geode; t = Type((int)t + 1)) {
                if (result[j].ore(t).robots() > maxNeeded[t]) keep = false;
            }

            // Too much ore, cap it to the max needed to prune states
            if (keep) {
                for (Type t = Type::Ore; t < Type::Geode; t = Type((int)t + 1)) {
                    auto maxUseful = maxNeeded[t] * (iterations - i - 1);
                    if (result[j].ore(t).collected() >= maxUseful) 
                        result[j].ore(t).addCollected(- (result[j].ore(t).collected() - maxUseful));
                }
            }

            // Check if any state subsumes current state
            if (keep) {
                for (size_t k = 0; keep && k < cases[i].size(); ++k) {
                    keep = keep && !(cases[i][k] >= result[j]); 
                }
            }

            // Check if current state subsumes any solution state
            if (keep) {
                vector<vector<Case>::iterator> toDelete;
                for (auto it = cases[i].begin(); it != cases[i].end(); ++it) {
                    if (result[j] >= *it) {
                        toDelete.push_back(it);
                    }
                }

                for (auto it : toDelete) {
                    cases[i].erase(it);
                }

                cases[i].push_back(result[j]);
            }
        }
    }

    size_t mx = 0;
    for (Case c : cases[iterations]) {
        if (c.ore(Type::Geode).collected() > mx) mx = c.ore(Type::Geode).collected();
    }
    return mx;
}

size_t part1(const vector<Blueprint> &blueprints) {
    vector<int> results = mapv(blueprints, [](auto bp) { return compute(bp, 24); });
    size_t result = 0;
    for (size_t i = 0; i < results.size(); ++i) {
        result += (i + 1) * results[i];
    }
    return result;
}

size_t part2(const vector<Blueprint> &blueprints) {
    size_t result = 1;
    for (size_t i = 0; i < 3; ++i) {
        result *= compute(blueprints[i], 32);
    }
    return result;
}

int main() {
    auto blueprints = parse(input());
    cout << "Part 1 answer: " << part1(blueprints) << "\n";
    cout << "Part 2 answer: " << part2(blueprints) << "\n";
}