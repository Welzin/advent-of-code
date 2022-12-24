#include "utils.hpp"

using Direction = pair<int, int>;

class Blizzard {
public:
    Blizzard(int x, int y, const Direction &dir) : _x(x), _y(y), _dir(dir) {}

    int x() const { return _x; }
    int y() const { return _y; }

    void setX(int x) { _x = x; }
    void setY(int y) { _y = y; }

    Blizzard next() { return Blizzard(_x + _dir.first, _y + _dir.second, _dir); }

private:
    int _x;
    int _y;
    Direction _dir;
};

class Grid {
public:
    Grid() : _blizzards(), _rows(0), _cols(0) {}

    void addBlizzard(const Blizzard &blizzard) { 
        _blizzards.push_back(blizzard); 
        _booleanMap[blizzard.x()][blizzard.y()] = true;
    }
    void setRows(size_t r) { _rows = r; initMap(); }
    void setCols(size_t c) { _cols = c; initMap(); }

    size_t rows() const { return _rows; }
    size_t cols() const { return _cols; }

    Grid next() {
        Grid nextGrid;
        nextGrid.setRows(_rows);
        nextGrid.setCols(_cols);
        for (auto blizzard : _blizzards) {
            auto nextBlizzard = blizzard.next();
            if (nextBlizzard.x() == 0) nextBlizzard.setX(_rows - 2);
            if (nextBlizzard.x() == (int)_rows - 1) nextBlizzard.setX(1);
            if (nextBlizzard.y() == 0) nextBlizzard.setY(_cols - 2);
            if (nextBlizzard.y() == (int)_cols - 1) nextBlizzard.setY(1);
            nextGrid.addBlizzard(nextBlizzard);
        }
        return nextGrid;
    }

    bool hasBlizzard(size_t x, size_t y) {
        return _booleanMap[x][y]; 
    }

private:
    void initMap() {
        _booleanMap.resize(_rows);
        for (size_t i = 0; i < _rows; ++i) {
            _booleanMap[i].resize(_cols);
            for (size_t j = 0; j < _cols; ++j) { _booleanMap[i][j] = false; }
        }
    }

    vector<vector<bool>> _booleanMap;
    vector<Blizzard> _blizzards;
    size_t _rows;
    size_t _cols;
};

Grid parse(const vector<string> &lines) {
    Grid g;
    g.setRows(lines.size());
    g.setCols(lines[0].size());
    for (size_t i = 0; i < lines.size(); ++i) {
        auto line = lines[i];
        for (size_t j = 0; j < line.size(); ++j) {
            if (line[j] == '>') g.addBlizzard(Blizzard(i, j, { 0, 1 }));
            if (line[j] == '<') g.addBlizzard(Blizzard(i, j, { 0, -1 }));
            if (line[j] == '^') g.addBlizzard(Blizzard(i, j, { -1, 0 }));
            if (line[j] == 'v') g.addBlizzard(Blizzard(i, j, { 1, 0 }));
        }
    }
    return g;
}

pair<size_t, Grid> bruteforce(Grid grid, size_t xsrc, size_t ysrc, size_t xdst, size_t ydst) {
    size_t moves = 0;

    set<pair<int, int>> states {{xsrc, ysrc}};

    while (1) {
        //cout << moves << " " << states.size() << "\n";
        moves += 1;
        grid = grid.next();

        set<pair<int, int>> nextStates;
        for (auto [x, y] : states) {
            // 4 neighbors
            for (auto dir : vector<pair<int, int>>{ {1, 0}, {0, 1}, {0, -1}, {-1, 0}, { 0, 0 } }) {
                int altx = (int)x + dir.first;
                int alty = (int)y + dir.second;

                if (alty <= 0 || alty >= grid.cols() - 1 || altx < 0 || altx >= grid.rows()) continue; // Out of bounds check
                if ((alty != 1 && altx <= 0) || (alty != grid.cols() - 2 && altx >= grid.rows() - 1)) continue; // Out of bounds if not in the holes of the walls

                if (!grid.hasBlizzard(altx, alty)) {
                    nextStates.insert({altx, alty});
                }
            }
        }

        for (auto [x, y] : nextStates) {
            if (x == (int)xdst && y == (int)ydst) {
                return {moves, grid};
            }
        }

        states = nextStates;
    }

    return {moves, grid};
}

size_t part1(const Grid &grid) {
    auto [result, _] = bruteforce(grid, 0, 1, grid.rows() - 1, grid.cols() - 2);
    return result;
}

size_t part2(const Grid &grid) {
    auto [p1, gridp1] = bruteforce(grid, 0, 1, grid.rows() - 1, grid.cols() - 2);
    auto [p2, gridp2] = bruteforce(gridp1, grid.rows() - 1, grid.cols() - 2, 0, 1); 
    auto [p3, _] = bruteforce(gridp2, 0, 1, grid.rows() - 1, grid.cols() - 2);
    return p1 + p2 + p3;
}

int main() {
    auto grid = parse(input());
    cout << "Part 1 answer: " << part1(grid) << "\n";
    cout << "Part 2 answer: " << part2(grid) << "\n";
}