#include "utils.hpp"

class Cell {
public:
    Cell(int x, int y) : _x(x), _y(y) {}

    bool operator<(const Cell &oth) const {
        return oth._x < _x || (oth._x == _x && oth._y < _y);
    }

    bool operator==(const Cell &oth) const {
        return oth._x == _x && oth._y == _y;
    }

    bool operator>=(const Cell &oth) const {
        return !(oth < *this);
    }

    bool operator>(const Cell &oth) const {
        return oth >= *this && !(oth == *this);
    }

    int x() const { return _x; }

    int y() const { return _y; }

private:
    int _x;
    int _y;
};

class Grid {
public:
    Grid() : _cells(), _directionsProposed() {
        for (auto p : vector<pair<int, int>>{{ -1, 0 }, { 1, 0 }, { 0, - 1}, { 0, 1 }}) {
            _directionsProposed.push_back(p);
        }
    }

    void addCell(const Cell &cell) { _cells.push_back(cell); }

    size_t emptyTilesInSmallestRectangle() const {
        auto [xmin, xmax, ymin, ymax] = smallestRectangle();
        // Calculate the area of the rectangle minus the number of cells to get the result.
        return (xmax - xmin + 1) * (ymax - ymin + 1) - _cells.size();
    }

    Grid next() {
        Grid nextGrid;
        auto [xmin, xmax, ymin, ymax] = smallestRectangle();
        bool grid[(xmax - xmin) + 3][(ymax - ymin) + 3];
        for (int x = 0; x <= (xmax - xmin) + 2; ++x) {
            for (int y = 0; y <= (ymax - ymin) + 2; ++y) {
                grid[x][y] = false;
            }
        }

        for (const auto &cell : _cells) {
            grid[cell.x() - xmin + 1][cell.y() - ymin + 1] = true;
        }

        vector<Cell> proposedMoves;
        for (const auto &cell : _cells) {
            bool neighborFound = false;
            for (int x = cell.x() - xmin; x <= cell.x() - xmin + 2; ++x) {
                for (int y = cell.y() - ymin; y <= cell.y() - ymin + 2; ++y) {
                    if (x == cell.x() - xmin + 1 && y == cell.y() - ymin + 1) continue;
                    neighborFound = neighborFound || grid[x][y];
                }
            }

            if (!neighborFound) proposedMoves.push_back(cell);
            else {
                bool moved = false;
                for (auto [a, b] : _directionsProposed) {
                    bool canMove = true;
                    if (a == 0) {
                        for (int x = cell.x() - xmin; x <= cell.x() - xmin + 2; ++x) {
                            canMove = canMove && !grid[x][cell.y() - ymin + 1 + b];
                        }
                    }
                    if (b == 0) {
                        for (int y = cell.y() - ymin; y <= cell.y() - ymin + 2; ++y) {
                            canMove = canMove && !grid[cell.x() - xmin + 1 + a][y];
                        }
                    }

                    if (canMove) {
                        proposedMoves.push_back(Cell(cell.x() + a, cell.y() + b));
                        moved = true;
                        break;
                    }
                }

                if (!moved) {
                    proposedMoves.push_back(cell);
                }
            }
        }

        for (size_t i = 0; i < proposedMoves.size(); ++i) {
            bool canBeAdded = true;
            for (size_t j = 0; j < proposedMoves.size(); ++j) {
                if (i == j) continue;
                
                canBeAdded = canBeAdded && !(proposedMoves[i] == proposedMoves[j]);
            }
            if (canBeAdded) {
                nextGrid.addCell(proposedMoves[i]);
            } else {
                nextGrid.addCell(_cells[i]);
            }
        }

        auto nextLastDir = _directionsProposed.front();
        _directionsProposed.pop_front();
        _directionsProposed.push_back(nextLastDir);

        nextGrid.setDirections(_directionsProposed);

        return nextGrid;
    }

    void setDirections(const deque<pair<int, int>> &dirs) { _directionsProposed = dirs; }

    void print() const {
        auto [xmin, xmax, ymin, ymax] = smallestRectangle();
        bool grid[(xmax - xmin) + 3][(ymax - ymin) + 3];
        for (int x = 0; x <= (xmax - xmin) + 2; ++x) {
            for (int y = 0; y <= (ymax - ymin) + 2; ++y) {
                grid[x][y] = false;
            }
        }

        for (const auto &cell : _cells) {
            grid[cell.x() - xmin + 1][cell.y() - ymin + 1] = true;
        }

        for (int x = 0; x <= (xmax - xmin) + 2; ++x) {
            for (int y = 0; y <=(ymax - ymin) + 2; ++y) {
                if (grid[x][y]) cout << "#";
                else cout << ".";
            }
            cout << "\n";
        }
    }

    bool operator==(Grid oth) {
        if (oth._cells.size() != _cells.size()) return false;

        sort(_cells.begin(), _cells.end());
        sort(oth._cells.begin(), oth._cells.end());

        for (size_t i = 0; i < _cells.size(); ++i) {
            if (!(_cells[i] == oth._cells[i])) return false;
        }

        return true;
    }
private:
    tuple<int, int, int, int> smallestRectangle() const {
        // Get min x, min y, max x and max y of any cell
        auto [xmin, xmax, ymin, ymax] = make_tuple(numeric_limits<int>::max(), 0, numeric_limits<int>::max(), 0);
        for (const auto &cell : _cells) {
            if (cell.x() < xmin) xmin = cell.x();
            if (cell.x() > xmax) xmax = cell.x();
            if (cell.y() < ymin) ymin = cell.y();
            if (cell.y() > ymax) ymax = cell.y();
        }
        return { xmin, xmax, ymin, ymax };
    }

    vector<Cell> _cells;
    deque<pair<int, int>> _directionsProposed;
};

Grid parse(const vector<string> &lines) {
    Grid grid;
    for (size_t i = 0; i < lines.size(); ++i) {
        for (size_t j = 0; j < lines[i].size(); ++j) {
            if (lines[i][j] == '#')
                grid.addCell(Cell(i, j));
        }
    }
    return grid;
}

size_t compute(Grid grid, int moves) {
    while (moves--) {
        grid = grid.next();
    }
    return grid.emptyTilesInSmallestRectangle();
}

size_t part1(const Grid &grid) {
    return compute(grid, 10);
}

size_t part2(Grid grid) {
    size_t iterations = 0;
    while (true) {
        ++iterations;
        Grid newGrid = grid.next();

        if (grid == newGrid) break;
        grid = newGrid;
    }
    return iterations;
}

int main() {
    auto grid = parse(input());
    cout << "Part 1 anwer: " << part1(grid) << "\n";
    cout << "Part 2 anwer: " << part2(grid) << "\n";
}