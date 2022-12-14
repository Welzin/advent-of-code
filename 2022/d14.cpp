#include "utils.hpp"

int sand = 500;

enum Case {
    Empty,
    Full,
    Sand,
};

struct Coord { 
    Coord(const string &coord) {
        auto xy = split(coord, ',');
        x = stoi(xy[0]);
        y = stoi(xy[1]);
    }
    Coord(int x, int y) : x(x), y(y) {}
    int x; int y; 
};

struct Size { int x; int y; int xmin; };

using Grid = vector<vector<Case>>;
using Path = vector<Coord>;

Path pathFromLine(const string &line) {
    Path p;
    for (auto coord : split(line, " -> ")) {
        p.push_back(Coord(coord));
    }
    return p;
}

vector<Path> getPaths(const vector<string> &lines) {
    vector<Path> paths;
    for (auto line : lines) {
        paths.push_back(pathFromLine(line));
    }
    return paths;
}

Size getGridSize(const vector<Path> &paths) {
    Size s { .x = 0, .y = 0, .xmin = INT_MAX };
    for (Path p : paths) {
        for (Coord c : p) {
            if (c.x > s.x) s.x = c.x;
            if (c.x < s.xmin) s.xmin = c.x;
            if (c.y > s.y) s.y = c.y;
        }
    }
    s.x -= s.xmin;
    return s;
}

Grid parse(const vector<string> &lines) {
    auto paths = getPaths(lines);
    auto gridSize = getGridSize(paths);
    sand -= gridSize.xmin;
    Grid grid(gridSize.y + 1);
    for (int i = 0; i <= gridSize.y; ++i) {
        grid[i].resize(gridSize.x + 1);
        for (int j = 0; j <= gridSize.x; ++j) {
            grid[i][j] = Empty;
        }
    }

    for (Path p : paths) {
        int xlast = -1, ylast = -1;
        for (Coord c : p) {
            int xcurr = c.x - gridSize.xmin, ycurr = c.y;
            if (xlast != -1) {
                int diff = (xcurr - xlast) / abs(xcurr - xlast);
                for (int a = xlast; a != xcurr + diff; a += diff) {
                    grid[ylast][a] = Full;
                }
            }
            if (ylast != -1) {
                int diff = (ycurr - ylast) / abs(ycurr - ylast);
                for (int a = ylast; a != ycurr + diff; a += diff) {
                    grid[a][xlast] = Full;
                }
            }
            xlast = xcurr;
            ylast = ycurr;
        }
    }

    return grid;
}

void displayGrid(Grid g) {
    for (auto row : g) {
        for (auto col : row) {
            if (col == Empty) cout << ".";
            else if (col == Full) cout << "#";
            else cout << "o";
        }
        cout << "\n";
    }
}

bool canMove(const Coord &c, const Grid &grid) {
    return c.y < grid.size() - 1 && 
        (grid[c.y + 1][c.x] == Empty || (c.x > 0 && grid[c.y + 1][c.x - 1] == Empty) ||
        (c.x < grid[c.y + 1].size() && grid[c.y + 1][c.x + 1] == Empty));
}

void move(Coord &c, const Grid &grid) {
    if (grid[c.y + 1][c.x] == Empty) c.y += 1;
    else if (c.x > 0 && grid[c.y + 1][c.x - 1] == Empty) {
        c.x -= 1; c.y += 1;
    }
    else {
        c.x += 1; c.y += 1;
    }
}

bool canMoveOutOfBounds(const Coord &c, const Grid &grid) {
    return c.y + 1 >= grid.size() || c.x - 1 < 0 || c.x + 1 >= grid[0].size();
}

void update(const Coord &c, Grid &g) {
    g[c.y][c.x] = Sand;
}

int compute(Grid g, function<bool(const Coord &c, const Grid &grid)> outOfBounds) {
    int iterations = 0;

    while (1) {
        Coord currentSand(sand, 0);
        while (canMove(currentSand, g)) {
            move(currentSand, g);
        }
        if (outOfBounds(currentSand, g)) {
            return iterations;
        }
        update(currentSand, g);
        iterations += 1;
    }
}

int part1(Grid g) {
    return compute(g, &canMoveOutOfBounds);
}

bool outOfBoundsP2(const Coord &c, const Grid &grid) {
    return c.x == sand && c.y == 0;
}

int part2(Grid g) {
    vector<Case> empty500(500);
    for (int i = 0; i < 500; ++i) empty500[i] = Empty;
    for (int i = 0; i < g.size(); ++i) {
        g[i].insert(g[i].begin(), empty500.begin(), empty500.end());
        g[i].insert(g[i].end(), empty500.begin(), empty500.end());
    }

    sand += 500;
    g.push_back(vector<Case>(g[0].size()));
    g.push_back(vector<Case>(g[0].size()));
    int len = g.size() - 1;
    for (int i = 0; i < g[0].size(); ++i) {
        g[len][i] = Full;
    }

    return compute(g, &outOfBoundsP2) + 1;
}

int main() {
    auto in = parse(input());
    cout << part1(in) << "\n\n";
    cout << part2(in) << "\n";
}