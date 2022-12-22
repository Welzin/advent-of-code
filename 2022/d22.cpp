#include "utils.hpp"

enum class Tile {
    Open,
    Wall,
    Empty,
};

enum class Rotation {
    Clockwise,
    CounterClockwise,
    None,
};

using Grid = vector<vector<Tile>>;
using Path = vector<pair<size_t, Rotation>>;
using Direction = pair<int, int>;
struct Problem { Grid grid; Path path; Direction dir; queue<pair<size_t, size_t>> moves; };

void appendGrid(const string &line, Grid &grid, size_t index) {
    grid[index].resize(line.size());
    for (size_t i = 0; i < line.size(); ++i) {
        switch (line[i]) {
        case '.': 
            grid[index][i] = Tile::Open;
            break;
        case '#':
            grid[index][i] = Tile::Wall;
            break;
        default:
            grid[index][i] = Tile::Empty;
            break;
        }
    }
}

void getPath(const string &line, Path &path) {
    string current = "";
    for (char c : line) {
        if (c == 'L' || c == 'R') {
            path.push_back({ stoll(current), c == 'L' ? Rotation::Clockwise : Rotation::CounterClockwise });
            current = "";
        } else {
            current.push_back(c);
        }
    }
    path.push_back({ stoll(current), Rotation::None });
}

void getMove(const string &line, queue<pair<size_t, size_t>> &moves) {
    auto spl = split(line, ' ');
    moves.push({stoi(spl[0]), stoi(spl[1])});
}

Problem parse(const vector<string> &lines) {
    Problem p;
    p.dir = { 0, 1 };
    size_t s = lines.size() - 1;
    p.grid.resize(s);
    for (size_t i = 0; i < lines.size(); ++i) {
        if (i < s)
            appendGrid(lines[i], p.grid, i);
        else if (i == s)
            getPath(lines[i], p.path);
            /*
        else
            getMove(lines[i], p.moves);*/
    }
    return p;
}

size_t numFromDir(const Direction &dir) {
    if (dir.first != 0) {
        if (dir.first == 1) return 1;
        else return 3;
    }
    else {
        if (dir.second == 1) return 0;
        else return 2;
    }
}

Direction rotate(const Direction &dir, Rotation rotation) {
    if (rotation == Rotation::None) return dir;

    using Matrix = vector<vector<int>>;
    Matrix rotationMatrix;
    if (rotation == Rotation::CounterClockwise) {
        rotationMatrix = {{0, 1}, {-1, 0}};
    } else {
        rotationMatrix = {{0, -1}, {1, 0}};
    }

    return { dir.first * rotationMatrix[0][0] + dir.second * rotationMatrix[0][1], 
             dir.first * rotationMatrix[1][0] + dir.second * rotationMatrix[1][1] };
}

bool outOfBounds(int x, int y, const Grid &grid) {
    return x < 0 || y < 0 || x >= (int)grid.size() || y >= (int)grid[x].size();
}

void move(size_t &x, size_t &y, const Direction &dir, const Grid &grid) {
    int altx = (int)x + dir.first;
    int alty = (int)y + dir.second;

    auto enterBounds = [&grid](int &a, int &b) {
        if (a < 0) a = grid.size() - 1;
        else if (a >= (int)grid.size()) a = 0;

        if (b < 0) b = grid[a].size() - 1;
        else if (b >= (int)grid[a].size() - 1) b = 0;
    };

    if (outOfBounds(altx, alty, grid) || grid[altx][alty] == Tile::Empty) {
        enterBounds(altx, alty);

        while (grid[altx][alty] == Tile::Empty) {
            altx += dir.first;
            alty += dir.second;
            enterBounds(altx, alty);
        }
    }

    if (grid[altx][alty] != Tile::Wall) {
        x = altx;
        y = alty;
    }
}

pair<size_t, size_t> initCoords(const Problem &p) {
    size_t x = 0, y = 0;
    for (size_t i = 0; i < p.grid[0].size(); ++i) {
        if (p.grid[0][i] == Tile::Open) {
            y = i;
            break;
        }
    }
    return {x, y};
}

size_t computeResult(size_t x, size_t y, const Direction &dir) {
    return 1000 * (x + 1) + 4 * (y + 1) + numFromDir(dir);
}

size_t part1(const Problem &p) {
    // Search for path's start
    auto [x, y] = initCoords(p);
    Direction currentDir = p.dir;
    // Play path
    for (auto [moves, rotation] : p.path) {
        while (moves--) {
            move(x, y, currentDir, p.grid);
        }
        currentDir = rotate(currentDir, rotation);
    }

    return computeResult(x, y, currentDir);
}

size_t getTileFrom(int x, int y) {
    if (x < 50) {
        if (y < 100) return 1;
        else return 2;
    }
    else if (x < 100) return 3;
    else if (x < 150) {
        if (y < 50) return 4;
        else return 5;
    }
    return 6;
}

void cubicMove(size_t &x, size_t &y, Direction &dir, const Grid &grid) {
    int altx = (int)x + dir.first;
    int alty = (int)y + dir.second;

    auto enterBounds = [&grid, x, y](int &a, int &b, Direction dir) {
        // Move from a face to another.
        // My cube has the following placement:
        //
        //   1122
        //   1122
        //   33
        //   33
        // 4455
        // 4455
        // 66
        // 66
        // 
        // Gonna calculate depending on the face
        // 1 -> Up
        // 2 -> Right
        // 3 -> Behind
        // 4 -> Left
        // 5 -> Below
        // 6 -> Front

        size_t tile = getTileFrom(x, y);
        size_t ndir = numFromDir(dir);

        Direction left = { 0, -1 }, right = { 0, 1 }, up = { -1, 0 }, down = { 1, 0 };

        // Move to indicated tile
        if (tile == 1) {
            // If dir is up, go to tile 6
            if (ndir == 3) {
                dir = right;
                a = y + 100;
                b = 0;  
            }
            // If dir is left, go to tile 4
            else if (ndir == 2) {
                dir = right;
                a = 149 - x;
                b = 0;
            }
        }
        else if (tile == 2) {
            if (ndir == 3) {
                dir = up;
                a = 199;
                b -= 100;    
            }
            else if (ndir == 0) {
                dir = left;
                a = 149 - x;
                b = 99;    
            }
            else if (ndir == 1) {
                dir = left;
                a = y - 50;
                b = 99;
            }
        }
        else if (tile == 3) {
            if (ndir == 0) {
                dir = up;
                b = x + 50;
                a = 49;    
            }
            else if (ndir == 2) {
                dir = down;
                b = x - 50;
                a = 100;    
            }
        }
        else if (tile == 4) {
            if (ndir == 3) {
                dir = right;
                a = y + 50;
                b = 50;    
            }
            else if (ndir == 2) {
                dir = right;
                a = 149 - x;
                b = 50;
            }
        }
        else if (tile == 5) {
            if (ndir == 0) {
                dir = left;
                a = 149 - x;
                b = 149;
            }
            else if (ndir == 1) {
                dir = left;
                a = y + 100;
                b = 49;    
            }
        }
        else {
            if (ndir == 0) {
                dir = up;
                b = x - 100;
                a = 149;    
            }
            else if (ndir == 1) {
                dir = down;
                a = 0;
                b += 100;
            }
            else if (ndir == 2) {
                dir = down;
                b = x - 100;
                a = 0;
            }
        }

        return dir;
    };

    Direction nextDir = dir;
    if (outOfBounds(altx, alty, grid) || grid[altx][alty] == Tile::Empty) {
        nextDir = enterBounds(altx, alty, dir);
    }

    if (grid[altx][alty] != Tile::Wall) {
        x = altx;
        y = alty;
        dir = nextDir;
    }
}

size_t part2(Problem p) {
    // Search for path's start
    auto [x, y] = initCoords(p);


    Direction currentDir = p.dir;
    // Play path
    for (auto [moves, rotation] : p.path) {
        while (moves--) {
            cubicMove(x, y, currentDir, p.grid);
        }
        currentDir = rotate(currentDir, rotation);
    }

    return computeResult(x, y, currentDir);
}

int main() {
    auto in = parse(input());
    cout << "Part 1 answer: " << part1(in) << "\n";
    cout << "Part 2 answer: " << part2(in) << "\n";
}