#include "utils.hpp"

enum Move {
    Left,
    Right,
};

enum Rock {
    Minus,
    Plus,
    Angle,
    Pipe,
    Square,
};

enum State {
    Empty,
    Full,
};

struct Coord { size_t x; size_t y; };

using Grid = vector<vector<State>>;
using RealRock = vector<Coord>;

queue<Rock> rocks({ Minus, Plus, Angle, Pipe, Square });

queue<Move> parse(const vector<string> &lines) {
    queue<Move> moves;
    for (char c : lines[0]) {
        if (c == '>') moves.push(Right);
        else moves.push(Left);
    }
    return moves;
}

size_t rockSize(Rock rock) {
    switch(rock) {
    case Minus: return 1;
    case Angle:
    case Plus: return 3;
    case Pipe: return 4;
    case Square: return 2;
    }
    return 0;
}

void insertLine(Grid &grid) {
    vector<State> line(7);
    for (size_t j = 0; j < 7; ++j) {
        line[j] = Empty;
    }
    grid.insert(grid.begin(), line);
}

Grid initGrid() {
    Grid grid(4);
    for (size_t i = 0; i < 4; ++i) {
        for (size_t j = 0; j < 7; ++j) {
            grid[i].push_back(Empty);
        }
    }
    return grid;
}

int insertLines(Grid &grid, size_t height, size_t rockSize) {
    int bottom = 4 - (grid.size() - height);
    int numberOfLines = bottom + rockSize - 1;

    if (numberOfLines >= 0) {
        while (numberOfLines--)
            insertLine(grid);
        return 0;
    }
    return -1*numberOfLines;
}

RealRock createRock(const Grid &grid, Rock rock, size_t xstart) {
    switch (rock) {
    case Minus: return { {.x = xstart, .y = 2}, {.x = xstart, .y = 3}, {.x = xstart, .y = 4}, {.x = xstart, .y = 5} };
    case Plus: return { {.x = xstart, .y = 3}, {.x = xstart+1, .y = 2}, {.x = xstart+1, .y = 3}, {.x = xstart+1, .y = 4}, {.x = xstart+2, .y = 3} };
    case Angle: return { {.x = xstart, .y = 4}, {.x = xstart+1, .y = 4}, {.x = xstart+2, .y = 4}, {.x = xstart+2, .y = 3}, {.x = xstart+2, .y = 2} };
    case Pipe: return { {.x = xstart, .y = 2}, {.x = xstart+1, .y = 2}, {.x = xstart+2, .y = 2}, {.x = xstart+3, .y = 2} };
    case Square: return { {.x = xstart, .y = 2}, {.x = xstart, .y = 3}, {.x = xstart+1, .y = 2}, {.x = xstart+1, .y = 3} };
    }
    return {};
}

bool canMove(const Grid &grid, const RealRock &rock) {
    for (auto c : rock) {
        if (c.x + 1 >= grid.size() || grid[c.x + 1][c.y] == Full) return false;
    }
    return true;
}

bool canMoveLeft(const Grid &grid, const RealRock &rock) {
    for (auto c : rock) {
        if (c.y == 0 || grid[c.x][c.y - 1] == Full) return false;
    }
    return true;
}

bool canMoveRight(const Grid &grid, const RealRock &rock) {
    for (auto c : rock) {
        if (c.y + 1 >= grid[c.x].size() || grid[c.x][c.y + 1] == Full) return false;
    }
    return true;
}

void moveRock(RealRock &rock, int x, int y) {
    for (Coord &c : rock) {
        c.x += x;
        c.y += y;
    }
}

void moveLeft(RealRock &rock) {
    moveRock(rock, 0, -1);
}

void moveRight(RealRock &rock) {
    moveRock(rock, 0, 1);
}

void moveDown(RealRock &rock) {
    moveRock(rock, 1, 0);
}

void move(const Grid &grid, RealRock &realRock, Move move) {
    switch(move) {
    case Left:
        if (canMoveLeft(grid, realRock)) { moveLeft(realRock); }
        break;
    case Right:
        if (canMoveRight(grid, realRock)) { moveRight(realRock); }
        break;
    }
}

void fall(const Grid &grid, RealRock &rock) {
    if (canMove(grid, rock)) {
        moveDown(rock);
    }
}

size_t computeNewHeight(const Grid &grid) {
    for (size_t i = 0; i < grid.size(); ++i) {
        for (State s : grid[i]) {
            if (s == Full) return grid.size() - i;
        }
    }
    return 0;
}

void updateGrid(Grid &grid, const RealRock &rock) {
    for (auto c : rock) grid[c.x][c.y] = Full;
}

void displayGrid(const Grid &grid) {
    for (auto v : grid) {
        for (auto s : v) {
            if (s == Full) cout << "#";
            else cout << ".";
        }
        cout << "\n";
    }
    cout << "\n";
}

void onestep(queue<Rock> &localRocks, Grid &grid, size_t height, queue<Move> &moves, RealRock &lastState, size_t &nmoves) {
    Rock rock = localRocks.front(); localRocks.pop();
    int xstart = insertLines(grid, height, rockSize(rock));
    auto realRock = createRock(grid, rock, xstart);
    while (true) {
        Move nextMove = moves.front(); moves.pop();
        moves.push(nextMove);
        move(grid, realRock, nextMove);
        ++nmoves;

        if (!canMove(grid, realRock)) 
            break;

        fall(grid, realRock);
    }

    updateGrid(grid, realRock);
    lastState = realRock;

    localRocks.push(rock);
}

struct Configuration {
    Configuration(Rock current, Rock next, size_t nmove, size_t iteration) : current(current), nextRock(next), nmove(nmove), iteration(iteration) {}
    bool operator==(const Configuration &oth) {
        return current == oth.current && oth.nextRock == nextRock && oth.nmove == nmove;
    }

    Rock current;
    Rock nextRock;
    size_t nmove;
    size_t iteration;
};

size_t compute(queue<Move> moves, size_t totalIterations) {
    size_t height = 0;
    size_t nmoves = 0;
    vector<Configuration> configurations;
    RealRock lastState;
    Grid grid = initGrid();
    queue<Rock> localRocks = rocks;

    for (size_t iteration = 1; iteration <= totalIterations; ++iteration) {
        onestep(localRocks, grid, height, moves, lastState, nmoves);

        Configuration config(localRocks.back(), localRocks.front(), nmoves % moves.size(), iteration);
        auto c = find(configurations.begin(), configurations.end(), config);
        if (c != configurations.end()) {
            //cout << "CYCLE FOUND: " << iteration << " " << c->iteration << " " << c->nmove << " " << config.nmove << "\n";
            //displayGrid(grid);
        } else {
            configurations.push_back(config);
        }
        height = computeNewHeight(grid);
    }
    //displayGrid(grid);
    return height;
    
    size_t yes = height * (totalIterations / 131);
    size_t lastHeight = height;
    for (size_t iteration = 1; iteration < totalIterations % 131; ++iteration) {
        onestep(localRocks, grid, height, moves, lastState, nmoves);
        height = computeNewHeight(grid);
    }
    //displayGrid(grid); 
    return yes + height - lastHeight;
}

int main() {
    auto in = parse(input());
    cout << "Part 1 answer: " << compute(in, 2022) << "\n";

    // 435 2144
    cout << "435 answer: " << compute(in, 435) << "\n";
    cout << "2144 answer: " << compute(in, 2144) << "\n";
    cout << "2800 answer: " << compute(in, 2799) << "\n";
    // 2620
    // 2620 * (1000000000000 / (2144 - 434)) = 1 532 163 740 000
    // 1000000000000%(1000000000000 / (2144 - 434)) = 1090
    // 1090 - 434 = 656 ~~>  = 1 532 163 742 758
    // ==> 1 532 163 742 758
    
    //cout << "Part 2 answer: " << compute(in, 1000000000000) << "\n";
}