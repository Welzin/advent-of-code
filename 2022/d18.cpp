#include "utils.hpp"

enum class Case {
    Empty = 0,
    Lava,
    Outside,
};

constexpr size_t GSIZE = 30;
using Grid = array<array<array<Case, GSIZE>, GSIZE>, GSIZE>;

Grid initGrid() {
    Grid g;
    for (size_t i = 0; i < GSIZE; ++i) {
        for (size_t j = 0; j < GSIZE; ++j) {
            for (size_t k = 0; k < GSIZE; ++k) {
                g[i][j][k] = Case::Empty;
            }
        }
    }
    return g;
}

tuple<size_t, size_t, size_t> coordFromLine(string line) {
    auto vec = split(line, ',');
    return { stoi(vec[0]), stoi(vec[1]), stoi(vec[2]) };
}

Grid parse(const vector<string> &lines) {
    Grid grid = initGrid();
    for (auto line : lines) {
        auto [x, y, z] = coordFromLine(line);
        grid[x][y][z] = Case::Lava;
    }
    return grid;
}

size_t countNeighbors(const Grid &grid, int i, int j, int k, Case count) {
    size_t neighbors = 0;
    
    for (int x = max(0, i - 1); x <= min((int)GSIZE - 1, i + 1); ++x)
        if (x != i && grid[x][j][k] == count) neighbors += 1;
    for (int y = max(0, j - 1); y <= min((int)GSIZE - 1, j + 1); ++y)
        if (y != j && grid[i][y][k] == count) neighbors += 1;
    for (int z = max(0, k - 1); z <= min((int)GSIZE - 1, k + 1); ++z)
        if (z != k && grid[i][j][z] == count) neighbors += 1;

    return neighbors;
}

size_t part1(const Grid &grid) {
    size_t count = 0;
    for (size_t i = 0; i < GSIZE; ++i) {
        for (size_t j = 0; j < GSIZE; ++j) {
            for (size_t k = 0; k < GSIZE; ++k) {
                if (grid[i][j][k] == Case::Lava) {
                    // At most: 6 neighbors.
                    count += 6 - countNeighbors(grid, i, j, k, Case::Lava);
                }
            }
        }
    }
    return count;
}

size_t part2(Grid grid) {
    deque<tuple<size_t, size_t, size_t>> queue;
    queue.push_back({ 0, 0, 0 });
    while (queue.size()) {
        auto [i, j, k] = queue.front(); queue.pop_front();

        grid[i][j][k] = Case::Outside;
        for (int x = max(0, (int)i - 1); x <= (int)min(GSIZE - 1, i + 1); ++x) {
            tuple<size_t, size_t, size_t> coord = { x, j, k };
            if (x != (int)i && grid[x][j][k] == Case::Empty) {
                if (find(queue.begin(), queue.end(), coord) == queue.end())
                    queue.push_back(coord);
            }
        }
        for (int y = max(0, (int)j - 1); y <= (int)min(GSIZE - 1, j + 1); ++y){
            tuple<size_t, size_t, size_t> coord = { i, y, k };
            if (y != (int)j && grid[i][y][k] == Case::Empty) {
                if (find(queue.begin(), queue.end(), coord) == queue.end())
                    queue.push_back(coord);
            }
        }
        for (int z = max(0, (int)k - 1); z <= (int)min(GSIZE - 1, k + 1); ++z){
            tuple<size_t, size_t, size_t> coord = { i, j, z };
            if (z != (int)k && grid[i][j][z] == Case::Empty) {
                if (find(queue.begin(), queue.end(), coord) == queue.end())
                    queue.push_back(coord);
            }
        }
    }

    size_t count = 0;
    for (size_t i = 0; i < GSIZE; ++i) {
        for (size_t j = 0; j < GSIZE; ++j) {
            for (size_t k = 0; k < GSIZE; ++k) {
                if (grid[i][j][k] == Case::Lava) {
                    count += countNeighbors(grid, i, j, k, Case::Outside);
                    if (i == 0 || j == 0 || k == 0) count += 1;
                }
            }
        }
    }
    return count;
}

int main() {
    auto grid = parse(input());
    cout << "Part 1 answer: " << part1(grid) << "\n";
    cout << "Part 2 answer: " << part2(grid) << "\n";
}