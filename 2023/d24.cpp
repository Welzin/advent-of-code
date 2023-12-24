#include "utils.hpp"

#define REP(i,a,b) for (int i = (a); i <= (b); ++i)
#define REPD(i,a,b) for (int i = (a); i >= (b); --i)
#define FORI(i,n) REP(i,1,n)
#define FOR(i,n) REP(i,0,int(n)-1)
#define mp make_pair
#define pb push_back
#define pii pair<int,int>
#define vi vector<int>
#define ll long long
#define SZ(x) int((x).size())
#define DBG(v) cerr << #v << " = " << (v) << endl;
#define FOREACH(i,t) for (auto i = t.begin(); i != t.end(); ++i)
#define fi first
#define se second

struct Point {
  utils::LL x; utils::LL y; utils::LL z;
  Point(utils::LL x, utils::LL y, utils::LL z) : x(x), y(y), z(z) {}
  Point(const string &s) {
    auto coords = utils::split(s, ", ");
    x = stol(coords[0]);
    y = stol(coords[1]);
    z = stol(coords[2]);
  }
  Point operator-(const Point &oth) const {
    return Point(x - oth.x, y - oth.y, z - oth.z);
  }
  Point operator+(const Point &oth) const {
    return Point(x + oth.x, y + oth.y, z + oth.z);
  }
};
ostream &operator<<(ostream &out, const Point &p) {
  out << "Point(" << p.x << ", " << p.y << ", " << p.z << ")";
  return out;
}
using Vector3D = Point;

vector<pair<Point, Vector3D>> parse(const vector<string> &inputs) {
  vector<pair<Point, Vector3D>> points;
  for (auto input : inputs) {
    auto spl = utils::split(input, " @ ");
    points.push_back(make_pair(Point(spl[0]), Vector3D(spl[1])));
  }
  return points;
}

double dot(const Point &p, const Point &q) {
  return p.x * q.x + p.y * q.y;
}

bool intersectRange(const Point &p, const Vector3D &r, const Point &q,
                    const Vector3D &s, double cmin, double cmax) {
  // Compute coeff by solving the linear system (by hand)
  double d = p.y + (double)((q.x - p.x) * r.y) / r.x - q.y;
  double n = s.y - (double)(s.x * r.y) / r.x;
  if (abs(n) < 1e-9) return false;
  double mu = d / n;
  double x = q.x + mu * s.x;
  double y = q.y + mu * s.y;

  // If the vector between p or q and the point found is negatively correlated with the
  // velocity vectors, they intersect in the past.
  if (dot(Point(x - p.x, y - p.y, p.z), r) < 0 ||
      dot(Point(x - q.x, y - q.y, q.z), s) < 0) {
    return false;
  }
  return cmin <= x && x <= cmax && cmin <= y && y <= cmax;
}

void part1(const vector<pair<Point, Vector3D>> &points) {
  utils::LL total = 0;
  for (size_t i = 0; i < points.size(); ++i) {
    auto [p, r] = points[i];
    for (size_t j = i + 1; j < points.size(); ++j) {
      auto [q, s] = points[j];
      total += intersectRange(p, r, q, s, 200000000000000, 400000000000000);
    }
  }
  cout << total << "\n";
}

double a[6][6];
double b[6];

void populate(int offset, const vector<Point> &p,
              const vector<Vector3D> &v, int i, int j) {
  a[offset][0] = 0.0;
  a[offset][1] = v[i].z - v[j].z;
  a[offset][2] = v[j].y - v[i].y;
  a[offset][3] = 0.0;
  a[offset][4] = p[j].z - p[i].z;
  a[offset][5] = p[i].y - p[j].y;
  b[offset] = p[i].z * v[i].y + p[j].y * v[j].z - p[i].y * v[i].z - p[j].z * v[j].y;

  a[offset + 1][0] = v[j].z - v[i].z;
  a[offset + 1][1] = 0.0;
  a[offset + 1][2] = v[i].x - v[j].x;
  a[offset + 1][3] = p[i].z - p[j].z;
  a[offset + 1][4] = 0.0;
  a[offset + 1][5] = p[j].x - p[i].x;
  b[offset + 1] = p[j].z * v[j].x + p[i].x * v[i].z - p[i].z * v[i].x - p[j].x * v[j].z;

  a[offset + 2][0] = v[i].y - v[j].y;
  a[offset + 2][1] = v[j].x - v[i].x;
  a[offset + 2][2] = 0.0;
  a[offset + 2][3] = p[j].y - p[i].y;
  a[offset + 2][4] = p[i].x - p[j].x;
  a[offset + 2][5] = 0.0;
  b[offset + 2] = p[j].x * v[j].y + p[i].y * v[i].x - p[i].x * v[i].y - p[j].y * v[j].x;
}

const long double EPS = 1e-9;
inline bool IsZero(double x) { return x >= -EPS && x <= EPS; }

vector<double> gauss() {
  int m = 6, n = 6, k, r;
  vector<double> x(n, 0);
  vi q;
  for (k = 0; k < min(m, n); k++) {
    int i, j;
    for (j = k; j < n; j++)
      for (i = k; i < m; ++i)
	if (!IsZero(a[i][j])) goto found;
    break;
  found:
    if (j != k)
      FOR(t, m) {
	double tmp = a[t][j];
	a[t][j] = a[t][k];
	a[t][k] = tmp;
      }
    q.pb(j);
    if (i != k) {
      for (int l = 0; l < n; ++l) {
	double tmp = a[i][l];
	a[i][l] = a[k][l];
	a[k][l] = tmp;
      }
      double tmp = b[i];
      b[i] = b[k];
      b[k] = tmp;
    }
    REP(j, k + 1, m - 1) if (!IsZero(a[j][k])) {
      double l = (a[j][k] / a[k][k]);
      REP(i, k, n - 1) a[j][i] = a[j][i] - (l * a[k][i]);
      b[j] = b[j] - (l * b[k]);
    }
  }
  r = k;
  REP(k, r, m - 1) if (!IsZero(b[k])) return x;
  REPD(k, r - 1, 0) {
    double s = b[k];
    REP(j, k + 1, r - 1) s = s - (a[k][j] * x[j]);
    x[k] = s / a[k][k];
  }
  return x;
}

void part2(const vector<Point> &p, const vector<Vector3D> &v) {
  // Some remarks:
  //  * we would want to do a gaussian elimination on everything. It is not
  //  possible as the system is not linear. If we linearize it, we have an
  //  underspecified system, which leads to an infinite number of solutions, so
  //  we don't want that.
  //  * what we do is that we remark that, if we call (q, v) the initial point /
  //  velocity and p[i], v[i] the ith point and velocity of `points`, and t[i]
  //  the time where the ith rock is disintegrated, we have that q + t[i]v =
  //  p[i] + t[i]v[i].
  //  * Thus (p[i] - q) = t[i](v - v[i]), i.e., they are colinear (as they are
  //  vectors). What I found is that we can exploit this information: if two
  //  vectors are colinear, their crossproduct is 0. Then, we actually want
  //  cross(p[i] - q, v - v[i]) = 0.
  //  * However, it is not so straightforward, as when doing the actual
  //  computations, we have a nonlinear system that comes in. We cannot
  //  linearize it without creating too much variables, thus we use another
  //  property: we also have cross(p[j] - q, v - v[j]) = 0.
  //  * As such, we have cross(p[i] - q, v - v[i]) = cross(p[j] - q, v - v[j])
  //  and there it is: the nonlinear things go away if we do:
  //            cross(p[i] - q, v - v[i]) - cross(p[j] - q, v - v[j]) = 0
  //  * We have 6 unknowns, thus we can take 3 points i, j, k and generate the 6
  //  equations needed, then solve it using classical Gaussian elimination.
  int i = 0, j = 1, k = 2;

  // Populate rows (I have computed all the coefficients by hand)
  populate(0, p, v, i, j);
  populate(3, p, v, i, k);

  // Gaussian elimination and get qx, qy, qz, vx, vy, vz (in this order).
  vector<double> x = gauss();
  cout << fixed << (size_t)-(x[0] + x[1] + x[2]) << "\n";
}

int main() {
  auto inputs = utils::input();
  vector<pair<Point, Vector3D>> points = parse(inputs);
  auto start = chrono::high_resolution_clock::now();
  part1(points);
  utils::display_time<chrono::milliseconds>(start, "Part 1 took: ", "ms");
  vector<Point> p;
  vector<Vector3D> v;
  for (auto [point, velocity] : points) {
    p.push_back(point);
    v.push_back(velocity);
  }
  start = chrono::high_resolution_clock::now();
  part2(p, v);
  utils::display_time<chrono::milliseconds>(start, "Part 2 took: ", "ms");
}
