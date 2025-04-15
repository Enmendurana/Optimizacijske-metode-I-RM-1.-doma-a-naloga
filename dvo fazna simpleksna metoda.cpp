/* * * * * * * * * * * * * * * * *
 *	author: leon fiser
 *	created: 03-02-2025
 * * * * * * * * * * * * * * * * */

#include <bits/stdc++.h>

using namespace std;

typedef long long ll;

const int maxm = int(1e3) + 10;

struct ulomek {
    ll num, den;

    ulomek(): num(0), den(1) {}

    ulomek(ll num, ll den) : num(num), den(den) {
        normalize();
    }

    void normalize() {
        if(num == 0) den = 1;
        if(den < 0) {
            den *= -1;
            num *= -1;
        }
        ll d = gcd(num,den);
        num /= d;
        den /= d;
    }

    ulomek operator-() const {
        return ulomek(-num,den);
    }

    ulomek& operator+=(const ulomek &drugi) {
        num = num * drugi.den + drugi.num * den;
        den = den * drugi.den;
        normalize();
        return *this;
    }
    ulomek& operator-=(const ulomek &drugi) {
        num = num * drugi.den - drugi.num * den;
        den = den * drugi.den;
        normalize();
        return *this;
    }
    ulomek& operator*=(const ulomek &drugi) {
        num = (num * drugi.num);
        den = (den * drugi.den);
        normalize();
        return *this;
    }
    ulomek& operator/=(const ulomek &drugi) {
        num = (num * drugi.den);
        den = (den * drugi.num);
        normalize();
        return *this;
    }

};

ulomek operator+(ulomek prvi, const ulomek &drugi) {
    prvi += drugi;
    return prvi;
}
ulomek operator-(ulomek prvi, const ulomek &drugi) {
    prvi -= drugi;
    return prvi;
}
ulomek operator*(ulomek prvi, const ulomek &drugi) {
    prvi *= drugi;
    return prvi;
}
ulomek operator/(ulomek prvi, const ulomek &drugi) {
    prvi /= drugi;
    return prvi;
}

bool operator==(const ulomek &prvi, const ulomek &drugi) {
    return (prvi.num == drugi.num) && (prvi.den == drugi.den);
}

bool operator<(const ulomek &prvi, const ulomek &drugi) {
    return (prvi.num * drugi.den) < (prvi.den * drugi.num);
}

bool operator<=(const ulomek &prvi, const ulomek &drugi) {
    return (prvi < drugi) || (prvi == drugi);
}

bool operator>(const ulomek &prvi, const ulomek &drugi) {
    return drugi < prvi;
}

bool operator>=(const ulomek &prvi, const ulomek &drugi) {
    return drugi <= prvi;
}

ostream& operator<<(ostream &out, const ulomek &prvi) {
    if(prvi.den == 1) {
        out << prvi.num;
    } else {
        out << prvi.num << "/" << prvi.den;
    }
    return out;
}

istream& operator>>(istream &in, ulomek &prvi) {
    in >> prvi.num; prvi.den = 1;
    return in;
}

ulomek a[maxm][maxm], b[maxm], c[maxm];

int base[maxm];

ulomek x[maxm][maxm];

void vstavi(int i, int j) {
    for(int k = 0; k < maxm; k++) {
        x[j][k] += x[j][i] * x[i][k];
    }
    x[j][i] = ulomek();
}

void izpostavi(int i, int j) {
    for(int k = 0; k < maxm; k++) {
        x[j][k] = x[i][k] / (-x[i][j]);
    }
    x[j][j] = ulomek();
    x[j][i] = (ulomek(1,1) / x[i][j]);
}

void print(int n) {

    cout << x[0][0] << "\n";

    for(int i = 1; i <= n; i++) {
        cout << (base[i] ? x[i][0] : ulomek()) << " ";
    }

    cout << "\n";

}

void simplex(int n, int m, int faza) {

    int w = n+m+1;

    if(faza == 1) {
        for(int i = 1; i <= m; i++) {
            x[n+i][0] = b[i]; base[n+i] = 1;
            for(int j = 1; j <= n; j++) {
                x[n+i][j] = -a[i][j];
            }
        }

        x[0][w] = ulomek(-1,1);

        for(int i = 1; i <= m; i++) {
            x[n+i][w] = ulomek(1,1);
        }

        int pivot = 0;

        for(int i = n+1; i <= n+m; i++) {
            if(!pivot && (x[i][0] < ulomek())) {
                pivot = i;
            }
            if(pivot && (x[i][0] < x[pivot][0])) {
                pivot = i;
            }
        }

        if(!pivot) return;

        izpostavi(pivot, w);
        base[w] = 1; base[pivot] = 0;

        for(int i = 0; i <= n+m+1; i++) {
            vstavi(w, i);
        }

    }
    if(faza == 2) {
        if(base[w]) {
            cout << "nedopusten\n"; return;
        }

        for(int i = 1; i <= n+m; i++) {
            if(base[i] && x[i][0] < ulomek()) {
                cout << "nedopusten\n"; return;
            }
        }

        for(int j = 1; j <= n; j++) {
            x[0][j] = c[j];
        }

        for(int j = 1; j <= m; j++) {
            x[0][n+j] = ulomek();
        }

        for(int i = 0; i <= n+m; i++) {
            x[i][w] = ulomek();
        }

        base[w] = 0;

        for(int i = 0; i <= n; i++) {
            if(base[i]) vstavi(i, 0);
        }
    }

    int tr = 1000;

    while(tr--) {

        int vhod = 0;

        for(int j = 1; j <= n+m; j++) {
            if(x[0][j] > ulomek()) {
                vhod = j; break;
            }
        }

        int pivot = 0;

        if(!vhod) {
            if(faza == 2) print(n);
            return;
        }

        for(int i = 1; i <= n+m+1; i++) {
            if(base[i] && x[i][vhod] < ulomek()) {
                if(!pivot || (x[pivot][0] / x[pivot][vhod] <= x[i][0] / x[i][vhod])) {
                    pivot = i;
                }
            }
        }

        if(!pivot) {
            cout << "neomejen\n"; return;
        }

        izpostavi(pivot, vhod);
        base[vhod] = 1; base[pivot] = 0;

        for(int i = 0; i <= n+m+1; i++) {
            vstavi(vhod, i);
        }

    }

    cerr << "error\n";

}

signed main() {

    int n, m; cin >> n >> m;

    for(int i = 1; i <= n; i++) {
        cin >> c[i];
    }

    for(int i = 1; i <= m; i++) {
        for(int j = 1; j <= n; j++) {
            cin >> a[i][j];
        }
    }

    for(int i = 1; i <= m; i++) {
        cin >> b[i];
    }

    simplex(n,m,1);
    simplex(n,m,2);

    return 0;

}
