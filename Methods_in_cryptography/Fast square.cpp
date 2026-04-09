#include <iostream>
#include <cstring>
#include <ctime>
#include <cassert>
#include <string>
#include <sstream>
#include <iomanip>
#include <chrono>

using namespace std;
using namespace std::chrono;

// typedef unsigned short BASE;
// typedef unsigned int DBASE;

typedef unsigned char BASE;
typedef unsigned short DBASE;

//typedef unsigned int BASE;
//typedef unsigned long long int DBASE;

#define BASE_SIZE (sizeof(BASE) * 8)

class BN {
    BASE* coef;
    int len;
    int maxlen;

    void len_norm() {
        len = maxlen;

        if (len == 0) len = 1;

        while (len > 0 && coef[len - 1] == 0) {
            len--;
        }

    }

public:

    BN(int mx = 1, bool mode = 0) {
        if (mx <= 0) mx = 1;

        maxlen = mx;

        coef = new BASE[maxlen];

        for (int i = 0; i < maxlen; ++i) {
            coef[i] = 0;
        }

        if (mode) {
            len = maxlen;
            for (int i = 0; i < len; ++i) {
                coef[i] = rand();
                if (sizeof(BASE) > 2) {
                    for (int j = 0; j < sizeof(BASE) / 2; ++j) {
                        coef[i] = (coef[i] << 16) | rand();
                    }
                }
            }
        }
        else {
            len = 1;
        }
        len_norm();
    }

    BN(const BN& a) {

        len = a.len;

        maxlen = a.maxlen;

        coef = new BASE[maxlen];

        for (int i = 0; i < maxlen; ++i) {
            coef[i] = a.coef[i];
        }
    }

    BN(BASE num) {
        maxlen = 1;
        len = 1;
        coef = new BASE[maxlen];
        coef[0] = num;
    }

    BN(const char* hexStr) {
        while (*hexStr == '0') hexStr++;

        int str_len = strlen(hexStr);
        if (str_len == 0) {
            len = 1;
            maxlen = 1;
            coef = new BASE[maxlen]();
            return;
        }

        len = (str_len - 1) / (BASE_SIZE / 4) + 1;
        maxlen = len;
        coef = new BASE[maxlen](); // Инициализация нулями

        int i = str_len - 1;
        int j = 0;
        int k = 0;

        while (i >= 0) {
            char c = tolower(hexStr[i]);
            BASE tmp;

            if (isdigit(c)) {
                tmp = c - '0';
            }
            else if (c >= 'a' && c <= 'f') {
                tmp = c - 'a' + 10;
            }
            else {
                throw invalid_argument("Invalid hexadecimal character");
            }

            coef[j] |= tmp << k;
            k += 4;
            if (k >= BASE_SIZE) {
                k = 0;
                j++;
            }
            i--;
        }

        len_norm();
    }

    ~BN() {
        delete[] coef;
    }

    BN& operator=(const BN& other) {
        if (this != &other) {
            delete[] coef;
            len = other.len;
            maxlen = other.maxlen;
            coef = new BASE[maxlen];
            for (int i = 0; i < maxlen; ++i) {
                coef[i] = other.coef[i];
            }
        }
        return *this;
    }

    BN& operator=(const BASE& num) {
        delete[] coef;
        len = 1;
        maxlen = 1;
        coef = new BASE[maxlen];
        coef[0] = num;
        return *this;
    }

    void Hex_input() {
        string s;
        cout << "Введите число в шестнадцатеричном формате: ";
        cin >> s;

        size_t start = s.find_first_not_of('0');
        if (start == string::npos) {
            *this = BN(0, 0);
            return;
        }

        for (size_t i = start; i < s.size(); ++i) {
            char c = tolower(s[i]);
            if (!(isdigit(c) || (c >= 'a' && c <= 'f'))) {
                throw invalid_argument("Неверный ввод: допустимы только цифры 0-9 и буквы a-f");
            }
        }

        *this = BN(s.c_str() + start);
    }

    // Функция для вывода числа в шестнадцатеричном формате
    void Hex_output() const {
        if (len == 0) {
            cout << "0";
            return;
        }

        bool flag = true;

        for (int j = len - 1; j >= 0; j--) {
            int k = BASE_SIZE - 4;
            while (k >= 0) {
                BASE tmp = (coef[j] >> k) & 0xF;

                if (flag && tmp == 0 && !(j == 0 && k == 0)) {
                    k -= 4;
                    continue;
                }

                flag = false;
                if (tmp <= 9) {
                    cout << static_cast<char>(tmp + '0');
                }
                else {
                    cout << static_cast<char>(tmp - 10 + 'a');
                }
                k -= 4;
            }
        }

        if (flag) {
            cout << "0";
        }
    }


    friend ostream& operator << (ostream& os, const BN& number) {
        if (number.len == 0) {
            os << "0";
            return os;
        }

        bool flag = true; // флаг для пропуска ведущих нулей

        for (int j = number.len - 1; j >= 0; j--) {
            int k = BASE_SIZE - 4;
            while (k >= 0) {
                BASE tmp = (number.coef[j] >> k) & 0xF;

                if (flag && tmp == 0 && !(j == 0 && k == 0)) {
                    k -= 4;
                    continue;
                }

                flag = false;
                if (tmp <= 9) {
                    os << static_cast<char>(tmp + '0'); //static_cast - static_cast используется для простого преобразования целочисленного значения в символьный тип
                }
                else {
                    os << static_cast<char>(tmp - 10 + 'a');
                }
                k -= 4;
            }
        }

        if (flag) { //если цифры были нулями
            os << "0";
        }

        return os;
    }

    friend istream& operator >> (istream& is, BN& number) {
        string s;
        is >> s;

        size_t start = s.find_first_not_of('0'); //пропуск ведущих нулей
        if (start == string::npos) { //Строка состоит только из нулей
            number = BN("0");
            return is;
        }

        for (size_t i = start; i < s.size(); ++i) {
            char c = tolower(s[i]);
            if (!(isdigit(c) || (c >= 'a' && c <= 'f'))) {
                throw invalid_argument("Неверный ввод");
            }
        }

        number = BN(s.c_str() + start);
        return is;
    }

    friend bool operator == (const BN& a, const BN& b) {
        if (a.len != b.len) return false;

        for (int i = a.len - 1; i >= 0; --i) {
            if (a.coef[i] != b.coef[i]) {
                return false;
            }
        }

        return true;
    }

    friend bool operator != (const BN& a, const BN& b) {
        return !(a == b);
    }

    friend bool operator < (const BN& a, const BN& b) {
        if (a.len != b.len)
            return a.len < b.len;

        for (int i = a.len - 1; i >= 0; --i) {
            if (a.coef[i] > b.coef[i])
                return false;
            if (a.coef[i] < b.coef[i])
                return true;
        }

        return false;
    }

    friend bool operator > (const BN& a, const BN& b) {
        return (b < a);
    }

    friend bool operator <= (const BN& a, const BN& b) {
        return !(b < a);
    }

    friend bool operator >= (const BN& a, const BN& b) {
        return !(a < b);
    }

    friend BN operator + (const BN& a, const BN& b) {
        int l = max(a.len, b.len) + 1;
        int t = min(a.len, b.len);
        BN result(l + 1);

        DBASE tmp;
        BASE k = 0;
        int j;

        for (j = 0; j < t; j++) {
            tmp = (DBASE)a.coef[j] + (DBASE)b.coef[j] + (DBASE)k;
            result.coef[j] = (BASE)tmp;
            k = (BASE)(tmp >> BASE_SIZE);
        }

        for (; j < a.len; j++) {
            tmp = (DBASE)a.coef[j] + (DBASE)k;
            result.coef[j] = (BASE)tmp;
            k = (BASE)(tmp >> BASE_SIZE);
        }

        for (; j < b.len; j++) {
            tmp = (DBASE)b.coef[j] + (DBASE)k;
            result.coef[j] = (BASE)tmp;
            k = (BASE)(tmp >> BASE_SIZE);
        }
        if (k == 0) {
            result.len = j;
        }
        else {
            result.coef[j] = k;
            result.len = j + 1;
        }

        result.len_norm();

        return result;
    }

    BN& operator += (const BN& a) {
        *this = *this + a;
        return *this;
    }

    friend BN operator + (const BN& a, BASE b) {
        int l = a.len + 1;
        int t = a.len;
        BN result(l);

        DBASE tmp = (DBASE)a.coef[0] + (DBASE)b;
        result.coef[0] = (BASE)tmp;
        BASE k = tmp >> BASE_SIZE;

        int j;

        for (j = 1; j < t; j++) {
            tmp = (DBASE)a.coef[j] + (DBASE)k;
            result.coef[j] = (BASE)tmp;
            k = (BASE)(tmp >> BASE_SIZE);
        }


        result.coef[j] = k;
        result.len_norm();

        return result;
    }

    // Операция вычитания (предполагается, что a >= b)
    friend BN operator - (const BN& a, const BN& b) {
        if (a < b) {
            throw invalid_argument("a < b");
        }

        int l = a.len;
        BN result(l);

        DBASE tmp;
        DBASE k = 0;
        int j;

        for (j = 0; j < b.len; j++) {
            tmp = ((DBASE)1 << BASE_SIZE) | (DBASE)a.coef[j];
            tmp -= (DBASE)b.coef[j] + (DBASE)k;
            result.coef[j] = (BASE)tmp;
            k = (DBASE)!(tmp >> BASE_SIZE);
        }

        for (; j < a.len; j++) {
            tmp = ((DBASE)1 << BASE_SIZE) | (DBASE)a.coef[j];
            tmp -= (DBASE)k;
            result.coef[j] = (BASE)tmp;
            k = (DBASE)!(tmp >> BASE_SIZE);
        }

        result.len = a.len;

        result.len_norm();

        return result;
    }

    BN& operator -= (const BN& a) {
        *this = *this - a;
        return *this;
    }

    friend BN operator - (const BN& a, BASE b) {
        int l = a.len;
        BN result(l);

        DBASE tmp = ((DBASE)1 << BASE_SIZE) | (DBASE)a.coef[0];
        tmp = tmp - (DBASE)b;
        result.coef[0] = (BASE)tmp;
        BASE k = 1 - (BASE)(tmp >> BASE_SIZE);;
        int j;

        for (j = 1; j < a.len; j++) {
            tmp = ((DBASE)1 << BASE_SIZE) | (DBASE)a.coef[j];
            tmp -= (DBASE)k;
            result.coef[j] = (BASE)tmp;
            k = 1 - (BASE)(tmp >> BASE_SIZE);
        }

        result.len_norm();

        return result;
    }

    friend BN operator * (const BN& a, BASE b) {
        if (b == 0) return BN(1);

        BN result((a.len + 1));
        DBASE tmp;
        BASE k = 0;
        int j = 0;

        for (; j < a.len; j++) {
            tmp = (DBASE)a.coef[j] * (DBASE)b + (BASE)k;
            result.coef[j] = (BASE)tmp;
            k = (BASE)(tmp >> BASE_SIZE);
        }

        if (k == 0) {
            result.len = j;
        }
        else {
            result.coef[j] = k;
            result.len = j + 1;
        }

        result.len_norm();

        return result;
    }

    friend BN operator*(const BN& a, const BN& b) {

        int m = a.len;
        int n = b.len;
        int l = m + n;
        BN result(l);

        DBASE tmp;
        int j = 0;

        for (; j < n; j++) {
            if (b.coef[j] == 0) continue;

            BASE k = 0;
            int i = 0;

            for (; i < m; i++) {
                tmp = (DBASE)a.coef[i] * (DBASE)b.coef[j] + (DBASE)result.coef[i + j] + (DBASE)k;
                result.coef[i + j] = (BASE)tmp;
                k = BASE(tmp >> BASE_SIZE);
            }
            result.coef[j + m] = k;

        }
        result.len = l;
        result.len_norm();
        return result;
    }

    friend pair<BN, BN> div(const BN& a, const BASE v) {
        if (v == 0) {
            throw invalid_argument("Division by zero");
        }

        int n = a.len;
        BN first(n);
        DBASE tmp;
        DBASE r = 0;

        for (int j = n - 1; j >= 0; j--) {
            tmp = ((DBASE)r << BASE_SIZE) + (DBASE)a.coef[j];
            first.coef[j] = (BASE)(tmp / (DBASE)v);
            r = (BASE)(tmp % (DBASE)v);
        }
        first.len = a.len;

        first.len_norm();

        BN second(1);
        
        second = BN((BASE)r);
        
        return make_pair(first, second);
    }

    friend BN operator / (const BN& a, const BASE v) {
        return div(a, v).first;
    }

    friend BN operator % (const BN& a, const BASE v) {
        return div(a, v).second;
    }




    friend pair<BN, BN> div(const BN& u, const BN& v) {
        if (v.len == 1 && v.coef[0] == 0) {
            throw invalid_argument("Division by zero");
        }

        if (u < v) {
            return make_pair(BN(0), u);
        }

        if (u == v) {
            BN q(1);
            q.coef[0] = 1;
            return make_pair(q, BN(0));
        }

        if (v.len == 1) {
            return make_pair((u / v.coef[0]), (u % v.coef[0]));
        }

        int m = u.len - v.len;

        DBASE b = ((DBASE)1 << BASE_SIZE);

        DBASE d = b / (DBASE)(v.coef[v.len - 1] + (BASE)1);
        if (d == 0) d = 1;
        //cout << hex << int(d) << endl;
        BN u_norm = u * d;
        BN v_norm = v * d;
        //cout << v_norm << endl;

        if (u_norm.len == u.len) {
            BASE* new_coef = new BASE[u_norm.len + 1];
            copy(u_norm.coef, u_norm.coef + u_norm.len, new_coef);
            new_coef[u_norm.len] = 0;
            delete[] u_norm.coef;
            u_norm.coef = new_coef;
            u_norm.len++;
            u_norm.maxlen = u_norm.len;
        }


        BN q(m + 1);

        for (int j = m; j >= 0; j--) {
            DBASE num = ((DBASE)u_norm.coef[j + v_norm.len] << BASE_SIZE) + u_norm.coef[j + v_norm.len - 1];
            DBASE q_hat = (DBASE)(num / v_norm.coef[v_norm.len - 1]);
            DBASE r_hat = (DBASE)(num % v_norm.coef[v_norm.len - 1]);
            while (q_hat >= b || (q_hat * v_norm.coef[v_norm.len - 2] > ((DBASE)r_hat << BASE_SIZE) + u_norm.coef[j + v_norm.len - 2])) {
                q_hat--;
                r_hat += v_norm.coef[v_norm.len - 1];
                q.coef[j] = (BASE)q_hat;
                q.len = m + 1;
                if (r_hat >= b) break;
            }


            BN u_part(v_norm.len + 1);
            for (int i = 0; i <= v_norm.len; i++) {
                u_part.coef[i] = u_norm.coef[j + i];
            }
            u_part.len = v_norm.len + 1;

            BN product = v_norm * q_hat;

            if (u_part < product) {

                q_hat--;
                product = v_norm * (BASE)q_hat;
            }



            BN diff = u_part - product;

            for (int i = 0; i <= v_norm.len; i++) {
                u_norm.coef[j + i] = (i < diff.len) ? diff.coef[i] : 0;
            }

            q.coef[j] = (BASE)q_hat;

        }
        q.len = m + 1;
        q.len_norm();
       
        BN rem = u - q * v;
        rem.len_norm();

        return make_pair(q, rem);
    }

    friend BN operator / (const BN& a, const BN& b) {
        return div(a, b).first;
    }

    friend BN operator % (const BN& a, const BN& b) {
        return div(a, b).second;
    }

    void Decimal_input() {
        int j = 0;
        string s;
        cout << "Decimal: ";
        getline(cin, s);
        int k = s.length();
        BASE t = 0;

        int b = BASE_SIZE;

        BN bNum((k - 1) / (b / 4) + 1);

        while (j < k) {
            if ('0' > s[j] || s[j] > '9') {
                throw invalid_argument("Invalid arguments");
            }
            t = s[j] - '0'; //преобразование из строки в число
            bNum = bNum * ((BASE)10);

            BN newNum;
            newNum.coef[0] = (BASE)t;
            bNum += newNum;
            j++;
        }

        bNum.len = bNum.maxlen;
        while (bNum.len > 1 && bNum.coef[bNum.len - 1] == 0) {
            bNum.len--;

        }
        *this = bNum;
    }


    friend BN square(const BN& num) {
        BN res(2 * num.len); //первый пункт алгоритма
        int j;
        DBASE cu = 0;
        DBASE uv = 0;
        BASE v = 0;
        DBASE tmp = 0;
        res.len = res.maxlen;
        
        for (int i = 0; i < num.len; i++) { // 2 пункт
            uv = (DBASE)res.coef[2 * i] + (DBASE)num.coef[i] * (DBASE)num.coef[i]; //вычисляем uv пункт 2.1
            res.coef[2 * i] = (BASE)uv;
            cu = (uv >> BASE_SIZE); //c=0 u=u
            
            for (j = i + 1; j < num.len; j++) { //geyrn 2.2
                tmp = static_cast<DBASE>(static_cast<DBASE>(static_cast<BASE>(res.coef[i + j]))
                + static_cast<DBASE>(static_cast<BASE>(static_cast<DBASE>(num.coef[i])
                * static_cast<DBASE>(num.coef[j])) * 2)
                + static_cast<DBASE>(static_cast<BASE>(cu)));

                v = static_cast<BASE>(tmp);

                cu = static_cast<DBASE>(static_cast<DBASE>((
                static_cast<DBASE>(static_cast<DBASE>(num.coef[i])
                * static_cast<DBASE>(num.coef[j])) >> BASE_SIZE)
                * static_cast<DBASE>(2))
                + static_cast<DBASE>(static_cast<DBASE>(cu) >> BASE_SIZE)
                + static_cast<DBASE>(static_cast<DBASE>(tmp) >> BASE_SIZE));

                res.coef[i + j] = v; // y=v
            }
            res.coef[i + num.len] += static_cast<BASE>(cu); //добавляем  u
            res.coef[i + num.len + 1] += static_cast<BASE>(cu >> BASE_SIZE); // пункт 2.3 1j добавляем c
        }

        res.len = res.maxlen;
        for (int i = 2 * num.len - 1; i > -1; i--) { //нормализация длины
            if (res.coef[i] == 0) {
                res.len--;
            } else {
                break;
            }
        }
        
        if (res.len == 0) res.len = 1;
        return res;
    }

    
     /*void Decimal_output() {
         BN temp = *this;
         string s;
         BN zero(1);

         while (temp != zero) {
             BASE digit = temp % 10;
             s.push_back(digit + '0');
             temp = temp / 10;
         }

         reverse(s.begin(), s.end());
         cout << "Decimal = " << s << endl;
     }*/
};

// void Test() {
//     int max_len = 1000;
//     int N = 1000;
//     BN A, D, Q, R;
//     int count = 0;
//     do {
//         int len_A = rand() % max_len + 1;
//         int len_d = rand() % max_len + 1;
//         BN E(len_A, 1);
//         cout << count << " " << len_A << " " << len_d << "\n";
//         BN G(len_d, 1);
//         if (G == 0) {
//             G = BN(len_d, 1);
//         }
//         A = E;
//         D = G;
//         try {
//             Q = A / D;
//             R = A % D;
//         }
//         catch (const invalid_argument& e) {
//             cout << "Error: " << e.what() << endl;
//             cout << "A = " << A << endl;
//             cout << "D = " << D << endl;
//             break;
//         }
//         count++;
//     } while (A == Q * D + R && A - R == Q * D && R < D && --N >= 0);
//     cout << count << "\n";
// }

int main() {
    
    BN bn1(6,1);
    
     cout << bn1 << endl;
     time_point begin1 = system_clock::now();
     BN bn2 = bn1*bn1;
     cout << bn2 << endl;
     time_point end1 = system_clock::now();
     auto diff1 = end1 - begin1;
     cout << duration <double, milli> (diff1).count() << " ms" << endl;

    
     time_point begin2 = system_clock::now();
     BN bn3 = square(bn1);
     time_point end2 = system_clock::now();
     auto diff2 = end2 - begin2;

     cout << duration <double, milli> (diff2).count() << " ms" << endl;
     cout << bn3 << endl;
     if (bn2 == bn3) {
         cout << "Values are equals" << endl;
     } else {
         cout << "Values are not equal" << endl;
     }
}
