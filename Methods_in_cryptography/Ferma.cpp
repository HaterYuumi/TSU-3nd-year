#include <iostream>
#include <random>
#include <cmath>
#include <vector>

using namespace std;

// Функция Эйлера
long long fi(long long n) {
    long long f = n;
    if (n % 2 == 0) {
        while (n % 2 == 0) {
            n = n / 2;
        }
        f = f / 2;
    }
    long long i = 3;
    while (i * i <= n) {
        if (n % i == 0) {
            while (n % i == 0) {
                n = n / i;
            }
            f = f / i;
            f = f * (i - 1);
        }
        i = i + 2;
    }
    if (n > 1) {
        f = f / n;
        f = f * (n - 1);
    }
    return f;
}

// Функция для вычисления вероятности ошибки
void error_f(long long n, int t) {
    double result = pow(static_cast<double>(fi(n)) / n, t);
    cout << "Вероятность ошибки = " << result << endl;
}

// Функция для возведения в степень по модулю (быстрое возведение в степень)
long long pow_mod(long long a, long long n, long long mod) {
    long long result = 1;
    a = a % mod;
    while (n > 0) {
        if (n & 1) {
            result = (result * a) % mod;
        }
        a = (a * a) % mod;
        n = n >> 1;
    }
    return result;
}

// Тест Ферма
int test_ferma(long long n, int t) {
    // Инициализация генератора случайных чисел
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<long long> dis(2, n - 2);
    
    for (int i = 1; i < t; i++) {
        long long a = dis(gen);
        long long r = pow_mod(a, n - 1, n);
        if (r != 1) {
            return 1;
        }
    }
    return 0;
}

// Обёртка для теста Ферма
void wrapper_test_ferma(long long n, int t) {
    if (n <= 3) {
        cout << "Число не подходит под условия\n" << endl;
    } else {
        int res = test_ferma(n, t);
        if (res == 1) {
            cout << "составное" << endl;
        } else {
            cout << "простое" << endl;
            // error_f(n, t); // Раскомментировать при необходимости
        }
    }
}

// Тестовая функция
void test() {
    vector<long long> test_data = {2, 3, 5, 7, 11, 1, 9, 10, 12, 14, 1, 561, 8911, 10585, 15841, 29341, 41041};
    int t = 12;
    // после изменения параметра надёжности выше 3 последние 2 числа Кармайкла определяются
    for (long long i : test_data) {
        wrapper_test_ferma(i, t);
    }
}

int main() {
    cout << "Hello Ferma Test" << endl;
    // wrapper_test_ferma(348251240609926627320927902551, 5); // Это число слишком большое для long long
    test();
    
    return 0;
}
