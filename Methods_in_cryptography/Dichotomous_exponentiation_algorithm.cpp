#include <iostream>
#include <chrono>
#include <cmath>

using namespace std;
using namespace std::chrono;

// Алгоритм возведения в степень справа налево (быстрое возведение)
long long algor(long long x, long long y) {
    long long z = x;
    
    // Находим количество бит в числе y
    int bit_length = 0;
    long long temp = y;
    while (temp > 0) {
        bit_length++;
        temp >>= 1;
    }
    
    // Проходим по битам справа налево
    for (int i = bit_length - 2; i >= 0; i--) {
        z = z * z;
        if ((y >> i) & 1) {
            z *= x;
        }
    }
    return z;
}

// Стандартное возведение в степень через цикл
long long standart(long long x, long long y) {
    long long res = 1;
    for (long long i = 0; i < y; i++) {
        res *= x;
    }
    return res;
}


void test(long long x, long long y) {
    
    // Тест algor
    auto start = high_resolution_clock::now();
    long long result_1 = algor(x, y);
    auto end = high_resolution_clock::now();
    auto duration_1 = duration_cast<nanoseconds>(end - start);
    cout << duration_1.count() << " ns\n";
    cout << result_1 << endl;
    
    // Тест standart
    start = high_resolution_clock::now();
    long long result_2 = standart(x, y);
    end = high_resolution_clock::now();
    auto duration_2 = duration_cast<nanoseconds>(end - start);
    cout << duration_2.count() << " ns\n";
    cout << result_2 << endl;
    
    if (result_1 == result_2) {
        cout << "Result Normal\n";
    } else {
        cout << "Result not Normal\n";
    }
}

int main() {
    long long x, y;
    
    cout << "Введите x: ";
    cin >> x;
    
    cout << "Введите y: ";
    cin >> y;
    
    test(x, y);
    
    return 0;
}
