#include <iostream>

int main() {

    auto f_prime = [=](auto& f) {
        return [=](auto& g) {
            return [=](auto a){
                return [=](auto b){
                    if (a == 0 || b == 0) {
                        return 0;
                    }
                    std::cout << a + b << std::endl;
                    g(g)(f)(a - 1)(b - 1);
                };
            };
        };
    };

    auto g_prime = [=](auto& g) {
        return [=](auto& f) {
            return [=](auto a){
                return [=](auto b){
                    if (a == 0 || b == 0) {
                        return 0;
                    }
                    std::cout << a + b << std::endl;
                    f(f)(g)(a - 1)(b - 1);
                };
            };
        };
    };

    auto f = f_prime(f_prime)(g_prime);
    auto g = g_prime(g_prime)(f_prime);

    f(10)(5);

    return 0;
}