#include <iostream>

int main() {
    auto f_impl = ([=](auto f) {
        return ([=](auto g) {
            return ([=](auto a){
                std::cout << "in f " << a << std::endl;
                std::cout << "f " << &f << std::endl;
                if (a == 0) {
                   return 0;
                }
                return g(g)(f)(a - 1);
            });
        });
    });

    auto g_impl = ([=](auto g) {
        return ([=](auto f) {
            return ([=](auto a){
                std::cout << "in g " << a << std::endl;
                std::cout << "f " << &f << std::endl;
                if (a == 0) {
                   return 0;
                }
                return f(f)(g)(a - 1);
            });
        });
    });

    auto v = ([=](auto f_prime) {
        return ([=](auto g_prime) {
            return ([=](auto f) {
                return ([=](auto g) {
                    return f(50);
                })(g_prime(g_prime)(f_prime));
            })(f_prime(f_prime)(g_prime));
        })(g_impl);
    })(f_impl);

    std::cout << v << std::endl;

    return 0;
}