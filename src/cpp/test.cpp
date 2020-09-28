#include <iostream>

int main() {

    {
        auto fst_prime = [=](auto fst) {
            return [=](auto a) {
                return [=](auto b) {
                    if (a == 0) {
                        return 0;
                    }
                    return ((fst)(fst)(a - 1))(b);
                };
            };
        };
        auto fst = (fst_prime)(fst_prime);
        auto main2_prime = [=](auto fst) {
            return ((fst)(1))(2);
        };
        auto main2 = (main2_prime)(fst);
        return main2;
        std::cout << main2 << std::endl;
    }


    return 0;
}