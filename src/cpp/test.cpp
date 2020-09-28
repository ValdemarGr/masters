#include <iostream>

int main() {

    {
    auto add_prime = [=](auto b) {
        return [=](auto a) {
            return ((a) + (b));
        };
    };
    auto add = add_prime;

    auto main_prime = [=](auto add) {
        return ((add)(1))(2);
    };
    auto main = (main_prime)(add);

    return main;
    }


    return 0;
}