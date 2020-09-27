#include <iostream>

int main() {

    {
        auto add_prime = (([=](auto fst) {
            return [=](auto snd) {
                return (fst + snd);
            };
        })(a))(b);

        auto main_prime = [=](auto add) {
            return ((add)(1))(2);
        };

        return main;
    }


    return 0;
}