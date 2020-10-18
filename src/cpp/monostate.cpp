#include <iostream>
#include <variant>

int main() {

    auto a = std::monostate{};
    std::cout << a << std::endl;


    return 0;
}
