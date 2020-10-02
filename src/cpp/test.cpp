#include <iostream>

int main() {
auto v =
([=](auto main) {
    return ([=](auto g) {
        return ([=](auto f) {
            return (((main)(main))(g))(f);
        })([=](auto main) {
            return [=](auto g) {
                return [=](auto f) {
                    return [=](auto a) {
                        if (a == 0) {
                            return 0;
                        }
                        std::cout << a << " f" << std::endl;
                        return ((((g)(main))(g))(f))(a - 1);
                    };
                };
            };
        });
    })([=](auto main) {
        return [=](auto g) {
            return [=](auto f) {
                return [=](auto a) {
                    if (a == 0) {
                        return 0;
                    }
                    std::cout << a << " g" << std::endl;
                    return ((((f)(main))(g))(f))(a - 1);
                };
            };
        };
    });
})([=](auto main) {
    return [=](auto g) {
        return [=](auto f) {
            return ((((f)(main))(g))(f))(5);
        };
    };
});

    std::cout << v << std::endl;

    return 0;
}