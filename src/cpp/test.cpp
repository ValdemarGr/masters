#include <iostream>

int main() {
    auto v = ([=](auto main_prime) {
                 return ([=](auto add_prime) {
                     return ([=](auto main) {
                         return ([=](auto add) {
                             return main;
                         })(((add_prime)(main_prime))(add_prime));
                     })(((main_prime)(main_prime))(add_prime));
                 })([=](auto main_prime) {
                     return [=](auto add_prime) {
                         return ([=](auto main) {
                             return ([=](auto add) {
                                 return [=](auto a) {
                                     return [=](auto b) {
                                         return ((a) + (b));
                                     };
                                 };
                             })(((add_prime)(main_prime))(add_prime));
                         })(((main_prime)(main_prime))(add_prime));
                     };
                 });
             })([=](auto main_prime) {
                 return [=](auto add_prime) {
                     return ([=](auto main) {
                         return ([=](auto add) {
                             return ((add)(2))(4);
                         })(((add_prime)(main_prime))(add_prime));
                     })(((main_prime)(main_prime))(add_prime));
                 };
             });

    std::cout << v << std::endl;

    return 0;
}