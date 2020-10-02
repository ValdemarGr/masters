#include <iostream>

int main() {
    auto v = ([=](auto main_prime) {
                 return ([=](auto add_prime) {
                     return ([=](auto main) {
                         return ([=](auto add) {
                             return main;
                         })(((add_prime)(main_prime))(add_prime));
                     })(((main_prime)(main_prime))(add_prime));
                 })([=](auto main) {
                     return [=](auto add) {
                         return [=](auto a) {
                             return [=](auto b) {
                                 return ((a) + (b));
                             };
                         };
                     };
                 });
             })([=](auto main) {
                 return [=](auto add) {
                     return ([=](auto c_prime) {
                         return ([=](auto c) {
                             return c;
                         })((c_prime)(c_prime));
                     })([=](auto c) {
                         return ((add)(1))(2);
                     });
                 };
             });

    std::cout << v(4)(5) << std::endl;

    return 0;
}