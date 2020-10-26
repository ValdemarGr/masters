

#include <iostream>
#include <variant>

int main() {
  auto v = ([=](auto IF) {
    return ([=](auto main) { return (main)(main); })([=](auto main) {
      return ([=](auto b) { return b; })([=](auto b) { return ((4) + (8)); });
    });
  })([=](auto exp) {
    return [=](auto fst) {
      return [=](auto snd) { return (exp) ? (fst) : (snd); };
    };
  });

  std::cout << v << std::endl;

  return 0;
}
