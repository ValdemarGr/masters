

#include <iostream>
#include <variant>

int main() {
  auto v = ([=](auto IF) {
    return ([=](auto main) {
      return ([=](auto cnd) { return ((main)(main))(cnd); })([=](auto main) {
        return [=](auto cnd) {
          return [=](auto i) {
            return (((IF)([=](auto unit) {
              return (((i)(nullptr)) ==
                      (([=](auto unit) { return 3; })(nullptr)));
            }))([=](auto unit) { return 2; }))([=](auto unit) { return 4; });
          };
        };
      });
    })([=](auto main) {
      return [=](auto cnd) {
        return (((cnd)(main))(cnd))([=](auto unit) { return 2; });
      };
    });
  })([=](auto exp) {
    return [=](auto fst) {
      return [=](auto snd) {
        return [=](auto unit) {
          return (exp)(nullptr) ? (fst)(nullptr) : (snd)(nullptr);
        };
      };
    };
  });

  std::cout << v(nullptr) << std::endl;

  return 0;
}
