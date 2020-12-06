#include <iostream>
#include <regex>
#include <string>

enum LamTag { VAR, APP, ABS };

typedef struct Term {
  LamTag tag;
  union {
    struct Var {
      std::string name;
    };

    struct App {
      struct Term *lhs;
      struct Term *rhs;
    };

    struct Abs {
      std::string param;
      struct Term *body;
    };
  } value;
} Term;

void expect(char e, std::string rest) {

}

Term parse(std::string str) {
    auto u = str[0];
    switch (u) {
        case '(': 
    }
}

int main() {

  std::cout << 4 << std::endl;

  return 0;
}
