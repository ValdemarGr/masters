

#include <iostream>
#include <variant>

int main() {
  auto v = ([=](auto IF) {
    return ([=](auto Nil) {
      return ([=](auto Cons) {
        return ([=](auto foldl) {
          return ([=](auto main) {
            return ([=](auto range) {
              return ([=](auto add) {
                return ([=](auto foldl) {
                  return ((((main)(range))(main))(foldl))(add);
                })([=](auto range) {
                  return [=](auto main) {
                    return [=](auto foldl) {
                      return [=](auto add) {
                        return [=](auto f) {
                          return [=](auto a) {
                            return [=](auto l) {
                              return ((l)([=](auto xs) {
                                return [=](auto x) {
                                  return (((((((foldl)(range))(main))(foldl))(
                                      add))(f))(((f)(a))(x)))(xs);
                                };
                              }))(a);
                            };
                          };
                        };
                      };
                    };
                  };
                });
              })([=](auto range) {
                return [=](auto main) {
                  return [=](auto foldl) {
                    return [=](auto add) {
                      return [=](auto a) {
                        return [=](auto b) {
                          return [=](auto unit) {
                            return (((a)(nullptr)) + ((b)(nullptr)));
                          };
                        };
                      };
                    };
                  };
                };
              });
            })([=](auto range) {
              return [=](auto main) {
                return [=](auto foldl) {
                  return [=](auto add) {
                    return [=](auto n) {
                      return (((IF)([=](auto unit) {
                        return (((n)(nullptr)) ==
                                (([=](auto unit) { return 0; })(nullptr)));
                      }))(((((Nil)(range))(main))(foldl))(add)))((
                          (((((Cons)(range))(main))(foldl))(add))(n))((((
                          ((range)(range))(main))(foldl))(add))([=](auto unit) {
                        return (((n)(nullptr)) -
                                (([=](auto unit) { return 1; })(nullptr)));
                      })));
                    };
                  };
                };
              };
            });
          })([=](auto range) {
            return [=](auto main) {
              return [=](auto foldl) {
                return [=](auto add) {
                  return ([=](auto b) {
                    return (((((((foldl)(range))(main))(foldl))(add))(
                        ((((add)(range))(main))(foldl))(add)))(
                        [=](auto unit) { return 0; }))(b);
                  })((((((range)(range))(main))(foldl))(add))(
                      [=](auto unit) { return 10; }));
                };
              };
            };
          });
        })([=](auto range) {
          return [=](auto main) {
            return [=](auto foldl) {
              return [=](auto add) {
                return [=](auto f) {
                  return [=](auto a) {
                    return [=](auto l) {
                      return ((l)([=](auto xs) {
                        return [=](auto x) {
                          return (((((((foldl)(range))(main))(foldl))(add))(f))(
                              (f)((a)(x))))(xs);
                        };
                      }))(a);
                    };
                  };
                };
              };
            };
          };
        });
      })([=](auto range) {
        return [=](auto main) {
          return [=](auto foldl) {
            return [=](auto add) {
              return [=](auto a_0) {
                return [=](auto List_1) {
                  return [=](auto Cons) {
                    return [=](auto Nil) { return ((Cons)(List_1))(a_0); };
                  };
                };
              };
            };
          };
        };
      });
    })([=](auto range) {
      return [=](auto main) {
        return [=](auto foldl) {
          return [=](auto add) {
            return [=](auto Cons) { return [=](auto Nil) { return Nil; }; };
          };
        };
      };
    });
  })([=](auto exp) {
    return [=](auto fst) {
      return [=](auto snd) { return (exp) ? (fst) : (snd); };
    };
  });

  std::cout << v(nullptr) << std::endl;

  return 0;
}
