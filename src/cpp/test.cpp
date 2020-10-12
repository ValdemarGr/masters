#include <iostream>

int main() {
auto v =([=](auto IF) {
 return ([=](auto Nil) {
 return ([=](auto Cons) {
 return ([=](auto main) {
 return ([=](auto foldl) {
 return ([=](auto add) {
 return (((main)(main))(foldl))(add);
 })([=](auto main) {
 return [=](auto foldl) {
 return [=](auto add) {
 return [=](auto a) {
 return [=](auto b) {
 return ((a) + (b));
 };
 };
 };
 };
 });
 })([=](auto main) {
 return [=](auto foldl) {
 return [=](auto add) {
 return [=](auto f) {
 return [=](auto a) {
 return [=](auto l) {
 return ((l)([=](auto xs) {
 return [=](auto x) {
 return ((((((foldl)(main))(foldl))(add))(f))(((f)(a))(x)))(xs);
 };
 }))(a);
 };
 };
 };
 };
 };
 });
 })([=](auto main) {
 return [=](auto foldl) {
 return [=](auto add) {
 return ((((((foldl)(main))(foldl))(add))((((add)(main))(foldl))(add)))(0))((((((Cons)(main))(foldl))(add))(2))((((((Cons)(main))(foldl))(add))(4))((((((Cons)(main))(foldl))(add))(9))((((Nil)(main))(foldl))(add)))));
 };
 };
 });
 })([=](auto main) {
 return [=](auto foldl) {
 return [=](auto add) {
 return [=](auto a_0) {
 return [=](auto List_1) {
 return [=](auto Cons) {
 return [=](auto Nil) {
 return ((Cons)(List_1))(a_0);
 };
 };
 };
 };
 };
 };
 });
 })([=](auto main) {
 return [=](auto foldl) {
 return [=](auto add) {
 return [=](auto Cons) {
 return [=](auto Nil) {
 return Nil;
 };
 };
 };
 };
 });
 })([=](auto exp) {
 return [=](auto fst) {
 return [=](auto snd) {
 return (exp) ? (fst) : (snd);
 };
 };
 });

    std::cout << v << std::endl;

    return 0;
}
