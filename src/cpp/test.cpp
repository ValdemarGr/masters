

#include <iostream>

int main() {
auto v =([=](auto IF) {
 return ([=](auto Nil) {
 return ([=](auto Cons) {
 return ([=](auto main) {
 return ([=](auto sum) {
 return ([=](auto lst) {
 return (((main)(sum))(main))(lst);
 })([=](auto sum) {
 return [=](auto main) {
 return [=](auto lst) {
 return (((Nil)(sum))(main))(lst);
 };
 };
 });
 })([=](auto sum) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto a) {
 return [=](auto l) {
 return ((l)([=](auto xs) {
 return [=](auto x) {
 return (((((sum)(sum))(main))(lst))(((a) + (x))))(xs);
 };
 }))(a);
 };
 };
 };
 };
 });
 })([=](auto sum) {
 return [=](auto main) {
 return [=](auto lst) {
 return (((((sum)(sum))(main))(lst))(0))((((((Cons)(sum))(main))(lst))(2))((((((Cons)(sum))(main))(lst))(4))((((((Cons)(sum))(main))(lst))(9))((((Nil)(sum))(main))(lst)))));
 };
 };
 });
 })([=](auto sum) {
 return [=](auto main) {
 return [=](auto lst) {
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
 })([=](auto sum) {
 return [=](auto main) {
 return [=](auto lst) {
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
