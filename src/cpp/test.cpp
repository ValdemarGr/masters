

#include <iostream>

int main() {
auto v =([=](auto Nil) {
 return ([=](auto Cons) {
 return ([=](auto main) {
 return ([=](auto ele) {
 return ([=](auto die) {
 return ([=](auto snd) {
 return ([=](auto lst) {
 return (((((main)(snd))(main))(lst))(ele))(die);
 })([=](auto snd) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto ele) {
 return [=](auto die) {
 return (((((Nil)(snd))(main))(lst))(ele))(die);
 };
 };
 };
 };
 });
 })([=](auto snd) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto ele) {
 return [=](auto die) {
 return (((((((Cons)(snd))(main))(lst))(ele))(die))(2))((((((Nil)(snd))(main))(lst))(ele))(die));
 };
 };
 };
 };
 });
 })([=](auto snd) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto ele) {
 return [=](auto die) {
 return [=](auto x) {
 return [=](auto xs) {
 return 2;
 };
 };
 };
 };
 };
 };
 });
 })([=](auto snd) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto ele) {
 return [=](auto die) {
 return [=](auto x) {
 return [=](auto xs) {
 return x;
 };
 };
 };
 };
 };
 };
 });
 })([=](auto snd) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto ele) {
 return [=](auto die) {
 return (((((((snd)(snd))(main))(lst))(ele))(die))((((((ele)(snd))(main))(lst))(ele))(die)))(4);
 };
 };
 };
 };
 });
 })([=](auto snd) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto ele) {
 return [=](auto die) {
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
 };
 };
 });
 })([=](auto snd) {
 return [=](auto main) {
 return [=](auto lst) {
 return [=](auto ele) {
 return [=](auto die) {
 return [=](auto Cons) {
 return [=](auto Nil) {
 return Nil;
 };
 };
 };
 };
 };
 };
 });

    std::cout << v << std::endl;

    return 0;
}
