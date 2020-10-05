#include <iostream>

int main() {
auto v =([=](auto IF) {
 return ([=](auto Nil) {
 return ([=](auto Cons) {
 return ([=](auto main) {
 return ([=](auto add) {
 return ((main)(main))(add);
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
 return (((IF)(((2) == (3))))(4))(2);
 };
 });
 })([=](auto main) {
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
 });
 })([=](auto main) {
 return [=](auto add) {
 return [=](auto Cons) {
 return [=](auto Nil) {
 return Nil;
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
