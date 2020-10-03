#include <iostream>

int main() {
auto v =([=](auto main) {
 return ([=](auto g) {
 return ([=](auto f) {
 return (((main)(main))(g))(f);
 })([=](auto main) {
 return [=](auto g) {
 return [=](auto f) {
 return [=](auto a) {
 return ((((g)(main))(g))(f))(a);
 };
 };
 };
 });
 })([=](auto main) {
 return [=](auto g) {
 return [=](auto f) {
 return [=](auto a) {
 return ((((f)(main))(g))(f))(a);
 };
 };
 };
 });
 })([=](auto main) {
 return [=](auto g) {
 return [=](auto f) {
 return ((((f)(main))(g))(f))(5);
 };
 };
 });

    std::cout << v << std::endl;

    return 0;
}
