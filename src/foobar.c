#include <stdio.h>
#include <stdint.h>
#define PCRE2_CODE_UNIT_WIDTH 0
#include <pcre2.h>

int64_t hellow() {
  printf("Hello, World!\n");
  return 32;
}

int32_t i32_l(int64_t v) {
  return (int32_t)v;
}

int32_t i32_s(char * v) {
  return atol(v);
}
