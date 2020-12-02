#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
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

char * chars = 0, * chars_cur = 0;
size_t chars_len = 1024;
void put_char(char c) {
  if (!chars) chars = chars_cur = (char*)malloc(chars_len);
  if (chars_cur - chars >= chars_len) {
    chars = realloc(chars, chars_len * 2);
    chars_cur = chars + chars_len;
    chars_len *= 2;
  }
  *chars_cur++ = c;
}
void put_string(const char * c) {
  if (*c) {put_char(*c++); put_string(c);}
}
// we use this instead of printf in a test, then we can "expect" the result
void test_printf(const char *fmt, ...) {
  int ret;
  va_list ap;
  va_start(ap, fmt);
  for(size_t i = 0, len = strlen(fmt); i < len; i++) {
    if(fmt[i] == '%') {
      if (i + 1 == len) { printf("format error: '%s'", fmt); exit(1); }
      else switch(fmt[++i]) {
        case 's': { const char * c = va_arg(ap, const char*); put_string(c); break; }
        case 'f': {
          char buf[20];
          sprintf(buf, "%g", va_arg(ap, double));
          put_string(buf);
          break;
        }
        case 'd': {
          char buf[20];
          sprintf(buf, "%ld", va_arg(ap, int64_t));
          put_string(buf);
          break;
        }
        default: printf("format error: '%s'", fmt); exit(1);
        }
    } else {
      put_char(fmt[i]);
    }
  }
  va_end(ap);
  printf("chars: %.*s", (int)chars_len, chars);
}
