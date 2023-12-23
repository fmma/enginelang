#include <stdio.h>

typedef float (*f1)(float x0);
typedef f1 (*f2)(float, f1);
typedef struct {int x0; float x1; } t1;
typedef struct {int x0; float x1; } t2;
typedef union {t1 x0; t2 x1} u1;
typedef struct {int tag; u1 x0} qq;

typedef float *float_ptr;

typedef union {} empty;

float x0(float x1) {
  float x4 = 2.0f;
  float x5 = 4.0f;
  return x5;
}
int main() {
  f1 foo = x0;
  t1 q = { 1, 1};
  u1 r = {1, 1};
  qq qq = {1, 1, 1};
  u1 u1 = qq.x0;
  float x9 = 99.0f;
  float x10;
  float x11;
  float_ptr yy = &x11;
  
  foo(x9);
  printf("(%f, %f)\n", x10, x11);
  printf("%d\n", sizeof(empty));
}
