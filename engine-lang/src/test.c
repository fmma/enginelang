#include <stdio.h>

typedef struct {} x0;
typedef union {x0 x0; x0 x1; } x1;
typedef struct {int x0; x1 x1; } x2;
typedef struct {float x0; float x1; } x4;
typedef struct {x4 x0; float x1; } x5;

int main() {
  x0 x3 = {};
  float x6 = 3.0f;
  float x7 = 4.0f;
  x4 x8 = {x6, x7};
  float x9 = 5.0f;
  x5 x10 = {x8, x9};
  x4 x11 = x10.x0;
  float x12 = x11.x0;
  x4 x13 = x10.x0;
  float x14 = x13.x0;
  x4 x15 = {x12, x14};
  float x16 = x15.x0;
  float x17 = x15.x1;
  float x18 = (x16 * x17);
  x4 x19 = x10.x0;
  float x20 = x19.x1;
  x4 x21 = x10.x0;
  float x22 = x21.x1;
  x4 x23 = {x20, x22};
  float x24 = x23.x0;
  float x25 = x23.x1;
  float x26 = (x24 * x25);
  x4 x27 = {x18, x26};
  float x28 = x27.x0;
  float x29 = x27.x1;
  float x30 = (x28 + x29);
  float x31 = x10.x1;
  float x32 = x10.x1;
  x4 x33 = {x31, x32};
  float x34 = x33.x0;
  float x35 = x33.x1;
  float x36 = (x34 * x35);
  x4 x37 = {x30, x36};
  float x38 = x37.x0;
  float x39 = x37.x1;
  int x40 = (x38 == x39);
  x2 x41 = {x40};
  printf("#%d ", x41.x0);
  switch(x41.x0) {
  case 0:
    printf("()");
    break;
  case 1:
    printf("()");
    break;
  }
}