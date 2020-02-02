#include <stdio.h>
#include <string.h>

void swap (void *x, void *y, int size) {
  char temp[size];
  memcpy(temp, x, size);
  memcpy(x, y, size);
  memcpy(y, temp, size);
}

int main() {
  int u = 10, v = 20;
  int a[2] = {5,6}, b[2] = {7,8};

  swap(u, v, sizeof(u));
  swap(a, b, sizeof(a));

  return 0;
}
