#include <stdio.h>
#include <stdlib.h>

extern void start_timer();
extern void stop_timer();

extern int _toplevel();

void *mymalloc(int n) {
  return malloc(n * sizeof(int));
}

int main() {
  start_timer();
  int r = _toplevel();
  stop_timer();
  printf("%d\n", r);
  return 0;
}
