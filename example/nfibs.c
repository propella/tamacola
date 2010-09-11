#include <stdio.h>

int nfibs(int n)
{
  return (n < 2) ? 1 : nfibs(n - 1) + nfibs(n - 2) + 1;
}

int main()
{
  int i;
  for (i= 0;  i < 10;  ++i)
    printf("%i\n", nfibs(32));
  return 0;
}
