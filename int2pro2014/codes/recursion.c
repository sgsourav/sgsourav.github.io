#include <stdio.h>

void samplePrint (int *sample, int r) {
  int i;
  for (i = 0; i < r; i++)
    printf("%d ", sample[i]);
  printf("\n");
}

void genSamp (int n, int r, int *sample, int k) {
  sample[k] = 0;
  while (sample[k] < n) {
    sample[k]++;
    if (k < r-1)
      genSamp(n, r, sample, k+1);
    else
      samplePrint(sample, r);
  }
}

void genComb (int n, int r, int *sample, int k) {
  sample[k] = sample[k-1];
  while (sample[k] < n - r + k + 1) {
    sample[k]++;
    if (k < r-1)
      genComb(n, r, sample, k+1);
    else
      samplePrint(sample, r);
  }
}

void genPerm (int n, int r, int *sample, int k) {
  int i, temp;
  for (i = k; i < n; i++) {
    temp = sample[k];
    sample[k] = sample[i];
    sample[i] = temp;

    if (k < r-1)
      genPerm(n, r, sample, k+1);
    else
      samplePrint(sample, r);

    temp = sample[k];
    sample[k] = sample[i];
    sample[i] = temp;
  }
}

int main () {
  int n, r, i;
  int sample[10];

  printf("\nThis code draws samples of size 'r' from a total of 'n' elements, as samples with unlimited repititions, as combinations, and as permutations.\n\n");
  printf("Enter the value of n : ");
  scanf("%d", &n);
  printf("Enter the value of r : ");
  scanf("%d", &r);

  printf("\nSamples : %d from %d\n", r, n);
  genSamp(n, r, sample, 0);

  printf("\nCombinations : %d from %d\n", r, n);
  genComb(n, r, sample, 0);

  printf("\nPermutations : %d from %d\n", r, n);
  for (i = 0; i < n; i++)
    sample[i] = i+1;
  genPerm(n, r, sample, 0);
  return 0;
}
