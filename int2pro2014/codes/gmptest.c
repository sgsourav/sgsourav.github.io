#include <stdio.h>
#include <gmp.h>
#include <string.h>

void printHelp () {
    printf("\n");
    printf("Error : Wrong input format!\n");
    printf("\n");
    printf("This program expects either of the following input formats.\n");
    printf("-Cmp <first number> <second number>\n");
    printf("-Add <first number> <second number>\n");
    printf("-Sub <first number> <second number>\n");
    printf("-Mul <multiplicand> <multiplier>\n");
    printf("-Div <dividend> <divisor>\n");
    printf("-Mod <dividend> <divisor>\n");
    printf("-Gcd <first number> <second number>\n");
    printf("-Exp <base> <exponent> <modulus>\n");
    printf("\n");
}

int main (int argc, char *argv[]) {
  int intRes;
  mpz_t firInt, secInt, thiInt, gmpRes;

  if (argc == 1) {
    printHelp();
    return 0;
  }

  if (!strcmp(argv[1], "-Cmp")) {
    if (argc < 4) {
      printf("-Cmp operation expects two inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    intRes = mpz_cmp(firInt, secInt);
    printf("%d\n", intRes);
    mpz_clear(firInt); 
    mpz_clear(secInt);
  }
  else if (!strcmp(argv[1], "-Add")) {
    if (argc < 4) {
      printf("-Add operation expects two inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    mpz_init(gmpRes);
    mpz_add(gmpRes, firInt, secInt);
    mpz_out_str(stdout, 10, gmpRes);
    printf("\n");
    mpz_clear(firInt); 
    mpz_clear(secInt);
    mpz_clear(gmpRes);
  }
  else if (!strcmp(argv[1], "-Sub")) {
    if (argc < 4) {
      printf("-Sub operation expects two inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    mpz_init(gmpRes);
    mpz_sub(gmpRes, firInt, secInt);
    mpz_out_str(stdout, 10, gmpRes);
    printf("\n");
    mpz_clear(firInt); 
    mpz_clear(secInt);
    mpz_clear(gmpRes);
  }
  else if (!strcmp(argv[1], "-Mul")) {
    if (argc < 4) {
      printf("-Mul operation expects two inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    mpz_init(gmpRes);
    mpz_mul(gmpRes, firInt, secInt);
    mpz_out_str(stdout, 10, gmpRes);
    printf("\n");
    mpz_clear(firInt); 
    mpz_clear(secInt);
    mpz_clear(gmpRes);
  }
  else if (!strcmp(argv[1], "-Div")) {
    if (argc < 4) {
      printf("-Div operation expects two inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    mpz_init(gmpRes);
    mpz_cdiv_q(gmpRes, firInt, secInt);
    mpz_out_str(stdout, 10, gmpRes);
    printf("\n");
    mpz_clear(firInt); 
    mpz_clear(secInt);
    mpz_clear(gmpRes);
  }
  else if (!strcmp(argv[1], "-Mod")) {
    if (argc < 4) {
      printf("-Mod operation expects two inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    mpz_init(gmpRes);
    mpz_mod(gmpRes, firInt, secInt);
    mpz_out_str(stdout, 10, gmpRes);
    printf("\n");
    mpz_clear(firInt); 
    mpz_clear(secInt);
    mpz_clear(gmpRes);
  }
  else if (!strcmp(argv[1], "-Gcd")) {
    if (argc < 4) {
      printf("-Gcd operation expects two inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    mpz_init(gmpRes);
    mpz_gcd(gmpRes, firInt, secInt);
    mpz_out_str(stdout, 10, gmpRes);
    printf("\n");
    mpz_clear(firInt); 
    mpz_clear(secInt);
    mpz_clear(gmpRes);
  }
  else if (!strcmp(argv[1], "-Exp")) {
    if (argc < 5) {
      printf("-Exp operation expects three inputs -- %d provided\n", (argc - 2));
      return 0;
    }
    mpz_init_set_str(firInt, argv[2], 10);
    mpz_init_set_str(secInt, argv[3], 10);
    mpz_init_set_str(thiInt, argv[4], 10);
    mpz_init(gmpRes);
    mpz_powm(gmpRes, firInt, secInt, thiInt);
    mpz_out_str(stdout, 10, gmpRes);
    printf("\n");
    mpz_clear(firInt); 
    mpz_clear(secInt);
    mpz_clear(thiInt);
    mpz_clear(gmpRes);
  }
  else {
    printHelp();
  }

  return 0;
}
