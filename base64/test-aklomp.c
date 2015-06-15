#include "stdlib.h"
#include "stdio.h"
#include "time.h"
#include "libbase64.h"
#include "../lib/config.h"

#define FLAGS (HAVE_SSSE3) ? BASE64_FORCE_SSSE3 : 0

int main() {
  const int STR_SIZE = 10000000;
  const int TRIES = 100;

  char *str = (char*) malloc(STR_SIZE + 1);
  for (int i = 0; i < STR_SIZE; i++) { str[i] = 'a'; }
  str[STR_SIZE] = '\0';

  int s = 0;
  clock_t t = clock();
  for (int i = 0; i < TRIES; i++) { 
    char *str2 = (char*) malloc( (int)(STR_SIZE/3.0*4) + 6 ); 
    size_t str2_size;
    base64_encode(str, STR_SIZE, str2, &str2_size, FLAGS);
    s += str2_size;
    free(str2); 
  }
  printf("encode: %d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);

  char *str2 = (char*) malloc( (int)(STR_SIZE/3.0*4) + 60 );
  size_t str2_size;
  base64_encode(str, STR_SIZE, str2, &str2_size, 0); 

  s = 0;
  t = clock();
  for (int i = 0; i < TRIES; i++) {
    char *str3 = (char*) malloc( (int)(STR_SIZE) + 60 );
    size_t str3_size;
    
    int res = base64_decode(str2, str2_size, str3, &str3_size, FLAGS);
    str3[str3_size] = '\0';
    s += str3_size;
    free(str3);
  }
  printf("decode: %d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);

  free(str);
  free(str2);
}
