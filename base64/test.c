#include "stdlib.h"
#include "stdio.h"
#include "time.h"

typedef unsigned int uint;
const char* chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static char decode_table[256];

int encode_size(int size) {
  return (int)(size * 4 / 3.0) + 6;
}

int decode_size(int size) {
  return (int)(size * 3 / 4.0) + 6;
}

void init_decode_table() {
  for (int i = 0; i < 256; i++) {
    char ch = (char)i;
    char code = -1;
    if (ch >= 'A' && ch <= 'Z') code = ch - 0x41;
    if (ch >= 'a' && ch <= 'z') code = ch - 0x47;
    if (ch >= '0' && ch <= '9') code = ch + 0x04; 
    if (ch == '+' || ch == '-') code = 0x3E;
    if (ch == '/' || ch == '_') code = 0x3F;
    if (ch == '=') code = -3;
    if (ch == '\r' || ch == '\n') code = -2;
    decode_table[i] = code;
  }
}

void decode(int size, const char* str, int* out_size, char** output) {
  *output = (char*) malloc( decode_size(size) );
  char *out = *output;
  uint buf = 0;
  const char* ends = str + size;
  int mod = 0;
  while (str != ends) {
    char ch = *str++;
    char dec = decode_table[(unsigned char)ch];
    if (dec < 0) {
      if (dec == -2) continue;
      if (dec == -3) break;
      continue; // ignore bad symbol
    }
    buf  = (buf | dec) << 6;
    mod += 1;
    if (mod == 4) {
      mod = 0;
      *out++ = (char)(buf >> 22);
      *out++ = (char)(buf >> 14);
      *out++ = (char)(buf >> 6);
    }    
  }

  if (mod == 2) *out++ = (char)(buf >> 10);
  else if (mod == 3) {
    *out++ = (char)(buf >> 16);
    *out++ = (char)(buf >> 8);
  }

  *out = '\0';
  *out_size = out - *output;
}

void encode(int size, const char* str, int* out_size, char** output) {
  *output = (char*) malloc( encode_size(size) );
  char *out = *output;
  const char* ends = str + (size - size % 3);
  uint n;
  while (str != ends) {
    n = (uint)*str++ << 16;
    n |= (uint)*str++ << 8;
    n |= (uint)*str++;
    *out++ = chars[(n >> 18) & 63];
    *out++ = chars[(n >> 12) & 63];
    *out++ = chars[(n >> 6) & 63];
    *out++ = chars[(n >> 0) & 63];
  }
  int pd = size % 3;
  if  (pd == 1) {
    n = (uint)*str << 16;
    *out++ = chars[(n >> 18) & 63];
    *out++ = chars[(n >> 12) & 63];
    *out++ = '=';
    *out++ = '=';
  } else if (pd == 2) {
    n = (uint)*str++ << 16;
    n |= (uint)*str << 8;
    *out++ = chars[(n >> 18) & 63];
    *out++ = chars[(n >> 12) & 63];
    *out++ = chars[(n >> 6) & 63];
    *out++ = '=';
  }
  *out = '\0';
  *out_size = out - *output;
}

int main() {
  init_decode_table();

  const int STR_SIZE = 10000000;
  const int TRIES = 100;

  char *str = (char*) malloc(STR_SIZE + 1);
  for (int i = 0; i < STR_SIZE; i++) { str[i] = 'a'; }
  str[STR_SIZE] = '\0';

  int s = 0;
  clock_t t = clock();
  for (int i = 0; i < TRIES; i++) { 
    char *str2; 
    int str2_size;
    encode(STR_SIZE, str, &str2_size, &str2); 
    s += str2_size;
    free(str2); 
  }
  printf("encode: %d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);

  char *str2;
  int str2_size;
  encode(STR_SIZE, str, &str2_size, &str2);

  s = 0;
  t = clock();
  for (int i = 0; i < TRIES; i++) {
    char *str3;
    int str3_size;
    decode(str2_size, str2, &str3_size, &str3);
    s += str3_size;
    free(str3);
  }
  printf("decode: %d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);
}
