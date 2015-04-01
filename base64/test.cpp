#include <openssl/bio.h>
#include <openssl/evp.h>
#include <stdlib.h>
#include <string>
#include <stdio.h>
#include <iostream>
#include <time.h>

std::string base64_encode( const std::string &str ) {
  BIO *base64_filter = BIO_new( BIO_f_base64() );
  BIO_set_flags( base64_filter, BIO_FLAGS_BASE64_NO_NL );
  BIO *bio = BIO_new( BIO_s_mem() );
  BIO_set_flags( bio, BIO_FLAGS_BASE64_NO_NL );
  bio = BIO_push( base64_filter, bio );
  BIO_write( bio, str.c_str(), str.length() );
  BIO_flush( bio );
  char *new_data;
  long bytes_written = BIO_get_mem_data( bio, &new_data );
  std::string result( new_data, bytes_written );
  BIO_free_all( bio );
  return result;
}

std::string base64_decode( const std::string &str ) {
  BIO *bio, *base64_filter, *bio_out;
  char inbuf[512];
  int inlen;
  base64_filter = BIO_new( BIO_f_base64() );
  BIO_set_flags( base64_filter, BIO_FLAGS_BASE64_NO_NL );
  bio = BIO_new_mem_buf( (void*)str.c_str(), str.length() );
  bio = BIO_push( base64_filter, bio );
  bio_out = BIO_new( BIO_s_mem() );
  while( (inlen = BIO_read(bio, inbuf, 512)) > 0 ){ BIO_write( bio_out, inbuf, inlen ); }
  BIO_flush( bio_out );
  char *new_data;
  long bytes_written = BIO_get_mem_data( bio_out, &new_data );
  std::string result( new_data, bytes_written );
  BIO_free_all( bio );
  BIO_free_all( bio_out );
  return result;
}

int main() {
  const int STR_SIZE = 10000000;
  const int TRIES = 100;

  const std::string str(STR_SIZE, 'a');
  std::string str2 = "";

  int s = 0;
  clock_t t = clock();
  for (int i = 0; i < TRIES; i++) { str2 = base64_encode(str); s += str2.length(); }
  printf("encode: %d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);

  s = 0;
  t = clock();
  for (int i = 0; i < TRIES; i++) { s += base64_decode(str2).length(); }
  printf("decode: %d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);
}
