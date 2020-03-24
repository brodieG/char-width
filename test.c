#define _XOPEN_SOURCE  
#include <wchar.h>
#include <stdio.h>
#include <locale.h>
#include <gnu/libc-version.h>

int main() {
#ifdef __STDC_ISO_10646__
  printf("hello world\n");
#endif
  printf("%s\n", gnu_get_libc_version());
  printf("%s\n", setlocale(LC_CTYPE, "en_US.UTF-8"));
  FILE * f = fopen("glibc_widths", "w");

  for(wchar_t i = 0; i < 0x3000; ++i) {
    fprintf(f, "%04x %d\n", i, wcwidth(i));
  }
  fclose(f);

  return 0;
}
