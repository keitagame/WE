#include <stdio.h>
#include <stdlib.h>

// ポインタ配列の読み書き (Vec内のポインタ操作)
void poke_ptr(void *addr, void *val) { *(void**)addr = val; }
void *peek_ptr(void *addr) { return *(void**)addr; }

// ポインタ<->int変換
long ptr_to_int(void *p) { return (long)p; }
void *int_to_ptr(long v) { return (void*)v; }

// argv helper
char *argv_get(char **argv, int i) { return argv[i]; }

// stderr/stdout
FILE *stderr_ptr(void) { return stderr; }
FILE *stdout_ptr(void) { return stdout; }