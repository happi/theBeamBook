#include <stdio.h>
#include <stdlib.h>

unsigned char *read_file(char *name) {
  FILE *file;
  unsigned char *code;
  long  size;
  
  file = fopen(name, "rb");
 
  if(file == NULL) exit(1);
 
  fseek(file, 0L, SEEK_END);
  size = ftell(file);
  code = (unsigned char*)calloc(size + 1, sizeof(char)); /* +1: a STOP after the last byte */
  if(code == NULL) exit(1);
 
  fseek(file, 0L, SEEK_SET);	
 
  fread(code, sizeof(char), size, file);
  fclose(file);
  return code;
}

#define STOP 0
#define ADD  1
#define MUL  2
#define PUSH 3

#define pop()   (stack[--sp])
#define push(X) (stack[sp++] = X)

int run(const unsigned char *code) {
  int stack[1000];
  int sp = 0, size = 0, val = 0;
  const unsigned char *ip = code;
  
  while (*ip != STOP) {
    switch (*ip++) {
    case ADD: { int y = pop(); int x = pop(); push(x + y); break; }
    case MUL: { int y = pop(); int x = pop(); push(x * y); break; }
    case PUSH:
      size = *ip++; 
      val = 0;
      while (size--) { val = val * 256 + *ip++; }
      push(val);
      break;
    }
  }
  return pop();
}
  

int main(int argc, char *argv[])
{
  unsigned char *code;
  int res;

  if (argc > 1) {
    code = read_file(argv[1]);
    res = run(code);
    printf("The value is: %i\n", res);
    return 0;
  } else {
    printf("Give the file name of a byte code program as argument\n");
    return -1;
  }
}

