#include <stdio.h>
#include <stdlib.h>

char *read_file(char *name) {
  FILE *file;
  char *code;
  long  size;
  
  file = fopen(name, "r");
 
  if(file == NULL) exit(1);
 
  fseek(file, 0L, SEEK_END);
  size = ftell(file);
  code = (char*)calloc(size, sizeof(char));	
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

int run(char *code) {
  int stack[1000];
  int sp = 0, size = 0, val = 0;
  char *ip = code;
  
  while (*ip != STOP) {
    switch (*ip++) {
    case ADD: push(pop() + pop()); break;
    case MUL: push(pop() * pop()); break;
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
  char *code;
  int res;

  if (argc > 1) {
    code = read_file(argv[1]);
    res = run(code);
    printf("The value is: %i\n", res);
    return 0;
  } else {
    printf("Give a the file name of a byte code program as argument\n");
    return -1;
  }
}

