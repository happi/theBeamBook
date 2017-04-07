#include <stdio.h>
#include <stdlib.h>

#define STOP 0
#define ADD  1
#define MUL  2
#define PUSH 3

#define pop() (stack[--sp])
#define push(X) (stack[sp++] = (X))

typedef void (*instructionp_t)(void);

int stack[1000];
int sp;
instructionp_t *ip;
int running;

void add()  { int x,y; x = pop(); y = pop(); push(x + y); }
void mul()  { int x,y; x = pop(); y = pop(); push(x * y); }
void pushi(){ int x;   x = (int)*ip++;       push(x); }
void stop() { running = 0; }

instructionp_t *read_file(char *name) {
  FILE *file;
  instructionp_t *code;
  instructionp_t *cp;
  long  size;
  char ch;
  unsigned int val;
  
  file = fopen(name, "r");
 
  if(file == NULL) exit(1);
 
  fseek(file, 0L, SEEK_END);
  size = ftell(file);
  code = calloc(size, sizeof(instructionp_t));	
  if(code == NULL) exit(1);
  cp = code;
  
  fseek(file, 0L, SEEK_SET);	
  while ( ( ch = fgetc(file) ) != EOF ) 
    {
      switch (ch) {
      case ADD: *cp++ = &add; break;
      case MUL: *cp++ = &mul; break;
      case PUSH:
	*cp++ = &pushi; 
	ch = fgetc(file); 
	val = 0;
	while (ch--) { val = val * 256 + fgetc(file); } 
	*cp++ = (instructionp_t) val;
	break;
      }
    }
  *cp = &stop;

  fclose(file);
  return code;
}
	     

int run() {
  sp = 0;
  running = 1;

  while (running) (*ip++)();

  return pop();
}
  

int main(int argc, char *argv[])
{
  if (argc > 1) {
    ip = read_file(argv[1]);
    printf("The value is: %i\n", run());
    return 0;
  } else {
    printf("Give a the file name of a byte code program as argument\n");
    return -1;
  }
}

