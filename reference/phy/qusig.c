#include <stdio.h>

void qusig_(kn, kr, ke)
     int* kn;
     int* kr;
     int* ke;
{  fprintf(stderr," KNEVT : %d, KRUN : %d, KEVT : %d, signal received !\n",
	  *kn,*kr,*ke);
  return;
}
