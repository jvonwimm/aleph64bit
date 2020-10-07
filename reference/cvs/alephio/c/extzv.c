#if defined(ALEPH_C)

extzv(pos,size,base,dest)

int pos;
int size;
int base;
int *dest;

{

   *dest = (base >> pos) & ((1 << size) - 1);
   if ((32 - pos) < size) {
     *dest = *dest & ((1 << (32 - pos)) - 1);
   }

   return;

}
#endif
