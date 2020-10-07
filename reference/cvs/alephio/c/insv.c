#if defined(ALEPH_C)

insv(src,pos,size,base)

int src;
int pos;
int size;
int *base;

{

   if (size == 0) return;

   *base = (~(((1 << size) - 1) << pos) & *base) |
                          ((((1 << size) - 1) & src) << pos);

   return;

}
#endif
