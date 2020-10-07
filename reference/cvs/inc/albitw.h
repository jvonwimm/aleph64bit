*CD albitw
C! set number of bits in a machine word
      INTEGER NBITW, NBYTW, LCHAR
#if defined(BIT64)
      PARAMETER (NBITW = 64)
#else
      PARAMETER (NBITW = 32)
#endif
      PARAMETER (NBYTW = NBITW/8, LCHAR = NBYTW)
#if defined(DOC)
      NBITW   = number of bits in a machine word
      NBYTW   = number of bytes in a machine word
      LCHAR   = number of characters in a machine word
#endif
