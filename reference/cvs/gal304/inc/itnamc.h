*CD itnamc
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
#if defined(DOC)
        This common stores bank pointers for use in ITC routines.
        The NAITnn are results from NAMIND for named bank addresses.
        The JDITnn are work bank pointers.  These are used to allow
        different routines to modify the same work banks without having
        problems with relocation of indices.

      LITWBK       = number of work banks used in the ITC module
#endif
