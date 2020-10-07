#if defined(IBM) || defined(APOLLO) || defined(GOULD) || defined(ALEPH_HP) || defined(ALEPH_LINUX)
      OPEN(UNIT=LUN,ACCESS='DIRECT',FORM='UNFORMATTED',
     1     RECL=4*IRECL,ERR=90)
#else
#if defined(CRAY)
      OPEN(UNIT=LUN,ACCESS='DIRECT',FORM='UNFORMATTED',
     1     RECL=8*IRECL,ERR=90)
#else
#if defined(ALEPH_DEC) || defined(ALEPH_SGI)
      OPEN(UNIT=LUN,ACCESS='DIRECT',FORM='UNFORMATTED',
     +    SHARED,
     +   RECL=IRECL,ERR=90)
#endif
#endif
#endif
