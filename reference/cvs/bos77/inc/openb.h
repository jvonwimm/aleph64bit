#if defined(IBM) || defined(APOLLO) || defined(GOULD) || defined(ALEPH_HP) || defined(ALEPH_LINUX)
         OPEN(UNIT=LUN,FILE=DSN,STATUS='OLD',ACCESS='DIRECT',
     1     RECL=4*IRECL,ERR=90,FORM='UNFORMATTED')
#else
#if defined(CRAY)
         OPEN(UNIT=LUN,FILE=DSN,STATUS='OLD',ACCESS='DIRECT',
     1     RECL=8*IRECL,ERR=90,FORM='UNFORMATTED')
#else
#if defined(ALEPH_DEC) || defined(ALEPH_SGI)
         OPEN(UNIT=LUN,FILE=DSN,STATUS='OLD',ACCESS='DIRECT',
     +   RECL=IRECL,ERR=90,FORM='UNFORMATTED')
#endif
#endif
#endif
