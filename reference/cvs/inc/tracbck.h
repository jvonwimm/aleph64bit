*CD tracbck   CALL THE MACHINE DEPENDANT TRACEBACK
#if defined(ALEPH_DEC) && ! defined(UNIX)
C - VAX trace back
         N=44
         CALL LIB$STOP (%VAL(N))
         CALL EXIT
#else
#if defined(IBM)
C - IBM trace back
         CALL ERRTRA
         CALL EXIT
#else
#if defined(UNIX) && defined(ALEPH_DEC)
C - DECS trace back
      CALL EXIT(1)
#else
      CALL EXIT
#endif
#endif
#endif
