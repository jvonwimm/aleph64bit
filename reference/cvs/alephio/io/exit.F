#if defined(RS6K)
      SUBROUTINE EXIT
C --------------------------------------------------
CKEY IO EXIT RS6000
C - R.Johnson - 920327
C! IBMRS6000 has no EXIT subroutine
C -------------------------------------------------
#ifndef DOC
      WRITE (6,1)
 1    FORMAT(' EXIT called. Do-nothing RS6000 version')
      STOP
      END
#endif
#endif
