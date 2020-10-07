      SUBROUTINE QBPROF(ID,CHTITL,NX,XMI,XMA,YMI,YMA,CHOPT)
C----------------------------------------------------------------------
CKEY HIST /USER
C! ALPHA version of HBPROF
C  Author : I. Tomalin    27-Jul-1993
C----------------------------------------------------------------------
#ifndef DOC
      LOGICAL HEXIST
      CHARACTER * (*) CHTITL
      CHARACTER*1 CHOPT
      IF (.NOT.HEXIST(ID)) CALL HBPROF(ID,CHTITL,NX,XMI,XMA,YMI,YMA,
     +CHOPT)
      END
#endif
