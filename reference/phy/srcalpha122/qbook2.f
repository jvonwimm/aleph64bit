      SUBROUTINE QBOOK2 (ID,CHTITL,NX,XMI,XMA,NY,YMI,YMA,VMX)
C----------------------------------------------------------------------
CKEY HIST /USER
C! ALPHA version of HBOOK2
C----------------------------------------------------------------------
      LOGICAL HEXIST
      CHARACTER * (*) CHTITL
      IF (.NOT. HEXIST (ID))
     +    CALL HBOOK2 (ID,CHTITL,NX,XMI,XMA,NY,YMI,YMA,VMX)
      END
