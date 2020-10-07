      SUBROUTINE QBOOK1 (ID,CHTITL,NX,XMI,XMA,VMX)
C----------------------------------------------------------------------
CKEY HIST /USER
C! ALPHA version of HBOOK1
C----------------------------------------------------------------------
      LOGICAL HEXIST
      CHARACTER * (*) CHTITL
      IF (.NOT. HEXIST (ID))  CALL HBOOK1 (ID,CHTITL,NX,XMI,XMA,VMX)
      END
