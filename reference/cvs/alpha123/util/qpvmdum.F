#if defined(UNIX)
      SUBROUTINE QPVMCD(CQFWRK)
C! Dummy routines for PVM on SAGA machines
#ifndef DOC
      CHARACTER*(*) CQFWRK
      RETURN
      END
C********************************************************************
      SUBROUTINE QPVMEN
      RETURN
      END
C********************************************************************
      SUBROUTINE QPVMES(NRUN,NEVT,IRET,IEND)
      RETURN
      End
C******************************************************************
      SUBROUTINE QPVMEX(KUPRNT)
      RETURN
      End
C********************************************************************
      SUBROUTINE QPVMGT(*)
      RETURN
      End
C********************************************************************
      SUBROUTINE QPVMIM(KUPRNT)
      RETURN
      END
C********************************************************************
      SUBROUTINE QPVMIS(KUPRNT)
      RETURN
      END
C********************************************************************
      SUBROUTINE QPVMOF (CFILE)
      CHARACTER*(*) CFILE
      RETURN
      END
c********************************************************************
      SUBROUTINE QPVMTE(KUPRNT,XHISTO,CQFHIS)
      INTEGER KUPRNT
      LOGICAL XHISTO
      CHARACTER*(*) CQFHIS
      RETURN
      END
c********************************************************************
      SUBROUTINE QPVMTI(KUPRNT,KNEVT,TITO,TIIN,TIPR,TIAV,TITE)
      RETURN
      END
c********************************************************************
      SUBROUTINE QPVMWR(*)
      RETURN
      END
c********************************************************************
#endif
#endif
