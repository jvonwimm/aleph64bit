      SUBROUTINE PCRLTJ(IERR)
C ----------------------------------------------------------------------
C - J.F. Grivaz 15-03-1989
C! Split PCRL bank into PFER, PFHR, PHER, PHPR relation banks
C - Output
C -   IERR = 0 if successful unpacking
C          =-1 OK but garbage collection
C          = 1 PCRL bank missing or structure error
C          = 2 if unsuccessful ( No room to book banks )
C
#ifndef DOC
      SAVE
#include "pcrljj.h"
#include "bcs.h"
      DATA IFRST/0/
#include "bmacro.h"
C ----------------------------------------------------------------------
C
      IF(IFRST.LE.0) THEN
        CALL BKFMT('PFER','2I,(2I)')
        CALL BKFMT('PFHR','2I,(2I)')
        CALL BKFMT('PHER','2I,(2I)')
        CALL BKFMT('PHPR','2I,(2I)')
        NAPCRL=NAMIND('PCRL')
        IFRST = 1
      ENDIF
C
      CALL BDROP(IW,'PFERPFHRPHERPHPR')
      IERR = 1
      IF(IW(NAPCRL).LE.0) GO TO 100
      NUM = 0
      IER = 0
      IG = 0
C
      ICL1 = MIN0(JPCRPF,JPCRPE)
      ICL2 = MAX0(JPCRPF,JPCRPE)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PFER',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PFER')
C
      ICL1 = MIN0(JPCRPF,JPCRPH)
      ICL2 = MAX0(JPCRPF,JPCRPH)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PFHR',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PFHR')
C
      ICL1 = MIN0(JPCRPH,JPCRPE)
      ICL2 = MAX0(JPCRPH,JPCRPE)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PHER',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PHER')
C
      ICL1 = MIN0(JPCRPH,JPCRPP)
      ICL2 = MAX0(JPCRPH,JPCRPP)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PHPR',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PHPR')
C
      IERR = 0
      IF (IG.NE.0) IERR = -1
      GO TO 100
C
  999 CONTINUE
      IERR = 2
C
  100 CONTINUE
      RETURN
      END
#endif
