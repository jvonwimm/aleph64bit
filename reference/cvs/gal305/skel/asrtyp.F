*DK asrtyp
      SUBROUTINE ASRTYP
C --------------------------------------------------------------------
C - F.Ranjard - 861028
C!  Build run type IPROKI
C - Called by  ASIRUN                                 from this .HLB
C ------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "jqcom.h"
#include "kgjjpar.h"
#include "kincom.h"
      INTEGER ALKRUN
C -------------------------------------------------------------------
C
C - set offset for run type : IPROKI
      IPROKI = LOFFMC*100
      IF (TKINJO .EQ. 'PART') IPROKI = LOFFMC*3
      IF (TKINJO .EQ. 'USER') IPROKI = LOFFMC*10
C
C - if KINE process is not defined , call EXIT
      IF (IPROKI .EQ. LOFFMC*100) THEN
         CALL ALTELL ('ASRTYP: no KINE process defined ',0,'STOP')
      ENDIF
C
C - Build the kine run header bank KRUN if it does not exist
      IF (IW(NAKRUN) .EQ. 0) THEN
         JKRUN = ALKRUN (IPROKI,NOTRKI,TRUNJO)
         IF (JKRUN.EQ.0) THEN
            CALL ALTELL ('ASRTYP: not enough space to book KRUN ',1,
     &                   'FATAL')
         ENDIF
      ENDIF
C
      MRTYP = IPROKI / LOFFMC
C
      IF (MRTYP.EQ.3) THEN
C        single particle generator
         IPROKI = IPROKI + INT (BKINJO(4))
      ELSE IF (MRTYP.EQ.10) THEN
C        USER generator
         CALL ASKUSI (IGCOD)
         IPROKI = IPROKI + IGCOD
         IW(JKRUN+LMHLEN+JKRUGI) = IPROKI
      ENDIF
C
      RETURN
      END
#endif
