      SUBROUTINE FIXPCRL
CKEY  EVENT / INTERNAL
C----------------------------------------------------------------------
C!  - restore LCAL clusters in PCRL,PCOB for POT-POT proc. Julver < 262
C!
C!   Author   :- E. Lancon             10-JUN-1992
C!
C?   Modifications.
C!       1. Patrick Janot -- 13 June 1992:
C!          o Update PCOB as well as PCRL (# of calobjects)
C!          o Update the 10th row of the LCAL PECOs (Relation PECO/PCOB)
C!          o Fill the 1st row of PCRL for the new lines (calobject
C!            number)
C!       2. E. Lancon 15-JUN-1992
C!          o order in PCRL
C!
C!  Called from QMEVNT
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "pcrljj.h"
#include "pecojj.h"
#include "rhahjj.h"
      COMMON / JUVV / IJVER
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C   Do nothing if Julia version IJVER >= 262 and if :
C?                  - No PECO
C?                  - No LCAL cluster (region code=192) in PECO
C
      KPECO = IW(NAMIND('PECO'))
      IF (IJVER.LE.0.OR.IJVER.GE.262) GO TO 999
      IF (KPECO.LE.0) GOTO 999
      NPECO = LROWS (KPECO)
      IF (ITABL(KPECO,NPECO,JPECKD).NE.192) GOTO 999
C PAJ ----------------------
C PAJ : Initialize PCOB
      KPCOB = IW(NAMIND('PCOB'))
      NPCOB = 0
      IF ( KPCOB .GT. 0 ) NPCOB = LROWS (KPCOB)
C PAJ ----------------------
C
C?   one have LCAL cluster(s) in PECO check if they are in PCRL
C
      KPCRL = IW(NAMIND('PCRL'))
      IF ( KPCRL.GT.0 ) THEN
        NPCRL = LROWS (KPCRL)
        IF ( NPCRL.GT.0 ) THEN
          IPECO = ITABL(KPCRL,NPCRL,JPCRPE)
          IF ( IPECO.NE.NPECO ) THEN
C
C?   One has to DO something !!!
C?        - Count number of lcal cluster
C?        - enlarge PCRL by the corresponding number and fill PCRL
C
            IF (FIRST) WRITE (IW(6),'(///,4(/,10x,a),///)')
     &'+------------------------------------------------------------+',
     &'|      FIXPCRL called to Fix PCRL/PCOB Banks                 |',
     &'|  !!!! Output banks are not equal to Input banks !!!!       |',
     &'+------------------------------------------------------------+'
            FIRST = .FALSE.
            NLCAL = 0
            DO IPECO =  1, NPECO
              IF ( ITABL(KPECO,IPECO,JPECKD).EQ.192 ) THEN
                NLCAL = NLCAL + 1
C PAJ -----------------
C PAJ : Update PECO
                NPCOB = NPCOB + 1
                IW (KROW(KPECO,IPECO)+JPECPC) = NPCOB
C PAJ -----------------
              ENDIF
            ENDDO
            NROWS = LROWS(KPCRL) + NLCAL
            NTOT = NROWS*LCOLS(KPCRL) + LMHLEN
            CALL AUBOS ('PCRL', 0, NTOT, KPCRL, IGARB)
C PAJ -----------------
C PAJ : Update PCOB
            IW (KPCOB+LMHROW) = NPCOB
C PAJ -----------------
            IF ( IGARB.EQ.2 ) THEN
              WRITE (IW(6),*) ' **** FIXPCRL **** unable to fill PCRL '
              GOTO 999
            ENDIF
            KPECO = IW(NAMIND('PECO'))
            IW (KPCRL+LMHROW) = NROWS
            DO ILCAL  =  1, NLCAL
              IW (KROW(KPCRL,NPCRL+ILCAL)+JPCRPE) = NPECO - NLCAL +
     &          ILCAL
C PAJ -----------------
C PAJ : Update PCRL
              IW (KROW(KPCRL,NPCRL+ILCAL)+JPCRPC) = NPCOB - NLCAL +
     &          ILCAL
C PAJ -----------------
            ENDDO
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
#endif
