      SUBROUTINE SICRPOT
C----------------------------------------------------------------------
C!  - SICAL Create POT banks
C!
C!   Author   :- B. Bloch          18-SEP-1992
C!   modified B. Bloch  June 98 : exclude clusters identified as 
C!                                electronic noise 
C!
C?   Add SICAL cluster info to PECO/PCRL/PCOB  banks
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "rflags.h"
#include "pcrljj.h"
#include "pecojj.h"
#include "pcobjj.h"
#include "sclsjj.h"
#include "sinamc.h"
      LOGICAL FIRST
      PARAMETER ( ISIRC = 256 )
      DATA FIRST / .TRUE. /
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
         NAPECO = NAMIND('PECO')
         NAPCOB = NAMIND('PCOB')
         NAPCRL = NAMIND('PCRL')
         FIRST = .FALSE.
      ENDIF
C    Do nothing if  No SiCAL cluster
C
      KSCLS = IW(NASCLS)
      IF (KSCLS.LE.0) GOTO 999
      NSCLS = LROWS (KSCLS)
      IF (NSCLS.LE.0) GOTO 999
C    Check if any PECO there
      KPECO = IW(NAPECO)
      IF (KPECO.GT.0) THEN
C check if PECO contains only SICAL clusters : THEN drop PECO,PCRL,PCOB
C this is the case when reprocessing from POT with only SICAL
C Or when processing GALEPH without writing PECO on POT.....
        IF ( ITABL(KPECO,1,JPECKD).EQ.ISIRC) THEN
          CALL BDROP(IW,'PECO')
          CALL BDROP(IW,'PCRL')
          CALL BDROP(IW,'PCOB')
        ENDIF
      ENDIF
      KPECO = IW(NAPECO)
      KPCRL = IW(NAPCRL)
      KPCOB = IW(NAPCOB)
      IF (KPECO.LE.0) THEN
         NTOT = LPECOA *NSCLS + LMHLEN
         NTOT2= LPCRLA *NSCLS + LMHLEN
         CALL AUBOS ('PCOB', 0, LMHLEN,KPCOB, IGARB)
         IF(IGARB .EQ. 2)  GOTO 998
         NPCOB = 0
      ELSE
         NPECO = LROWS (KPECO)
         NPCRL = LROWS (KPCRL)
         NPCOB = LROWS (KPCOB)
         NTOT = LPECOA *( NPECO+NSCLS)+ LMHLEN
         NTOT2= LPCRLA *( NPCRL+NSCLS)+ LMHLEN
      ENDIF
      CALL AUBOS ('PECO', 0, NTOT, KPECO, IGARB)
      IF(IGARB .EQ. 2)  GOTO 998
      IW(KPECO+LMHCOL)= LPECOA
      CALL AUBOS ('PCRL', 0, NTOT2,KPCRL, IGARB)
      IF(IGARB .EQ. 2)  GOTO 998
      IW(KPCRL+LMHCOL)= LPCRLA
C   Redefine pointers
      KPECO = IW(NAPECO)
      KPCRL = IW(NAPCRL)
      KPCOB = IW(NAPCOB)
      KSCLS = IW(NASCLS)
C   Loop on SCLS to book extra PECO and PCRL and update PCOB
      nkeep = 0
      DO 10 ICL = 1,NSCLS
C   First , PECO.....
         IND= KNEXT(KPECO)
C   do not store clusters identified as noise
         CALL SICLEAN(ICL,IFLAG)
         IF ( IFLAG.NE.0) GO TO 10
         nkeep = nkeep + 1
         NPCOB = NPCOB+1
         RW(IND+JPECER)= RTABL(KSCLS,ICL,JSCLEC)
         RW(IND+JPECE1)= 1.
         RW(IND+JPECE2)= 0.
         RW(IND+JPECTH)= RTABL(KSCLS,ICL,JSCLTC)
         RW(IND+JPECPH)= RTABL(KSCLS,ICL,JSCLPC)
         RW(IND+JPECEC)= RTABL(KSCLS,ICL,JSCLEC)
         IW(IND+JPECKD)= ISIRC
         IW(IND+JPECCC)= 0
         IW(IND+JPECRB)= 0
         IW(IND+JPECPC)= NPCOB
         IW(KPECO+LMHROW)= IW(KPECO+LMHROW)+1
C   Now PCRL.....
         JND= KNEXT(KPCRL)
         IW(JND+JPCRPC)= NPCOB
         IW(JND+JPCRPE)= IW(KPECO+LMHROW)
         IW(JND+JPCRPF)= 0
         IW(JND+JPCRPH)= 0
         IW(JND+JPCRPP)= 0
         IW(KPCRL+LMHROW)= IW(KPCRL+LMHROW)+1
  10  CONTINUE
C   shrink PECO,PCRL to their real size if needed
      IF ( nkeep.ne.nscls ) then
         len = LMHLEN + IW(KPECO+LMHROW)*LPECOA
         CALL AUBOS ('PECO', 0, len, KPECO, IGARB)
         len = LMHLEN + IW(KPCRL+LMHROW)*LPCRLA
         CALL AUBOS ('PCRL', 0, len, KPCRL, IGARB)
       ENDIF
C   Last PCOB....
      IW(KPCOB+LMHROW)= NPCOB
      IF(FDEBRF.AND.JDBDRF(JULSI).GE.2) THEN
         CALL PRTABL('PECO',0)
         CALL PRTABL('PCRL',0)
         CALL PRTABL('PCOB',0)
      ENDIF
      GO TO 999
  998 CALL RERROR ('SICRPOT', -1, 'Unable to extend PCOB/PECO/PCRL ')
  999 RETURN
      END
#endif
