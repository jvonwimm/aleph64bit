      SUBROUTINE PHCHIC (IER)
C--------------------------------------------------------------------
C! Builds the relation between CAL objects and MC tracks for the HCAL
C D. Cinabro
C
C Input:  PHCO,HTHT,HTDT,PHST,FPOI
C
C Output: PHMH
C         IER   = 0  OK
C                 -1 input bank missing
C                 -2 inconsistancy in HCAL address
C                 -3 not enough space
C--------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "phmhjj.h"
#include "phstjj.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
C Some initialization
C
      IER = 0
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPHST=NAMIND('PHST')
        NPHCO=NAMIND('PHCO')
        NPHOB=NAMIND('PHOB')
        NHTHT=NAMIND('HTHT')
        NFPOI=NAMIND('FPOI')
        NHTDI=NAMIND('HTDI')
        CALL BKFMT('PHMH','I')
      ENDIF
      KHTD1 = NLINK('HTDT',1)
      KPHST = IW(NPHST)
      KPHCO = IW(NPHCO)
      KPHOB = IW(NPHOB)
      KHTHT = IW(NHTHT)
      KFPOI = IW(NFPOI)
C
C See that these things are here
C
      IF (KPHST.LE.0.OR.KHTHT.LE.0.OR.KFPOI.LE.0.OR.
     &    KHTD1.LE.0) GOTO 901
      IF (KPHOB.LE.0.AND.KPHCO.LE.0) GOTO 901
C
C Book the PHMH bank
C
      IF (KPHCO.GT.0) THEN
         NHOB = LROWS(KPHCO)
      ELSEIF (KPHOB.LE.0) THEN
         NHOB = LROWS(KPHOB)
      ENDIF
      NCOLS = LPHMHA
      LPEMH = NHOB*5*NCOLS + LMHLEN
      CALL AUBOS('PHMH',0,LPEMH,KPHMH,IGARB)
      IF (IGARB.EQ.2) GO TO 902
      IW(KPHMH+LMHCOL) = NCOLS
C
C Update pointers
C
      KHTD1 = NLINK('HTDT',1)
      KPHST = IW(NPHST)
      KPHCO = IW(NPHCO)
      KPHOB = IW(NPHOB)
      KHTHT = IW(NHTHT)
      KFPOI = IW(NFPOI)
      KHTD2 = NLINK('HTDT',2)
      KHTD3 = NLINK('HTDT',3)
C
C One stack or two?
C
      KHTDI = IW(NHTDI)
      NSTA = LCOLS(KHTDI)
      IF (NSTA.EQ.2) THEN
         NSTA = 1
      ELSEIF (NSTA.EQ.3) THEN
         NSTA = 2
      ENDIF
C
      IFIL = 0
      NHST = LROWS(KHTHT)
C
C Loop on PHOB or PHCO
C
      DO 10 I = 1,NHOB
       NTPC = 0
C
C Now loop on PHST to get related stories
C
       DO 20 ISTO = 1,LROWS(KPHST)
C
C Is this story in this PHOB?
C
        IPHO = ITABL(KPHST,ISTO,JPHSPH)
        IF (IPHO.NE.I) GOTO 20
C
C Address in PHST
C
        ITHE = ITABL(KPHST,ISTO,JPHSTI)
        IF (ITHE.GT.62) THEN
         ITHE = ITHE - 128
         ISTA = 2
        ELSE
         ISTA = 1
        ENDIF
        IPHI = ITABL(KPHST,ISTO,JPHSPI)
C
C Pack the address
C
   29   IADD = 10000*IPHI+100*ITHE+ISTA
C
C Loop on HTHT
C
        DO 30 J = 1,NHST
         I2ADD = ITABL(KHTHT,J,1)
         IF (IADD.EQ.I2ADD) THEN
          IPOI11 = J
          GOTO 31
         ENDIF
   30   CONTINUE
C
C If there is only one stack then must search stack 2 of HTHT also
C
        IF (NSTA.EQ.1.AND.ISTA.EQ.1) THEN
         ISTA = 2
         GOTO 29
        ENDIF
C
C Inconsistancy in HCAL address
        IER = -2
        GOTO 20
C
C Get the tracks in HTDT3
C
   31   NHTR = IW(KROW(KHTD1,IPOI11)+1)
        IPOI7 = IW(KROW(KHTD2,IPOI11)+1)
        DO 40 J = 1,NHTR
         NKIN = ITABL(KFPOI,IW(KROW(KHTD3,IPOI7+J)+1),1)
C
C Here loop over the tracks already assocciated with this
C Cal. Obj. and do not consider them again.
C
         IF (NTPC.GT.0) THEN
          DO 50 K = 1,NTPC
   50      IF (NKIN.EQ.ITABL(KPHMH,IFIL+K,JPHMTN)) GOTO 40
         ENDIF
         NTPC = NTPC + 1
         IW(KROW(KPHMH,NTPC+IFIL)+JPHMCO) = I
         IW(KROW(KPHMH,NTPC+IFIL)+JPHMTN) = NKIN
C
   40   CONTINUE
   20  CONTINUE
C
C Update the number of  filled columns
C
       IFIL = IFIL + NTPC
   10 CONTINUE
C
C Finished so reduce the size of PHMH
C
      IW(KPHMH+LMHROW) = IFIL
      CALL AUBPRS('PHMH')
      GOTO 999
C
C Handle errors
C - input bank missing
  901 IER = -1
      GOTO 999
C
C - not enough space
  902 IER = -3
C
  999 RETURN
      END
#endif
