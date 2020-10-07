*DK siasig
      SUBROUTINE SIASIG
C--------------------------------------------------------------
C! Controls the analog signal processing in SCal
C. - B.Bloch-Devaux October 91
C. - Called by  ASASIG                             from this Lib
C. - Calls      BDROP                              from BOS  Lib
C. - Calls      AUBPRS,PRTABL,SIDCOD,CAFIHT,ALTELL from ALEPHLIB
C --------------------------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
#include "sinamc.h"
#include "sistat.h"
      INTEGER CAFIHT
#include "bmacro.h"
C -------------------------------------------------------------
C
C - If no SIHI exists nothing to be done
      IF (IW(NASIHI) .LE. 0) GO TO 999
C - Reduce bank size to real one
      CALL AUBPRS('SIHI')
C - protect against zero length
      IF (LROWS(IW(NASIHI)).LE.0) GO TO 999
      IF (NSIPRT.GT.0)  CALL PRTABL('SIHI',0)
C - Fill hit bank
      JSIHT = CAFIHT(NASIHI,'SIHT')
      IF (JSIHT.LE.0) GO TO 998
      CALL BLIST (IW,'T+','SIHT')
C - count number of pads hit per event
      RET = 0.
      DO 120 I=1 , LROWS(JSIHT)
      DO 120 J = 1,3
         IF ( ITABL(JSIHT,I,J+1).GT.0) NSICOU(4)=NSICOU(4)+1
         RET =RET+ 0.001*FLOAT(ITABL(JSIHT,I,J+1))
 120  CONTINUE
      IF (NSIPRT.GT.0) THEN
         WRITE (LOUTIO,'(1X,''Total pad energy (Mev)  '',F10.3)') RET
         IF (NSIPRT.GE.2) THEN
            WRITE (LOUTIO,*)' +++SIASIG+++'
            WRITE (LOUTIO,*)'  SIHT bank : nrows/ncols ',
     &             LROWS(JSIHT),LCOLS(JSIHT)
            WRITE (LOUTIO,*)' Pad hits bank SIHT'
            WRITE (LOUTIO,777)
            DO 130 J =1 , LROWS(JSIHT)
               IAD = IW(KROW(JSIHT,J)+1)
               CALL SIDCOD(IAD,0,IMD,IST,IPH,IRD)
               WRITE(LOUTIO,'(9I8)') IMD,IPH,IRD,IST,IST+1,IST+2,
     $              (IW(KROW(JSIHT,J)+K),K=2,4)
  130       CONTINUE
          ENDIF
       ENDIF
C
      ESICOU(10)= ESICOU(10)+DBLE(RET**2)
      IF (FHISJO(9)) CALL SIFILH(1)
C
 999  RETURN
 777  FORMAT(3X,'Module    phi radius      Planes',19X,'Hits')
C
C - not enough space
C
 998  CONTINUE
      CALL ALTELL ('SIASIG: not enough space for HIT banks ',1,'NEXT')
      END
#endif
