      FUNCTION CORAD94(EN,TH,PH)
C -------------------------------------------------------------
CKEY GAMPACK ECAL
C! Correct ECAL cluster energy
C - Author: M.N Minard          930406
C - Revised : M.N Minard        981810 to use EGAZ bank when available
C - Input : EN      : Energy of cluster from 4 towers
C-          TH      : Cluster theta angle (rad)
C-          PH      : Cluster PHI angle (rad)
C - Output: CORAD94 : corrected energy
C -----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "egozjj.h"
#include "egazjj.h"
      EXTERNAL CHAINT, NAMIND
      CHARACTER*4 CHAINT
      CHARACTER*8 PRNAM
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2,LIST*8
      PARAMETER( IDEVRM = 241 )
C
      DATA DET    / 'EC' /
      DATA LIST   / 'EGOZEGAZ' /
      DATA IFIRST / 0 / 
      DATA IROLD, IROLDI, IROLDC  / 0 , 0 , 0 /
#include "bmacro.h"
C ----------------------------------------------------------------
       IF ( IFIRST .EQ. 0) THEN 
         NAEGOZ = NAMIND('EGOZ')
         NAEGAZ = NAMIND('EGAZ')
         IF ( IW(NAMIND('OLGA')).NE.O ) IROLDC =2
       ENDIF
       CORAD94 = 1.

       
C
C- Get EGOZ, EGAZ bank from DB
#include "gtdbbk.h"
C
C
C-     New bank asked , if not there old processing
C
C-     Check data or Monte-Carlo
C
       KRUNH = IW(NAMIND('RUNH'))
       IF (KRUNH.NE.0) IROLDI = IW(KRUNH+1)
       IF (IROLDI.LT.1001) IROLDI=0
       IF(IROLDC.EQ.0.AND.IW(NAEGAZ).EQ.0.AND.IFIRST.EQ.0) THEN 
         IROLDC = 1+IROLDC
       ENDIF
C
C-     Check type database version
C
       IF (IFIRST.EQ.0) THEN
         CALL ALVERS(ALVERC)
         ILVERC = INT(ALVERC)
         CALL ADBVER(ICVERC,IDVERC)
         IF(ICVERC.LT.IDVERM) IROLDC =1
       ENDIF
C
C-     Write warning 
C
       IF (IFIRST.EQ.0) THEN
         IF (IROLDC.GE.2) THEN
           WRITE(IW(6),201) ICVERC,ILVERC
         ELSEIF(IROLDC.EQ.1.AND.IROLDI.EQ.0)THEN 
           WRITE(IW(6),202) IDVERM-1
         ELSEIF((IROLDC.EQ.0.AND.IROLDI.EQ.0).OR.IROLDI.NE.0)THEN 
           WRITE(IW(6),203) ICVERC,ILVERC
         ENDIF
         IFIRST = 1
       ENDIF
       IF(IROLDC.NE.0)THEN 
         KEGOZ = IW(NAEGOZ)
         IF (KEGOZ.EQ.0) RETURN
C
C-     LOOK FOR CORRECTION FROM EGOZ BANK
C
         NEGOZ = LROWS(KEGOZ)
         COSI = ABS(COS(TH))
         COSI = MIN (COSI,0.999999)
         IROW = 0
         DO IEGOZ = 1,NEGOZ
           IF (COSI.GE.RTABL(KEGOZ,IEGOZ,JEGOTM).AND.
     &         COSI.LT.RTABL(KEGOZ,IEGOZ,JEGOTX)) IROW=IEGOZ
         ENDDO
         GCOR = RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(EN**RTABL(KEGOZ,IROW,JEGOC3)))
         IF(EN.GT.RTABL(KEGOZ,IROW,JEGOC4))THEN
           CORAD94 = 1./GCOR
         ELSE
           ESU = RTABL(KEGOZ,IROW,JEGOC4)
           GCORS= RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(ESU**RTABL(KEGOZ,IROW,JEGOC3)))
           EIN = RTABL(KEGOZ,IROW,JEGOC5)
           GCORI= RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(EIN**RTABL(KEGOZ,IROW,JEGOC3)))
           GCOR1 = (1./(RTABL(KEGOZ,IROW,JEGOC4)-RTABL(KEGOZ,IROW,
     &              JEGOC5)))*(GCORS*(EN-RTABL(KEGOZ,IROW,JEGOC5))
     &              + GCORI*(RTABL(KEGOZ,IROW,JEGOC4)-EN))
           CORAD94 = 1./GCOR1
         ENDIF
       ELSE
         KEGAZ = IW(NAEGAZ)
         IF (KEGAZ.EQ.0) RETURN
C
C-     LOOK FOR CORRECTION FROM EGAZ BANK
C
         NEGAZ = LROWS(KEGAZ)
         COSI = ABS(COS(TH))
         COSI = MIN (COSI,0.999999)
         IROW = 0
         DO IEGAZ = 1,NEGAZ
           IF (COSI.GE.RTABL(KEGAZ,IEGAZ,JEGATM).AND.
     &         COSI.LT.RTABL(KEGAZ,IEGAZ,JEGATX)) IROW=IEGAZ
         ENDDO
         GCOR = (1.-RTABL(KEGAZ,IROW,JEGAC1)
     &         /(EN**RTABL(KEGAZ,IROW,JEGAC3)))
         IF(EN.GT.RTABL(KEGAZ,IROW,JEGAC4))THEN
           IF (EN.GT.RTABL(KEGAZ,IROW,JEGAC6))THEN 
             GCOR = GCOR *(1.-RTABL(KEGAZ,IROW,JEGAC2)
     &             *SQRT(EN-RTABL(KEGAZ,IROW,JEGAC6)))
           ENDIF
           CORAD94 = 1./GCOR
         ELSE
           ESU = RTABL(KEGAZ,IROW,JEGAC4)
           GCORS= (1.-
     &        RTABL(KEGAZ,IROW,JEGAC1)/(ESU**RTABL(KEGAZ,IROW,JEGAC3)))
           EIN = RTABL(KEGAZ,IROW,JEGAC5)
           GCORI= (1.-
     &        RTABL(KEGAZ,IROW,JEGAC1)/(EIN**RTABL(KEGAZ,IROW,JEGAC3)))
           GCOR1 = (1./(RTABL(KEGAZ,IROW,JEGAC4)-RTABL(KEGAZ,IROW,
     &             JEGAC5)))*(GCORS*(EN-RTABL(KEGAZ,IROW,JEGAC5))
     &            + GCORI*(RTABL(KEGAZ,IROW,JEGAC4)-EN))
           CORAD94 = 1./GCOR1
         ENDIF
       ENDIF
 201  FORMAT(//1X,'+---------------------------------------------+'/
     .       1X,'|     Gamma correction package used           |'/
     .       1X,'|     Database ',I4,' ALEPHLIB',I6,'            |'/
     .       1X,'|                                             |'/
     .       1X,'|     Gamma correction as in ALEPHLIB309      |'/
     .       1X,'+---------------------------------------------+'//)
 202  FORMAT(//1X,'+---------------------------------------------+'/
     .       1X,'|     Gamma correction package used           |'/
     .       1X,'|     Database ',I4,' ALEPHLIB 309              |'/
     .       1X,'|                                             |'/
     .       1X,'|     Gamma correction as in ALEPHLIB309      |'/
     .       1X,'|     EGAZ bank missing                       |'/
     .       1X,'+---------------------------------------------+'//)
 203   FORMAT(//1X,'+---------------------------------------------+'/
     .       1X,'|     Gamma correction package updated        |'/
     .       1X,'|     Database ',I4,' ALEPHLIB',I6,'            |'/
     .       1X,'|     EGAZ bank used for data                 |'/
     .       1X,'+---------------------------------------------+'//)


C
       RETURN
       END
#endif
