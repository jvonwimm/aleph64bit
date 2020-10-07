      SUBROUTINE HCRDCA
C------------------------------------------------------------------
C
C!  Reads the calibration constants for the year
C!
C!           Author: A.Sciaba`  29/04/98
C!           Modified: 18/08/98
C!                -added HIMM bank
C!           Input banks: HSCA,HT0C,HIMC,HSTD,HIFC,HIMM
C!
C!
C!         -Called by : HCIRUN
C!         -Calls     : MDARD                      from BOS
C------------------------------------------------------------------
#ifndef DOC
      INTEGER YEAR, ADBRPN, ALGTDB
#include "asimjj.h"
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "hcalct.h"
#include "hccoun.h"
#include "bmacro.h"
C -----------------------------------------------------------------
C
C     Initializing some variables
      DO I=1,24
         CALBA(I) = 1.
         CALIMB(I) = 1.
         CALMCB(I) = 1.
      ENDDO
      DO I=1,12
         CALEC(I) = 1.
         CALIME(I) = 1.
         CALMCE(I) = 1.
      ENDDO
      CALTBA = 1.
      CALTEC = 1.
      CT0MCB = 1.
      CT0MCE = 1.
      CT0DAB = 1.
      CT0DAE = 1.
      CALTUB = 1.
      CALTUE = 1.
      JHSCA = 0
      JHIFC = 0
      JHIMM = 0
C     
C     
C     Load HNGR
      IND = MDARD(IW,LRDBIO,'HNGR',0)
C
      IF(FHCDEB)THEN
         WRITE(LOUTIO,*)' HCRDCA - Reading HCAL calibration',
     &        ' constants:'
      ENDIF
C
C     Look at the period number
      JASIM = IW(NAMIND('ASIM'))
      IPERIOD = ITABL(JASIM,1,JASIYM)
C
C      Get the setup code
      IER = ADBRPN('HC',IROW,IPERIOD,JRUN,JSTUP)
C
C     Select the appropriate calibration costants for each year
      IND = MDARD(IW,LRDBIO,'HSCA',0)
      JHSCA = IW(NAMIND('HSCA'))
      IF(JHSCA.NE.0) THEN
         IRHT0C = ITABL(JHSCA,JSTUP,1)
         IRHSTD = ITABL(JHSCA,JSTUP,2)
         IRHIMC = ITABL(JHSCA,JSTUP,3)
         IF(FHCDEB)THEN
            WRITE(LOUTIO,*)'SETUP CODE  HT0C    HSTD    HIMC' 
            WRITE(LOUTIO,'(4(I8))')JSTUP,IRHT0C,IRHSTD,IRHIMC
         ENDIF
      ELSE
         IF(FHCDEB)THEN
            WRITE(LOUTIO,*)'ERROR: HCSA NOT FOUND!'
         ENDIF
         GOTO 10
      ENDIF
C
C     Calibration constants from HT0C, HSTD, HIMC
      IND = ALGTDB(LRDBIO,'HIMC',IRHIMC)
      JHIMC = IW(NAMIND('HIMC'))
      IF(JHIMC.NE.0) THEN
         DO I=1,24
            CALBA(I) = RTABL(JHIMC,1,I)
         ENDDO
         DO I=1,12
            CALEC(I) = RTABL(JHIMC,1,24+I)
         ENDDO
      ENDIF
      IND = ALGTDB(LRDBIO,'HSTD',IRHSTD)
      JHSTD = IW(NAMIND('HSTD'))
      IF(JHSTD.NE.0) THEN
         CALTBA = RTABL(JHSTD,1,1)
         CALTEC = RTABL(JHSTD,1,2)
      ENDIF
      IND = ALGTDB(LRDBIO,'HT0C',IRHT0C)
      JHT0C = IW(NAMIND('HT0C'))
      IF(JHT0C.NE.0) THEN
         CT0MCB = RTABL(JHT0C,1,1)
         CT0MCE = RTABL(JHT0C,1,3)
         CT0DAB = RTABL(JHT0C,1,2)
         CT0DAE = RTABL(JHT0C,1,4)
      ENDIF
C
C     Load HIFC (with calibration costants on FICs)
      NR = NDANR(LRDBIO,'HIFC','LE',JSTUP)
      IF (NR.EQ.0) GOTO 3
      IND = MDARD(IW,LRDBIO,'HIFC',NR)
      JHIFC = IW(NAMIND('HIFC'))
 3    IF(JHIFC.NE.0) THEN
         DO I=1,24
            CALIMB(I) = RTABL(JHIFC,1,I)
         ENDDO
         DO I=1,12
            CALIME(I) = RTABL(JHIFC,1,24+I)
         ENDDO
      ENDIF

C     Load HIMM (with calibration costants of MC)
      NR = NDANR(LRDBIO,'HIMM','LE',JSTUP)
      IF (NR.EQ.0) GOTO 4
      IND = MDARD(IW,LRDBIO,'HIMM',NR)
      JHIMM = IW(NAMIND('HIMM'))
 4    IF(JHIMM.NE.0) THEN
         DO I=1,24
            CALMCB(I) = RTABL(JHIMM,1,I)
         ENDDO
         DO I=1,12
            CALMCE(I) = RTABL(JHIMM,1,24+I)
         ENDDO
      ENDIF
C      
C     Load calibration constant from online monitoring
      IND = MDARD(IW,LRDBIO,'HCOL',0)
      JHCOL = IW(NAMIND('HCOL'))
      IF(JHCOL.NE.0) THEN
         CALTUB = RTABL(JHCOL,JSTUP,1)
         CALTUE = CALTUB
      ENDIF
C
 10   CONTINUE
C
c     Printout of the calibration constants
      IF(FHCDEB)THEN
         WRITE(LOUTIO,*) ' HCRDCA : Intermodule Calib Barrel'
         WRITE(LOUTIO,1) (CALBA(J),J=1,24)
         WRITE(LOUTIO,*) ' HCRDCA : Intermodule Calib Endcaps'
         WRITE(LOUTIO,1) (CALEC(J),J=1,12)
         WRITE(LOUTIO,*) ' HCRDCA : Normalizations at T=0'
         WRITE(LOUTIO,*) CT0MCB,CT0MCE,CT0DAB,CT0DAE
         WRITE(LOUTIO,*) ' HCRDCA : time dependence'
         WRITE(LOUTIO,*) CALTBA,CALTEC
         WRITE(LOUTIO,*) ' HCRDCA : Online Intermodule Calib Barrel'
         WRITE(LOUTIO,1) (CALIMB(J),J=1,24)
         WRITE(LOUTIO,*) ' HCRDCA : Online Intermodule Calib Endcaps'
         WRITE(LOUTIO,2) (CALIME(J),J=1,12)
         WRITE(LOUTIO,*) ' HCRDCA : MC  Intermodule Calib Barrel'
         WRITE(LOUTIO,1) (CALMCB(J),J=1,24)
         WRITE(LOUTIO,*) ' HCRDCA : MC Intermodule Calib Endcaps'
         WRITE(LOUTIO,2) (CALMCE(J),J=1,12)
         WRITE(LOUTIO,*) ' HCRDCA : Online Monitoring Constant'
         WRITE(LOUTIO,*) CALTUB
         WRITE(LOUTIO,*) ' '
      ENDIF
C
 1    FORMAT(6(2X,F8.4))
 2    FORMAT(4(2X,F8.4))
C
      RETURN
      END
#endif
