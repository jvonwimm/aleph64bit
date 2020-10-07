      SUBROUTINE HRDCAL
C********************************************************************
C! Read calibrations form Data Base                                 *
C                                                                   *
C  This reads Intermodule calibrations and time dependence          *
C                                                                   *
C    Author: R.Tenchini  10.3.90                                    *
C!                                                                   *
C!    Mod     : A.Messineo                                           *
C!                  19.7.95                                          *
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "htowca.h"
#include "hstdjj.h"
#include "ht0cjj.h"
#include "himcjj.h"
#include "ht0wjj.h"
      EXTERNAL ALGTDB
      INTEGER ALGTDB
      CHARACTER*16 LISTN
      DATA LISTN /'HSTDHT0CHIMCHT0W'/
#include "bmacro.h"
C
C    Initializations first
C
      DO 10 I=1,24
      CALBA(I)=1.
 10   CONTINUE
      DO 20 I=1,12
      CALEC(I)=1.
 20   CONTINUE
      CALTBA=1.
      CALTEC=1.
      CT0MCB=1.
      CT0MCE=1.
      CT0DAB=1.
      CT0DAE=1.
      DO I = 1,4
       CT0DWB(I) = 1.
       CT0DWE(I) = 1.
      ENDDO
c...
      IFLAG = ALGTDB(LRGEOM,LISTN,-IRUNRC)
      IF(JDBDRF(JULHC).GE.2 ) THEN
         WRITE(LDEBRL,*) ' HRDCAL : Flag from DBASE ',IFLAG
      ENDIF
      JHIMC=IW(NAMIND('HIMC'))
      IF(JHIMC.NE.0) THEN
         DO 30 I=1,24
         CALBA(I)=RTABL(JHIMC,1,JHIMBC+I-1)
  30     CONTINUE
         DO 40 I=1,12
         CALEC(I)=RTABL(JHIMC,1,JHIMEC+I-1)
  40     CONTINUE
      ENDIF
      JHT0C=IW(NAMIND('HT0C'))
      IF(JHT0C.NE.0) THEN
         CT0MCB=RTABL(JHT0C,1,JHT0BM)
         CT0MCE=RTABL(JHT0C,1,JHT0EM)
         CT0DAB=RTABL(JHT0C,1,JHT0BD)
         CT0DAE=RTABL(JHT0C,1,JHT0ED)
      ENDIF
c... read the inter-wagon  calibration
      JHT0W=IW(NAMIND('HT0W'))
      IF(JHT0W.NE.0) THEN
c... barrel
         CT0DWB(1)=RTABL(JHT0W,1,JHT0B1)
         CT0DWB(2)=RTABL(JHT0W,1,JHT0B2)
         CT0DWB(3)=RTABL(JHT0W,1,JHT0B3)
         CT0DWB(4)=RTABL(JHT0W,1,JHT0B4)
c... endcap
         CT0DWE(1)=RTABL(JHT0W,1,JHT0E1)
         CT0DWE(2)=RTABL(JHT0W,1,JHT0E2)
         CT0DWE(3)=RTABL(JHT0W,1,JHT0E3)
         CT0DWE(4)=RTABL(JHT0W,1,JHT0E4)
      ENDIF
c
      JHSTD=IW(NAMIND('HSTD'))
      IF(JHSTD.NE.0) THEN
         CALTBA=RTABL(JHSTD,1,JHSTBL)
         CALTEC=RTABL(JHSTD,1,JHSTES)
      ENDIF
      IF(JDBDRF(JULHC).GE.2 ) THEN
         WRITE(LDEBRL,*) ' HRDCAL : Intermodule Calib Barrel'
         WRITE(LDEBRL,1) (CALBA(J),J=1,24)
         WRITE(LDEBRL,*) ' HRDCAL : Intermodule Calib Endcaps'
         WRITE(LDEBRL,1) (CALEC(J),J=1,12)
         WRITE(LDEBRL,*) ' HRDCAL : Normalizations at T=0'
         WRITE(LDEBRL,*) CT0MCB,CT0MCE,CT0DAB,CT0DAE
         WRITE(LDEBRL,2) (CT0DWB(J), J=1,4)
         WRITE(LDEBRL,2) (CT0DWE(J), J=1,4)
         WRITE(LDEBRL,*) ' HRDCAL : time dependence'
         WRITE(LDEBRL,*) CALTBA,CALTEC
      ENDIF
      RETURN
 1    FORMAT(6(2X,F8.4))
 2    FORMAT(4(2X,F8.4))
      END
#endif
