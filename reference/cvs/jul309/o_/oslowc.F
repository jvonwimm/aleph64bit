      SUBROUTINE OSLOWC
C----------------------------------------------------------------------
C!  - Read BOM slow control record
C!
C!   Author:  R.W.Forty  16-Oct-91
C?
C!======================================================================
C
C June 14, 1996: O. Schneider: Since 1996, the BOMC bank is not produced 
C                              anymore !! Instead the LEP information is 
C                              stored in the LXCR and LXSP banks.
C                              This routine modified to also take the
C                              data from LXCR and LXSP banks.
C                              
#ifndef DOC
#include "bcs.h"
#include "bomflg.h"
#include "bomcal.h"
#include "bomcjj.h"
#include "lxcrjj.h"
#include "lxspjj.h"
C
C*      INTEGER NPRNT
      INTEGER LEP
      SAVE LEP 
      DATA LEP/0/
      INTEGER I, KBOMC, KLXCR, KLXSP
      INTEGER NWCOR,NWSEP,NCHCOR,NCHSEP
      PARAMETER(NWCOR=JLXCVA-JLXCNA,NCHCOR=4*NWCOR)
      PARAMETER(NWSEP=JLXSVA-JLXSNA,NCHSEP=4*NWSEP)
      CHARACTER*(NCHCOR) CHCOR
      CHARACTER*(NCHSEP) CHSEP
      REAL FACTOR,VALUE
      PARAMETER(FACTOR=100.)
#include "bmacro.h"
C.......................................................................
C
C
C     Read data from BOMC bank
C
      KBOMC = IW( NAMIND('BOMC') )
      IF (KBOMC .GT. 0) THEN 
C*        I=NPRNT('BOMC',IW(KBOMC-2))
        IF(LEP.EQ.2)
     &   CALL RERROR('OSLOWC',1,'LEP1 data after LEP2 data !!??')
        LEP=1
        DO I = 1, 29
          CURCOR(I) = FLOAT ( ITABL (KBOMC, 1, JBOMCU+I-1) )
        ENDDO
        XCURFL = .TRUE.
      ENDIF
C
C     Read corrector data from LXCR bank
C
      KLXCR = IW( NAMIND('LXCR') )
      IF (KLXCR .GT. 0) THEN 
C*        I=NPRNT('LXCR',IW(KLXCR-2))
        IF(LEP.EQ.1)
     &   CALL RERROR('OSLOWC',2,'LEP2 data after LEP1 data !!??')
        LEP=2
        DO I=7,29
          CURCOR(I)=0.
        ENDDO
        DO I=1,LROWS(KLXCR)
          CALL ALSTIN(IW(KROW(KLXCR,I)+JLXCNA),NWCOR,CHCOR)
          VALUE=FACTOR*RTABL(KLXCR,I,JLXCVA)
          IF(CHCOR.EQ.'CV_QS2_R4') CURCOR(11)=VALUE
          IF(CHCOR.EQ.'CV_QS2_L4') CURCOR(12)=VALUE
          IF(CHCOR.EQ.'CVC_QS0_R4') CURCOR(12)=VALUE
          IF(CHCOR.EQ.'CVC_QS0_L4') CURCOR(13)=VALUE
          IF(CHCOR.EQ.'CHB_QS2A_R4') CURCOR(16)=VALUE
          IF(CHCOR.EQ.'CHB_QS2A_L4') CURCOR(17)=VALUE
          IF(CHCOR.EQ.'CVA_QS4_R4') CURCOR(18)=VALUE
          IF(CHCOR.EQ.'CVA_QS4_L4') CURCOR(19)=VALUE
        ENDDO
C       CURCOR(28)=nominal LEP beam energy (online only)
        CURCOR(29)=FACTOR
        XCURFL = .TRUE.
      ENDIF
C
C     Read separator data from LXSP bank
C
      KLXSP = IW( NAMIND('LXSP') )
      IF (KLXSP .GT. 0) THEN 
C*        I=NPRNT('LXSP',IW(KLXSP-2))
        IF(LEP.EQ.1)
     &   CALL RERROR('OSLOWC',3,'LEP2 data after LEP1 data !!??')
        LEP=2
        DO I=1,6
          CURCOR(I)=0.
        ENDDO
        DO I=1,LROWS(KLXSP)
          CALL ALSTIN(IW(KROW(KLXSP,I)+JLXSNA),NWSEP,CHSEP)
          VALUE=FACTOR*RTABL(KLXSP,I,JLXSVA)
          IF(CHSEP.EQ.'CP4_GENM2P') CURCOR(01)=VALUE
          IF(CHSEP.EQ.'CP4_GENM2N') CURCOR(02)=VALUE
          IF(CHSEP.EQ.'CP4_GENM4P') CURCOR(03)=VALUE
          IF(CHSEP.EQ.'CP4_GENM4N') CURCOR(04)=VALUE
          IF(CHSEP.EQ.'CP4_GENM7P') CURCOR(05)=VALUE
          IF(CHSEP.EQ.'CP4_GENM7N') CURCOR(06)=VALUE
        ENDDO
C       Here we don't set XCURFL = .TRUE., because there is a bit of 
C       a logic problem (what if there is no LXCR bank ?); for the moment it's
C       OK because the separators are not used yet in the reconstruction ...
      ENDIF
C*      WRITE(*,1000) CURCOR
C* 1000 FORMAT(' CURCOR = ',(5F12.4))
      END
#endif
