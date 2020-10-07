      SUBROUTINE HCALIB(IPOTG)
C**************************************************
C!  Single tower calibraton of hcal               *
C   Changes the contents of bank hsda             *
C                                                 *
C   Author: F. Ligabue    11/14/88                *
C           modified R.Tenchini                   *
C                                                 *
C   IPOTG = 0 running on raw data                 *
C           1 running on POT                      *
C                                                 *
C   Input banks: HSDA, H1EC, H2EC, H3EC, H4EC     *
C   Output banks: HSDA                            *
C                                                 *
C   Called by HPRANA                              *
C   Calls HCALIB                                  *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "hsdajj.h"
#include "hwbnkw.h"
      PARAMETER(ITMI1=1,ITMI2=5,ITMI3=11,ITMI4=18)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA IFLAG /0/
#include "bmacro.h"
C
      IHSDA = NLINK('HSDA',0)
      IF(IHSDA.EQ.0) THEN
         CALL RERROR('HCALIB',1,'HSDA NOT FOUND')
         GOTO 770
      ENDIF
      LWORK=LROWS(IHSDA)
      IF(FIRST) THEN
         FIRST=.FALSE.
         ICORHW=0
      ENDIF
      CALL WBANK(IW,ICORHW,LWORK,*200)
C
C...LOOP OVER TOWERS
C
      DO 100 I=1,LROWS(IHSDA)
         IPH0   = ITABL(IHSDA,I,JHSDPI)
         ITH0   = ITABL(IHSDA,I,JHSDTI)
         ECALB  = RTABL(IHSDA,I,JHSDDE)
         RW(ICORHW+I)=1.
C
C...READ CALIBRATION CONSTANT FROM BANK HCAC
C
         CALL HCSIMM(ITH0,IPH0,ITH1,IPH1,IRGN)
         IF(IRGN.EQ.0)THEN
           CALL RERROR('HCALIB',2,'IRGN = 0')
           GOTO 770
         ENDIF
         IF(IRGN.EQ.1)THEN
            NHCAC  = NAMIND('H1EC')
            IHCAC  = IW(NHCAC)
            IMIN   = ITMI1
         ENDIF
         IF(IRGN.EQ.2)THEN
            NHCAC  = NAMIND('H2EC')
            IHCAC  = IW(NHCAC)
            IMIN   = ITMI2
         ENDIF
         IF(IRGN.EQ.3)THEN
            NHCAC  = NAMIND('H3EC')
            IHCAC  = IW(NHCAC)
            IMIN   = ITMI3
         ENDIF
         IF(IRGN.EQ.4)THEN
            NHCAC  = NAMIND('H4EC')
            IHCAC  = IW(NHCAC)
            IMIN   = ITMI4
         ENDIF
         IF(IHCAC.EQ.0)THEN
            CALL RERROR('HCALIB',3,' CALIB COSTANTS NOT FOUND ')
            GOTO 770
         ENDIF
         CALCN = RTABL(IHCAC,ITH1+1-IMIN,IPH1)
            IF(CALCN.EQ.0.) THEN
               CALL RERROR('HCALIB',4,'CALCN=0')
               GOTO 770
            ENDIF
C
C...REPLACE BANK CONTENTS BY CALIBRATED ENERGIES
C
         IF(IPOTG.EQ.0) THEN
            INDEN = KROW(IHSDA,I)+JHSDDE
            RW(INDEN) = ECALB*CALCN
         ENDIF
         RW(ICORHW+I)=CALCN
C
  100 CONTINUE
  770 CONTINUE
      GO TO 999
 200  CALL RERROR('HCALIB',5,'no space for work bank')
 999  RETURN
      END
#endif
