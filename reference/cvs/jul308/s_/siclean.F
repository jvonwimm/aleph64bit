      SUBROUTINE SICLEAN(kcl,iflag)
C----------------------------------------------------------------------
C!  - Checks for quality flag of cluster 
C!
C!
C!    Author:     B. Bloch-Devaux   June 1998
C!
C!    Input:      - kcl = cluster number in SCLS, if 0 = initialisation
C!                _ banks 'SCLS' from Sical clusters
C!                  from data base: 'SNOI' to setup noise cuts
C!    Output:
C!                - iflag quality flag 0 = OK, 1= oscillating , 2= noise
C?
C!======================================================================
#ifndef DOC
      SAVE nclu,ifclu,silcut,iruno,icut3
#include "alcons.h"
#include "bcs.h"
#include "sclsjj.h"
#include "snoijj.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rparac.h"
#include "sinamc.h"
      PARAMETER( LMAXC = 20)
      DIMENSION SILCUT(3),IFCLU(LMAXC)
      DIMENSION efrac(12),npad(2)
C==================================================================
      DATA IRUNO/0/
      DATA SILCUT/1.,5.,16./
#include "bmacro.h"
C----------------------------------------------------------------------
      IF ( IRUNRC.NE.IRUNO) THEN
         IRUNO= IRUNRC
         NASNOI = namind('SNOI')
         KSNOI = IW(NASNOI)
         IF ( KSNOI.GT.0) THEN
            SILCUT(1) = RTABL(KSNOI,1,JSNOEF)
            SILCUT(2) = RTABL(KSNOI,1,JSNORV)
            SILCUT(3) = RTABL(KSNOI,1,JSNONP)
         ENDIF
         icut3 = int(SILCUT(3))
         IF(JDBDRF(JULSI).GE.1) THEN
            WRITE (IW(6),1000) 
     $      (SILCUT(LI),LI=1,3)
         ENDIF
      ENDIF
      iflag = 0
      If ( kCL.le.0) THEN
C loop on clusters found to identify possible amplex oscillations
        CALL VZERO(IFCLU,LMAXC)
        JSCLS = IW(NASCLS)
        IF (JSCLS .LE. 0) GO TO 999
        NCLU = LROWS(JSCLS)
        IF ( NCLU.LE.0) GO TO 999
        DO 320 ICL = 1, NCLU
          EN = RTABL(JSCLS,ICL,JSCLEC)
          DR = RTABL(JSCLS,ICL,JSCLWR)
          CALL SIFSUM(ICL,EFRAC,NPAD)
          efr =0.
          ipn = 0
          do ii= 1,12
            efrac(ii) = efrac(ii)/en
            if ( efrac(ii).ge.efr) then
               efr = efrac(ii)
               ipn = ii
            endif
          enddo
C   oscillation : energy fraction in one plane > min ,Radial variance >min, 
C   N>1 phi sector (all R's)
          if((efrac(ipn).ge.SILCUT(1)).and.(dr.ge.SILCUT(2)) 
     &    .and.(mod(npad(1),icut3).eq.0)) IFCLU(ICL) = 1
  320 CONTINUE
C
      ELSE IF ( (KCL.gt.0).and.(KCL.le.nclu)) then
           iflag = ifclu(kcl)
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
 1000 FORMAT (
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +   SICAL CUTS FOR Amplex oscillation identification   +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +  Min energy fraction in one z plane  : ',F6.2,'      +',/,
     $' +  Minimum Radial variance             : ',F6.2,'      +',/,
     $' +  Typical number of pads              : ',F6.2,'      +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      END
#endif
