      SUBROUTINE SICLEAN(kcl,iflag)
C----------------------------------------------------------------------
C!  - Checks for quality flag of cluster
C!
C!
C!    Author:     B. Bloch-Devaux   June 1998
C!                modified April 99 to extend to more backgrounds types
C!
C!    Input:      - kcl = cluster number in SCLS, if 0 = initialisation
C!                _ banks 'SCLS' from Sical clusters
C!                  from data base: 'SNOI' to setup background cuts
C!    Output:
C!                - iflag quality flag 0 = OK,1= oscillating,2= beam loss
C!                                     3=backwards,4=scraping,5=offmom
C?
C!======================================================================
#ifndef DOC
      SAVE nclu,ifclu,silcut,iruno,icut3,sibcut,sikcut,siscut,siocut
#include "alcons.h"
#include "bcs.h"
#include "sclsjj.h"
#include "snoijj.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rparac.h"
#include "sinamc.h"
#include "siidjj.h"
      PARAMETER( LMAXC = 20)
      DIMENSION SILCUT(3),IFCLU(LMAXC)
      DIMENSION SIBCUT(3),SIKCUT(3),SISCUT(3),SIOCUT(3)
      DIMENSION efrac(12),npad(2)
C==================================================================
      DATA IRUNO/0/
      DATA SILCUT/.999,5.,16./
      DATA SIBCUT/ 1.7,400.,0./, SIKCUT/.25,.25,0./
      DATA SISCUT/ 0.033,0.033,0./, SIOCUT/0. ,0. ,0./
#include "bmacro.h"
C----------------------------------------------------------------------
      IF ( IRUNRC.NE.IRUNO) THEN
         IRUNO= IRUNRC
         NASNOI = namind('SNOI')
         KSNOI = IW(NASNOI)
         nrows = LROWS(KSNOI)
         IF ( KSNOI.GT.0) THEN
            DO ICUT = 1,3
            SILCUT(ICUT) = RTABL(KSNOI,1,JSNOEF+ICUT-1)
            IF (NROWS.GT.1)SIBCUT(ICUT) = RTABL(KSNOI,2,JSNOEF+ICUT-1)
            IF (NROWS.GT.2)SIKCUT(ICUT) = RTABL(KSNOI,3,JSNOEF+ICUT-1)
            IF (NROWS.GT.3)SISCUT(ICUT) = RTABL(KSNOI,4,JSNOEF+ICUT-1)
            IF (NROWS.GT.4)SIOCUT(ICUT) = RTABL(KSNOI,5,JSNOEF+ICUT-1)
            enddo
         ENDIF
         icut3 = int(SILCUT(3))
         ebeam = 0.5* RENLEP   
         IF(JDBDRF(JULSI).GE.1) THEN
            WRITE (IW(6),1000) (SILCUT(LI),LI=1,3),(SIBCUT(LI),LI=1,3),
     $      (SIKCUT(LI),LI=1,3),(SISCUT(LI),LI=1,3),(SIOCUT(LI),LI=1,3)
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
C book SIID bank with same length as SCLS
        LEN = NCLU*LSIIDA + LMHLEN
        CALL AUBOS('SIID',0,LEN,KSIID,IGARB)
        IF (IGARB.EQ.2) GO TO 998
        IW(KSIID+LMHROW) = NCLU
        IW(KSIID+LMHCOL) = LSIIDA   
        DO 320 ICL = 1, NCLU
          EN = RTABL(JSCLS,ICL,JSCLEC)
          DR = RTABL(JSCLS,ICL,JSCLWR)
          ZC = RTABL(JSCLS,ICL,JSCLZC)
          TE = RTABL(JSCLS,ICL,JSCLCT)
          if ( zc.lt.0.) TE = pi - TE
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
     &    .and.(mod(npad(1),icut3).eq.0)) THEN
               IFCLU(ICL) = 1
C   Beam loss : energy > frac * beam energy, npad > min
          else if((en.gt.SIBCUT(1)*EBEAM).and.
     &            (npad(1).GE.SIBCUT(2))) then
               IFCLU(ICL) = 2
C   Backwards : fraction in first 3 planes < max1,
C               fraction in last 3 planes > min1
          else if((efrac(1)+efrac(2)+efrac(3).lt.SIKCUT(1)).and.
     &          (efrac(10)+efrac(11)+efrac(12).gt.SIKCUT(2))) then
               IFCLU(ICL) = 3
C   Scraping particles : Theta smaller than TminA ( B)
          else if (((TE.LT.SISCUT(1)).and.(zc.gt.0.)).OR.((TE.LT.
     &      SISCUT(2)).and.(zc.lt.0.))) then
               IFCLU(ICL) = 4
C   Off momentum : not yet implemented
          endif
          KID = KROW(KSIID,ICL)
          IW(KID+JSIICI) = IFCLU(ICL)
          IW(KID+JSIISC) = ICL
  320 CONTINUE
C
      ELSE IF ( (KCL.gt.0).and.(KCL.le.nclu)) then
           iflag = ifclu(kcl)
      ENDIF
C----------------------------------------------------------------------
      GO TO 999
  998 CALL RERROR('SICLEAN', -2, ' no room for SIID')
  999 CONTINUE
      RETURN
 1000 FORMAT (
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +   SICAL CUTS FOR Amplex oscillation identification   +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +  Min energy fraction in one z plane  : ',F6.3,'      +',/,
     $' +  Minimum Radial variance             : ',F6.3,'      +',/,
     $' +  Typical number of pads              : ',F6.1,'      +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +   SICAL CUTS FOR Beam loss identification            +',/,
     $' +  Min cluster energy /Beam energy     : ',F6.3,'      +',/,
     $' +  Minimum number of pads              : ',F6.1,'      +',/,
     $' +  not used                            : ',F6.2,'      +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +   SICAL CUTS FOR Backwards identification            +',/,
     $' +  Max energy fraction in first 3 planes  : ',F6.3,'   +',/,
     $' +  Min energy fraction in last 3 planes   : ',F6.3,'   +',/,
     $' +  not used                               : ',F6.2,'   +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +   SICAL CUTS FOR Scraping identification             +',/,
     $' +  Max polar angle on side A           : ',F6.4,'      +',/,
     $' +  Max polar angle on side B           : ',F6.4,'      +',/,
     $' +  not used                            : ',F6.4,'      +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++',/,
     $' +   SICAL CUTS FOR Off momemntum identification        +',/,
     $' +  not yet used                           : ',F6.2,'   +',/,
     $' +  not yet used                           : ',F6.2,'   +',/,
     $' +  not yet used                           : ',F6.2,'   +',/,
     $' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      END
#endif
