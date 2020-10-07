*DK vdroch
      SUBROUTINE VDROCH(IMOD,IWAF,NHITT)
C!----------------------------------------------------------------------
C! Compute signal on readout channels
CKEY VDET DIGITIZE
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  Algorithm copied from VDROST, by P. Cattaneo :
C!  For each fired strip,
C!      deposit charge on the neighbouring readout channels,
C!      proportional to the capacitive coupling coefficients
C!  The readout channel number has to take into account
C!  possible multiplexing
C!
C! Input :  IMOD, IWAF : Input, module and wafer numbers
C!          NHITT : total number of hits in the event
C! Input :  VWS1, VWS2 banks
C!
C! Output : VWC1, VWC2 banks
C!
C-----------------------------------------------------------------------
C
#ifndef DOC
      SAVE NAVWSX,NAVWCX
C
      PARAMETER (ICDIM=10)
      DIMENSION CCOEF(2,0:ICDIM),MXCPL(2),COEFN(2,ICDIM)
      DIMENSION MINIC(2),MAXIC(2)
      DIMENSION MUXL(2),MAXC(2),NROS(2),IPLSS(2)
      DIMENSION NAVWSX(2),MAXS(2),ISOFF(2),IPICH(2),CSTRP(2)
      DIMENSION NAVWCX(2)
#include "vws1jj.h"
#include "vwc1jj.h"
#include "bcs.h"
      INTEGER VNISOF,VNIRFS
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
C
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVWSX(1) = NAMIND('VWS1')
         NAVWSX(2) = NAMIND('VWS2')
         NAVWCX(1) = NAMIND('VWC1')
         NAVWCX(2) = NAMIND('VWC2')
      ENDIF
C
C Get some module quantities
C
      CALL VFNDMP
     $   (IMOD,MAXS,IPICH,MAXC,MUXL,CCOEF,
     $   COEFN,CSTRP,ICDIM,MXCPL)
C
C Obtain number of readout strips in one wafer (NROS)
C
      CALL VFNDMC(1,MAXC,NROS,MUXL,IPICH)
         ISOFF(1)=NROS(1)
         ISOFF(2)=0
      DO IV=1,2
C Removed when VNISOFF disappeared from geometry package (1 Nov '94)
C Replaced by 2 lines above
C         ISOFF(IV) = VNISOF(IV)
C
C IRFS is the strip number of the first readout strip (0 or 1)
C
         IRFS =  VNIRFS(IV)
C
C We compute IPLSS so that the equation below gives the correct answer
C for the readout channel number which corresponds to a given strip
C          ICH = (ISTR+IPLSS(IV) )/IPICH(IV)
C
         IPLSS(IV) = IPICH(IV)-IRFS
      ENDDO
C      ISOFF(1) = NROS(1)
C      ISOFF(2) = 0
C
C Compute the limits of readout channels for the wafer under
C consideration
C
      MINIC(1) = (IWAF-1)*NROS(1)+1
      MAXIC(1) = IWAF*NROS(1)
      MINIC(2) = 1
      MAXIC(2) = MAXC(2)
C
C IRFS is the strip number of the first readout strip (0 or 1)
C
C      IRFS(1)  = 1
C      IRFS(2)  = 1
C
      DO 12 IV=1,2
        KVWSX = IW(NAVWSX(IV))
        KVWCX = IW(NAVWCX(IV))
        NSTR  = LROWS(KVWSX)
C
C Loop over strips
C
        DO 11 ISTR=1,NSTR
          ESTR = RTABL(KVWSX,ISTR,JVWSSC)
          IF(ESTR.EQ.0.)GO TO 11
C
C Compute central readout channel number
C
          ICH = (ISTR+IPLSS(IV))/IPICH(IV)
C
C Make universal channel number (inside one module)
C
          ICOFF = (IWAF-1)*ISOFF(IV)
          ICHC = ICH+ICOFF
          MXCH = MXCPL(IV)/IPICH(IV) + 1
C
C
C Number of readout channels affected by one strip IS MXCPL(IV)
C
C Minimum and maximum readout channels for this strip
C
          IMIN = MAX(ICHC - MXCH,MINIC(IV))
          IMAX = MIN(ICHC + MXCH,MAXIC(IV))

          DO 10 ICH = IMIN,IMAX
C
C Compute index of the coupling coefficient
C ((ICH-ICADD)*IPICH(IV)-IPLSS ) transforms the readout channel
C                           into a strip address
C
            ICPL = IABS(ISTR-((ICH-ICOFF)*IPICH(IV)-IPLSS(IV)))
            IF(ICPL.GT.MXCPL(IV))GO TO 10
C            ICHOU = MOD(ICH,MUXL(IV))
            ICHOU = ICH
            IF(ICHOU.GT.MUXL(IV))ICHOU=ICHOU-MUXL(IV)
            EOLD = RTABL(KVWCX,ICHOU,JVWCCC)
            CC = CCOEF(IV,ICPL)
            RW(KROW(KVWCX,ICHOU)+JVWCCC)=EOLD+ESTR*CC
            IROLD = ITABL(KVWCX,ICHOU,JVWCVT)
            CALL VINSTC(ICHOU,ISTR,CC,IMOD,IV,NHITT,IROLD,IRNEW)
            IW(KROW(KVWCX,ICHOU)+JVWCVT)=IRNEW
 10       CONTINUE
 11     CONTINUE
C
C  Now, reset wafer strips map
C
      NDATA = LROWS(KVWSX)*LVWS1A
      CALL VZERO(IW(KVWSX+LMHLEN+1),NDATA)
C
 12   CONTINUE
      RETURN
      END
#endif
