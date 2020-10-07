      SUBROUTINE BMSUM
C-----------------------------------------------------------------------
C!  - Accumulate data to determine beam position
C!
C!
C!   Author   : T. Burnett
C!   Last Update: 13 Sep 1991
C!   Last Update: 16 Jun 1995
C!   Modified by S. Wasserbaech: use VDET-fitted tracks;
C!       save sums separately for tracks with and without
C!       VDET r-phi coordinates.  The A and B ends are
C!       no longer separated.
C!   Modified by P. Comas: have the same information
C!       per wagon in a train to fill the extended
C!       JSUM bank.
C!
C!   Inputs:
C!        -  JSUM : vertex info
C!        -  FRFT, FRTL : fit tracks
C!        -  VDCO : Vdet coordinates
C!
C!   Outputs:
C!        -  BSUM : accumulate info, 10 words for each category
C!                  (with and without VDET);
C!                  assume zero initially
C!        -  BSUMBU : just the same per bunch
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   Gathers information for a d0-vs-phi fit to determine the
C!   average beam position
C!
C?
C!======================================================================
#ifndef DOC
#ifndef DOC
C
#include "bsumco.h"
#include "bcs.h"
#include "rcurnt.h"
#include "frtljj.h"
#include "frftjj.h"
#include "jsumjj.h"
#include "vdcojj.h"
#include "alcons.h"
*
      REAL QQIRP,QMFLDC
      PARAMETER (QQIRP=0.00029979, QMFLDC=15.*QQIRP)
      INTEGER GTSTUP
      LOGICAL USE2, BTEST, BUNCH_OK
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C cuts: momentum, abs(costheta), abs(z0), abs(dbeam)
      DATA CUTP/2.0/,CUTTH/0.8/, CUTZ0/5./, CUTD0/0.15/
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
*
      IF (FIRST) THEN
         NFRTL=NAMIND('FRTL')
         NEVEH=NAMIND('EVEH')
         NJSUM=NAMIND('JSUM')
         FIRST=.FALSE.
      END IF
C
      USE2 = .FALSE.
      JFRFT0 = NLINK('FRFT',0)
      IF (JFRFT0 .LE. 0) GO TO 901
      JFRFT2 = NLINK('FRFT',2)
C
      JVDCO = NLINK('VDCO',0)
C
C     Should we use FRFT NR=2?
C     Use GTSTUP to check configuration.
C     If (VDET config .eq. 89 or 90) then
C       use FRFT 0
C     Else
C       if FRFT 2 exists, use FRFT 2
C       if FRFT 2 doesn't exist, use FRFT 0
C     Endif
C
      ICONF = GTSTUP ('VD',IRUNRC)
      IF (ICONF .LT. 3) THEN
        JFRFT = JFRFT0
      ELSE
        IF ((JFRFT2 .GT. 0) .AND. (JVDCO .GT. 0)) THEN
          JFRFT = JFRFT2
          USE2 = .TRUE.
        ELSE
          JFRFT = JFRFT0
        ENDIF
      ENDIF
C
      JFRTL = IW(NFRTL)
      JJSUM = IW(NJSUM)
      JEVEH = IW(NEVEH)

*  Get preliminary beam position from vertex

      NVERT = ITABL(JJSUM,1,JJSUNV)
      IF (JJSUM.EQ.0 .OR. NVERT.EQ.0) GOTO 901

      XB = RTABL(JJSUM,1,JJSUXV)/NVERT
      YB = RTABL(JJSUM,1,JJSUYV)/NVERT

*  Then go through the tracks and collect beampos info

      DO 101 ITK= 1, LROWS(JFRFT)

*     require ITC and TPC hits

        NITC = ITABL(JFRTL,ITK,JFRTNI)
        NTPC = ITABL(JFRTL,ITK,JFRTNT)
        IF (NITC.LT.4 .OR. NTPC.LT.10) GOTO 101

        CURV  = RTABL(JFRFT,ITK,JFRFIR)
        IF (ABS(CURV).LT.1.E-30) THEN
          WRITE(*,*)'Bad curvature.'
          GOTO 101
        END IF

*    Calculate momentum, cos(theta) for cuts

        PXY    = QMFLDC/ABS(CURV)
        TANL   = RTABL(JFRFT,ITK,JFRFTL)
        COSTH  = TANL/SQRT(1.+TANL**2)
        PZ     = PXY *TANL
        PTOT   = SQRT(PZ**2+PXY**2)

*       require minimum momentum and angle

        IF(PTOT.LT.CUTP)       GOTO 101
        IF(ABS(COSTH).GT. CUTTH)GOTO 101

*      Get Z0,D0, phi: use current vertex average to make cut

        Z0  = RTABL(JFRFT,ITK,JFRFZ0)
        IF (ABS(Z0).GT.CUTZ0) GOTO 101

        D0  = RTABL(JFRFT,ITK,JFRFD0)
        PHI = RTABL(JFRFT,ITK,JFRFP0)
        D0BEAM = D0 - XB*SIN(PHI) + YB*COS(PHI)
        IF (ABS(D0BEAM).GT.CUTD0) GOTO 101

*       Does the track have any VDET r-phi hits?

        NVDET = 0
        IF (USE2) THEN
          DO 50 ICO=1,LROWS(JVDCO)
            IFRFT = ITABL(JVDCO,ICO,JVDCTN)
            IF (IFRFT .NE. ITK) GO TO 50
            IQF = ITABL(JVDCO,ICO,JVDCQF)
            IF (BTEST(IQF,0)) NVDET = NVDET + 1
  50      CONTINUE
        ENDIF

*      add data to beam fit routine

        IF (NVDET .GT. 0) THEN
          ISIDE = 0
        ELSE
          ISIDE = 10
        ENDIF

        C = COS(PHI)
        S = SIN(PHI)
        BSUM(ISIDE+1)= BSUM(ISIDE+1) +  1.
        BSUM(ISIDE+2)= BSUM(ISIDE+2) +  C
        BSUM(ISIDE+3)= BSUM(ISIDE+3) +  S
        BSUM(ISIDE+4)= BSUM(ISIDE+4) +  C*S
        BSUM(ISIDE+5)= BSUM(ISIDE+5) +  S*S
        BSUM(ISIDE+6)= BSUM(ISIDE+6) +  C*C
        BSUM(ISIDE+7)= BSUM(ISIDE+7) +  D0*S
        BSUM(ISIDE+8)= BSUM(ISIDE+8) +  D0*C
        BSUM(ISIDE+9)= BSUM(ISIDE+9) +  D0
        BSUM(ISIDE+10)=BSUM(ISIDE+10)+  D0**2

C     New stuff for bunch trains
        CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
        IF ( (NWAG.LT.1).OR.(NWAG.GT.4) ) NWAG=1
        BUNCH_OK=NWAG.GT.1.AND.IBUN.GT.0.AND.IBUN.LE.NWAG.AND.IQUA.EQ.3
        IF (BUNCH_OK) THEN
          BSUMBU(ISIDE+1,IBUN)= BSUMBU(ISIDE+1,IBUN) +  1.
          BSUMBU(ISIDE+2,IBUN)= BSUMBU(ISIDE+2,IBUN) +  C
          BSUMBU(ISIDE+3,IBUN)= BSUMBU(ISIDE+3,IBUN) +  S
          BSUMBU(ISIDE+4,IBUN)= BSUMBU(ISIDE+4,IBUN) +  C*S
          BSUMBU(ISIDE+5,IBUN)= BSUMBU(ISIDE+5,IBUN) +  S*S
          BSUMBU(ISIDE+6,IBUN)= BSUMBU(ISIDE+6,IBUN) +  C*C
          BSUMBU(ISIDE+7,IBUN)= BSUMBU(ISIDE+7,IBUN) +  D0*S
          BSUMBU(ISIDE+8,IBUN)= BSUMBU(ISIDE+8,IBUN) +  D0*C
          BSUMBU(ISIDE+9,IBUN)= BSUMBU(ISIDE+9,IBUN) +  D0
          BSUMBU(ISIDE+10,IBUN)=BSUMBU(ISIDE+10,IBUN)+  D0**2
        ENDIF

 101  CONTINUE
 901  RETURN
      END
#endif
#endif
