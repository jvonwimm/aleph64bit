      SUBROUTINE TZCSVD(ISLOT,R,PHI,Z,ZC)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for effects from sector missalignment
C! effects in Rz. This is an effective correction, because
C! only offsets of the sector centre relative to its nominal
C! position and tilts of the sector relative to the radial direction
C! from the origin to the nominal sector centre around the
C! nominal sector centre are considered.
C!
C!  Author    :   W. Wiedenmann  91/10/15
C!  Modified  :   W. Wiedenmann  97/03/05
C!                - in case there is a valid TNLO bank, do no  
C!                  correction in z (already done via alignment in 
C!                  TALINI) 
C!
CKEY TPC Z-CORRECTION VDET
C!
C!  Input     :
C!                ISLOT /I  : sector number
C!                R     /R  : radius of TPC coordinate  [cm]
C!                PHI   /R  : angle  of TPC coordinate  [radian]
C!                Z     /R  : z of TPC coordinate [cm]
C!
C!  Output     :  ZC   /R  : corrected Z coordinate
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "evehjj.h"
#include "bcs.h"
#include "trzsjj.h"
#include "tcgdjj.h"
#include "tcrljj.h"
#include "tmtyjj.h"
#include "tpcojj.h"
#include "tphejj.h"
#include "tphtjj.h"
#include "tptejj.h"
#include "tsgmjj.h"
#include "tslojj.h"
#include "tthejj.h"
#include "tthtjj.h"
#include "tscojj.h"
#include "t1fcjj.h"
#include "t2fcjj.h"
#include "tpgpar.h"
#include "tpgeom.h"
C
C++   Correction coefficients for each sector
C
      REAL TANTHS(LTSECT), DELTZS(LTSECT)
      REAL RADSEC(LTSECT)
      LOGICAL FIRST
      INTEGER AGETDB
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
C
      ZC = Z
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTRZS=NAMIND('TRZS')
        NEVEH=NAMIND('EVEH')
        NTNLO=NAMIND('TNLO')
        IRLST=0
        KTMTY=IW(NAMIND('TMTY'))
        KTCGD=IW(NAMIND('TCGD'))
        RSTEP=RTABL(KTCGD,1,JTCGRS)
        DO 5 LS=1,LTSECT
           ISTYP = ITPTYP(LS)
           NPADR= ITABL(KTMTY,ISTYP,JTMTNP)
           RFST= RTABL(KTMTY,ISTYP,JTMTRF)
           RLST= RFST + FLOAT(NPADR-1)*RSTEP
           RADSEC(LS) = 0.5*(RFST+RLST)
5       CONTINUE
      ENDIF
C
C++   Get the current run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL ALTELL('TZCSVD: Cannot find eveh bank to get the'//
     &              ' run number.',0,'RETURN')
        IRUN=4001
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Link to the TPC z correction bank; for MC data, or if using
C++   new alignment procedure (TNLO present), just return
C
      IF (IRUN.LE.2000 .OR. IW(NTNLO).GT.0) THEN
        RETURN
      ELSE
        IF (IRLST.NE.IRUN) THEN
          IRET=AGETDB('TRZS',IRUN)
          IF (IRET.EQ.0) THEN
            CALL ALTELL('TZCSVD: '//
     &       'Bank TRZS is missing from the database.',0,'RETURN')
          ENDIF
          KTRZS=IW(NTRZS)
          IF (KTRZS.NE.0) THEN
            DO 10 LS=1,LTSECT
               DELTZS(LS) =      RTABL(KTRZS,LS,JTRZDS)
               TANTHS(LS) = TAN( RTABL(KTRZS,LS,JTRZRS) )
10          CONTINUE
          ELSE
            DO 20 LS=1,LTSECT
               DELTZS(LS) = 0.
               TANTHS(LS) = 0.
20          CONTINUE
          ENDIF
          IRLST=IRUN
        ENDIF
      ENDIF
C
C++   Transform to sector coordinate system
C
      CALL TCTSEC(ISLOT,R,PHI,Z,RS,RPHIS,ZS)
      PHIS = RPHIS/RS
      IF (PHIS.GT.PI) THEN
        PHIS=PHIS-TWOPI
      ELSEIF (PHIS.LT.-PI) THEN
        PHIS=PHIS+TWOPI
      ENDIF
C
C++   Calculate effective z-shift
C
      DZ  = (RS*COS(PHIS)-RADSEC(ISLOT))*TANTHS(ISLOT) + DELTZS(ISLOT)
      ZCS = ZS - DZ
C
C++   Transform to global coordinate system
C
      CALL TCTGLB(ISLOT,RS,RPHIS,ZCS,RG,RPHIG,ZG)
      PHIG = RPHIG/RS
      IF (PHIG.GT.TWOPI) THEN
        PHIG=PHIG-TWOPI
      ELSEIF (PHIG.LT.0.) THEN
        PHIG=PHIG+TWOPI
      ENDIF
C
      ZC = ZG
C
      RETURN
      END
#endif
