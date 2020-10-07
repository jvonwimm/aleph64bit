      SUBROUTINE TRPROJ
C
C---------------------------------------------------------------*
C! Projects chain to find more hits                             *
C!                                                              *
C!   Author:     M. Mermikides    06-10-86                      *
C!   Modified:   R. Johnson       19-01-88                      *
C!   Modified:   M. Mermikides    19-08-89
C!                                                              *
C!   Called by TRKFND                                           *
C!                                                              *
C!  Road width is Min(TDZSTR,TDZMAX)*Min(TDWSTR,TDWMAX)         *
C!  The chain is extended iteratively until there are no more   *
C!  hits that can be added.                                     *
C---------------------------------------------------------------*
#ifndef DOC
C
#include "rparac.h"
#include "alcons.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tchajj.h"
#include "tpcojj.h"
#include "tclujj.h"
#include "tlrljj.h"
#include "tpuljj.h"
#include "tcrljj.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "tchain.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "trfdct.h"
#include "tcbins.h"
C
C  JROAD holds hits for each semicircle
C
      DIMENSION JROAD(21),IRSAV(21),IHSAV(21),PSAVE(23)
      DIMENSION D2SAV(21),MHSAV(21),SROAD(21)
      DIMENSION V(4,2)
C
      LOGICAL TURN,FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPCO=NAMIND('TPCO')
        NTCRL=NAMIND('TCRL')
      ENDIF
C
      IF (ISTCHA.EQ.0) GO TO 999
C
C   Skip if we already have full track
C
  200 IF(NHCHAI.EQ.NTPROW) GO TO 999
C
      KTPCO = IW(NTPCO)
      IF (KTPCO.EQ.0) GO TO 999
      KTCRL = IW(NTCRL)
      IF (KTCRL.EQ.0) GO TO 999
C
      RHO = ABS(PCHAIN(1))
      Q = SIGN(1.,PCHAIN(1))
      TANL = PCHAIN(JTCHTL)
      PHI0 = PCHAIN(JTCHP0)
      D0 = PCHAIN(JTCHD0)
      DMAX = 2.*RHO - D0
      TURN = .FALSE.
      IF(DMAX.LE.RTPCMX) TURN=.TRUE.
C
C  Get first and last padrow to be looked at.  For straight-through trac
C  we consider range covered by extending present chain length in both
C  directions
C
      IF(.NOT.TURN) THEN
         JSPAN = IRCHAI(1)-IRCHAI(NHCHAI)+1
         IF (JSPAN.GE.8) THEN
            IR1 = 1
            IR2 = NTPROW
         ELSE
            IR1 = MAX(IRCHAI(NHCHAI)-JSPAN, 1)
            IR2 = MIN(IRCHAI(1) + JSPAN, NTPROW)
         ENDIF
C
C  Case of spiral. Go up to furthest point of approach.
C
      ELSE
         DMIN = ABS(D0)
         IR1 = INT((DMIN-TPPROW(1))/TPDRST(1)) + 1
         IR1 = MAX(IR1,1)
         IR2 = INT((DMAX-TPPROW(1))/TPDRST(1)) + 1
         IR2 = MIN(IR2,NTPROW)
C
C  Return if we have all hits
C
         IF (NHCHAI.GT.IR2-IR1+1) GO TO 999
      ENDIF
C
C  Save initial chain in case fit fails
C
      NSAVE = NHCHAI
      CALL UCOPY(IRCHAI(1),IRSAV,NHCHAI)
      CALL UCOPY(IHCHAI(1),IHSAV,NHCHAI)
      CALL UCOPY(MCHAIN(1),MHSAV,NHCHAI)
      CALL UCOPY(D2CHAI(1),D2SAV,NHCHAI)
      CALL UCOPY(PCHAIN(1),PSAVE,NPARCH)
      ISTSV = ISTCHA
C
C  Get z road width allowing for track inclination
C
      DELZ = TDZSTR*SQRT(1. + TANL*TANL)
      IF(DELZ.GT.TDZMAX)    DELZ=TDZMAX
      PHIC = PHI0 - Q*PIBY2
      RC = RHO - D0
      XC = RC*COS(PHIC)
      YC = RC*SIN(PHIC)
C
C  Get hits in initial chain
C
      CALL VFILL(JROAD,21,-1)
      DO 40 M=1,NHCHAI
         JROAD(IRCHAI(M))=IHCHAI(M)
         SROAD(IRCHAI(M))=D2CHAI(M)
 40   CONTINUE
      NXTR1 = 0
C
C   Look at unused padrows for more hits
C
      DO 20 K = IR1,IR2
C
         NCOR = ITABL(KTCRL,K,JTCRNC)
         IF (NCOR.LE.0) GO TO 20
C
C  Don't look at padrow where we already have hit
C
         IF (JROAD(K).NE.-1)  GO TO 20
C
C   Find intersection of orbit with padrow K
C
         RAD = TPPROW(K)
         CALL TRHLX2(RAD,PCHAIN(1),V,IERR)
         IF (IERR.NE.0) GO TO 20
         ZP1 = V(3,1)
C
C   Skip padrow if intersection is unphysical or outside in Z
C
         IF (ABS(ZP1).GT.ZTPCMX+DELZ) GO TO 20
C
C   Get angle-dependent phi tolerance
C
         PHI1 = V(4,1)
         BETA = ATAN2(-(V(1,1)-XC), V(2,1)-YC )
         IF (BETA.LT.0.) BETA=BETA+TWOPI
         BETA = TRPHDI(PHI1,BETA)
         COSB = COS(BETA)
         SINB = SIN(BETA)
         DELW = TDWSTR/ABS(COSB)
         IF (DELW.GT.TDWMAX)  DELW=TDWMAX
         NFND1 = 0
C
C   Find the appropriate phi bin, and loop over all hits in it
C
         IBIN= IXWRAP(INT(TCOFFS+PHI1/TC2DEL))
         IPTR= IBNPTR(IBIN,K)
C
C   Search for hits over 1st semicircle
C
  814    IF (IPTR.EQ.0) GO TO 815
            IC= ITABL(INDBIN,IPTR,JNDBCO)
            JJ = KROW(KTPCO,IC)
            PHI = RW(JJ + JTPCPH)
            Z   = RW(JJ + JTPCZV)
            DPHI = TRPHDI(PHI1,PHI)
            DW = ABS(RAD*DPHI)
            IF (DW.GT.DELW) THEN
              IPTR=ITABL(INDBIN,IPTR,JNDBFW)
              GO TO 814
            ENDIF
            DZ = ABS(ZP1-Z)
            IF (DZ.GT.DELZ) THEN
              IPTR=ITABL(INDBIN,IPTR,JNDBFW)
              GO TO 814
            ENDIF
            NFND1 = NFND1 + 1
            IF (NFND1.GT.1) GO TO 815
            ISAV1 = IPTR
            SAGER2=RW(JJ+JTPCSR)*COSB*COSB + 0.1875*SINB*SINB
C
            IPTR=ITABL(INDBIN,IPTR,JNDBFW)
            GO TO 814
  815    CONTINUE
C
C  Accept only 1 candidate in each semicircle
C
         IF (NFND1.EQ.1) THEN
            NXTR1 = NXTR1 + 1
            JROAD(K) = ISAV1
            SROAD(K) = SAGER2
         ENDIF
C
 20   CONTINUE
C
       IF (FDEBRF .AND. JDBDRF(JULTP).GE.9 .AND. NXTR1.GT.0) THEN
          WRITE(LDEBRL,556) NXTR1
  556     FORMAT(' +++TRPROJ+++ ',I3,' more hits')
          WRITE(LDEBRL,557) (JROAD(M),M=1,21)
  557     FORMAT(/(2X,21I5))
      ENDIF
C
      IF (NXTR1.GT.0) THEN
         NEW = 0
         DO 50 M = NTPROW,1,-1
            IF (JROAD(M).LT.0) GO TO 50
            NEW = NEW + 1
            IHCHAI(NEW) = JROAD(M)
            IRCHAI(NEW) = M
            D2CHAI(NEW) = SROAD(M)
   50    CONTINUE
         NHCHAI = NEW
C
C  Refit
C
         ISTCHA = 0
         CALL TRFTCH
C
C  If fit is unsuccessful, restore original chain
C
         IF (ISTCHA.EQ.0) THEN
            CALL UCOPY(IRSAV,IRCHAI(1),NSAVE)
            CALL UCOPY(IHSAV,IHCHAI(1),NSAVE)
            CALL UCOPY(MHSAV,MCHAIN(1),NSAVE)
            CALL UCOPY(D2SAV,D2CHAI(1),NSAVE)
            CALL UCOPY(PSAVE,PCHAIN(1),NPARCH)
            ISTCHA = ISTSV
            NHCHAI=NSAVE
            IF (FDEBRF .AND. JDBDRF(JULTP).GE.9) THEN
               WRITE(LDEBRL,558) NEW
            ENDIF
  558       FORMAT(' +++TRPROJ+++ Fit of TRPROJ. chain  nextr =',
     &             I3,' failed')
            NXTR1 = 0
         ENDIF
      ENDIF
C
C  If extra hits have been found, project again until full length is
C  covered.  Skip curlers.
C
      IF (TURN) GO TO 999
      IF (ISTSV.EQ.ISTCHA) GO TO 999
C
      IF(JSPAN.LT.8)  GO TO 200
C
  999 RETURN
      END
#endif
