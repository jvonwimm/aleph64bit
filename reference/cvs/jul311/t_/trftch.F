      SUBROUTINE TRFTCH
C
C----------------------------------------------------------------
C! Perform "helix" fit to coordinates of current chain
C!
C!    Author:    M. Mermikides
C!    Modified:  M. Mermikides 22-04-87
C!    Modified:  R. Johnson    19-01-88
C!    Modified:  F. Weber       8-6-89
C!    Modified:  M.Mermikides  17-08-89
C!
C!   Called by TRKFND
C!
C!    Description
C!    ===========
C!    Use internal errors on r-phi and z coordinates
C!    Add m/s error and external r-phi error for oblique tracks
C----------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tpcojj.h"
#include "tclujj.h"
#include "tlrljj.h"
#include "tpuljj.h"
#include "tcrljj.h"
#include "tchajj.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "tchain.h"
#include "tpccon.h"
#include "tpcirc.h"
#include "tplinc.h"
#include "trfdct.h"
#include "tpwork.h"
#include "tcbins.h"
#include "rconds.h"
C
      LOGICAL FIRST
C-----------------------------------------------------------------------
C   These are needed for the Fast Helix Fitting Routine:
C
      PARAMETER (MPT = 40)
      DIMENSION RCIRCL(MPT),PHCIRC(MPT),ERRMAT(15)
      DOUBLE PRECISION XFIT(MPT),YFIT(MPT),WFIT(MPT)
      DIMENSION HELPAR(5)
C
C  SCATC  = m/s constant = .0141**2 / (3. * X0), X0 = 10983 cm for Ar/CH
C  P2CUT  = Momentum squared above which we ignore m/s
C
      DATA SCATC/6.034E-9/
      DATA P2CUT/9.0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
      ICPTR(M1)= ITABL(INDBIN,M1,JNDBCO)
      KCRXY(M1,M2)  = KROW(INDCXY,IW(KROW(KTCRL,M1)+JTCROC)+M2)
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPCO=NAMIND('TPCO')
        NTCRL=NAMIND('TCRL')
      ENDIF
C
      KTPCO = IW(NTPCO)
      IF (KTPCO.EQ.0) GO TO 999
      KTCRL = IW(NTCRL)
      IF (KTCRL.EQ.0) GO TO 999
C
C  Clear dropped hit list
C
      CALL VZERO(MCHAIN,LTPDRO)
C
      NTRY = 0
C
  500 NCIRCL = 0
      NTRY = NTRY + 1
      IF (NTRY.GT.MAXTRY)  GO TO 999
      SSIGX = 0.
      SSIGZ = 0.
C
C  Transfer hits to fitter array in increasing R order
C
      DO 10 J=NHCHAI,1,-1
C
         IF (MCHAIN(J).EQ.0) THEN
            NCIRCL = NCIRCL+1
            XCIRCL(NCIRCL) = RTABL(INDCXY,ICPTR(IHCHAI(J)),1)
            YCIRCL(NCIRCL) = RTABL(INDCXY,ICPTR(IHCHAI(J)),2)
            ZCIRCL(NCIRCL) = RTABL(KTPCO,ICPTR(IHCHAI(J)),JTPCZV)
            RCIRCL(NCIRCL) = RTABL(KTPCO,ICPTR(IHCHAI(J)),JTPCRV)
            PHCIRC(NCIRCL) = RTABL(KTPCO,ICPTR(IHCHAI(J)),JTPCPH)
            IF (NCIRCL.EQ.1) THEN
               Z1 = ZCIRCL(NCIRCL)
               IR1 = IRCHAI(J)
            ENDIF
C
            SIGRP = D2CHAI(J)
            SIGZ =  RTABL(KTPCO,ICPTR(IHCHAI(J)),JTPCSZ)
            IR2 = IRCHAI(J)
            Z2 = ZCIRCL(NCIRCL)
            WCIRCL(NCIRCL) = 1./SIGRP
            WLINEA(NCIRCL) = 1./SIGZ
            SSIGX = SSIGX + SIGRP
            SSIGZ = SSIGZ + SIGZ
         ENDIF
   10 CONTINUE
C
C  Fit if more than 3 hits
C
      IF (NCIRCL.LE.3)  GO TO 999
C
C  Mean sigma**2
C
      SSIGX = SSIGX/FLOAT(NCIRCL)
      SSIGZ = SSIGZ/FLOAT(NCIRCL)
C
C-----------------------------------------------------------------------
C   Call the modified Fast Helix Fitting Routine                       !
C                                                                      !
C  least sqrs fit w/error mat calc.
   25 IOPT = 2
      DO 26 I = 1,NCIRCL
          XFIT(I) = XCIRCL(I)
          YFIT(I) = YCIRCL(I)
          WFIT(I) = WCIRCL(I)
   26 CONTINUE
      CALL TFITHL(NCIRCL,XFIT,YFIT,RCIRCL,PHCIRC,WFIT,
     1            ZCIRCL,WLINEA,IOPT,
     2            HELPAR,ERRMAT,CHIRP,CHISZ)
C
C  Convert the CHI2 values to the ones this routine expects, calculate C
C  as a check vs. TFITHL failure
C
      CHITOT = CHIRP + CHISZ
      CHIRP=CHIRP/FLOAT(NCIRCL-3)
      CHISZ=CHISZ/FLOAT(NCIRCL-2)
C
      IF (CHITOT.GT.5000.0) THEN
C
C   TFITHL failed.  Borrowing code from L. Garrido's UFITMS
C   routine, I replace any zero diagonal elements of ERRMAT with the
C   average errors on a 3 GeV particle.  This assumes that the other
C   elements of the inverted ERRMAT are o.k.:
C
C     conversion radius of track <=> momentum
C     radius in meter , B in Tesla, p in GeV/c  q in units of e
C
C      p = 0.29979 * q * B * r
C
C     R[cm] = ROFP * P[Gev/c]:
C
         ROFP=1./(0.29979*FIELRC/10.)*100.
         IF (ERRMAT(1).EQ.0.0) ERRMAT(1)=1.0/(.005/ROFP)**2
         IF (ERRMAT(3).EQ.0.0) ERRMAT(3)=1.0/.002**2
         IF (ERRMAT(6).EQ.0.0) ERRMAT(6)=1.0/.003**2
         IF (ERRMAT(10).EQ.0.0) ERRMAT(10)=1.0/.007**2
         IF (ERRMAT(15).EQ.0.0) ERRMAT(15)=1.0/.007**2
      END IF
C
C   HELPAR contains the ALEPH-standard helix parameters; must be loaded
C   into internal variables
C
      SGN = SIGN(1.,HELPAR(1))
      RHO = ABS(1./HELPAR(1))
      TANL = HELPAR(2)
      PHI0 = HELPAR(3)
      D0 = HELPAR(4)
      Z0 = HELPAR(5)
      X0 = ABS(D0)*COS(PHI0)
      Y0 = ABS(D0)*SIN(PHI0)
C
C   Now compute SCIRCL values
C
      PHIC = PHI0 - PIBY2
      IF(SGN*D0.GT.0.0)   PHIC = PHI0 + PIBY2
      X0 = ABS(D0)*COS(PHIC)
      Y0 = ABS(D0)*SIN(PHIC)
C
C Find S-values measured from the point (X0,Y0); intercept
C in s-z plane then gives Z0. (Assume hits are ordered in S)
C
      SCIRCL(1)=0.
      KCIR=NCIRCL
      RR = RHO
      DPSI=SQRT((XCIRCL(1)-X0)**2
     1            +(YCIRCL(1)-Y0)**2)/(2.*RR)
      SCIRCL(1)= 2.*RR*ASIN(DPSI)
      DO 137 KS=2,KCIR
         DPSI=SQRT((XCIRCL(KS)-XCIRCL(KS-1))**2
     1            +(YCIRCL(KS)-YCIRCL(KS-1))**2)/(2.*RR)
C
C Check if we turn through pi
C
         IF (DPSI.GT.1.0) THEN
            NCIRCL=KS
            GO TO 138
         ENDIF
         SCIRCL(KS)=SCIRCL(KS-1) + 2.*RR*ASIN(DPSI)
  137 CONTINUE
  138 CONTINUE
C                                                                      !
C-----------------------------------------------------------------------
C
C  Take m/s into account in chisq. For now we just rescale
C  by average error.
C
      SECL = SQRT(1. + TANL*TANL)
      PP = RHO*BCFGEV*SECL
      P2 = PP*PP
C
C Skip track with p > P2CUT
C
      IF (P2.GT.0.0 .AND.P2.LT.P2CUT) THEN
         DX = XCIRCL(1) - XCIRCL(NCIRCL)
         DY = YCIRCL(2) - YCIRCL(NCIRCL)
C
C Approx. arc by chord
C
         SS = SQRT(DX*DX + DY*DY)*SECL/4.
         SIGMS = SCATC*SS*SS*SS/P2
         SCALX = SSIGX/(SIGMS +SSIGX)
         SCALZ = SSIGZ/(SIGMS +SSIGZ)
      ELSE
         SCALX = 1.
         SCALZ = 1.
      ENDIF
C
C  In case of failed circle fit, reject hit with worst pull
C
      IF (CHIRP*SCALX.GT.CHLIM1) THEN
         PULMX = 0.
         CURV = - SGN/(RHO-D0)
         COSP0 = COS(PHI0)
         SINP0 = SIN(PHI0)
C
C  Compute residuals
C
         DO 20 J = 1,NCIRCL
            TERM = XCIRCL(J)*XCIRCL(J) + YCIRCL(J)*YCIRCL(J) - D0*D0
            DXY = 0.5*CURV*TERM + XCIRCL(J)*SINP0
     &           -YCIRCL(J)*COSP0 + SGN*D0
            PULL = ABS(DXY)*SQRT(WCIRCL(J))
            IF (PULL.GT.PULMX) THEN
               PULMX = PULL
               JMAX = J
            ENDIF
C            PRINT 556, XCIRCL(J),YCIRCL(J),ZCIRCL(J),DXY,PULL
C 556         FORMAT ( 2(5X,3F10.3,2X,G10.3,2F7.2) )
   20    CONTINUE
C
         IF (FDEBRF .AND. JDBDRF(JULTP).GE.9) THEN
            WRITE(LDEBRL,558) JMAX,PULMX,CHIRP
         ENDIF
  558    FORMAT (' +++TRFTCH+++  R-phi fit, reject hit ',I4,
     &           ' Max. Dev.,CHISQ =',F10.3,E10.3)
         MCHAIN(NHCHAI + 1 - JMAX) = 1
         GO TO 500
C
      ENDIF
C
C  Do the same in case of bad s-z fit
C
      IF (CHISZ*SCALZ.GT.CHLIM2) THEN
         PULMX = 0.
         DO 22 J = 1,NCIRCL
            ZZ = Z0 + SCIRCL(J)*TANL
            DZ = ZCIRCL(J) - ZZ
            PULL = ABS(DZ)*SQRT(WLINEA(J))
            IF (PULL.GT.PULMX) THEN
               PULMX = PULL
               JMAX = J
            ENDIF
   22    CONTINUE
         IF (FDEBRF .AND. JDBDRF(JULTP).GE.9) THEN
             WRITE(LDEBRL,559) JMAX,PULMX,CHISZ
         ENDIF
  559    FORMAT (' +++TRFTCH+++ S-Z fit, reject hit ',I4,
     *           ' Max. Dev.,CHISQ = ',F10.3,E10.3)
         MCHAIN(NHCHAI + 1 - JMAX) = 1
         GO TO 500
      ENDIF
C
C Update chain parameters
C
      PCHAIN(1) = RHO*SGN
      PCHAIN(JTCHTL) = TANL
      PCHAIN(JTCHP0) = PHI0
      PCHAIN(JTCHD0) = D0
      PCHAIN(JTCHZ0) = Z0
      PCHAIN(JTCHLC) = SCIRCL(NCIRCL) - SCIRCL(1)
      PCHAIN(JTCHC1) = CHIRP
      PCHAIN(JTCHC2) = CHISZ
      ISTCHA = 10000*IR1 + 100*IR2 + NCIRCL
      CALL UCOPY (ERRMAT(1),PCHAIN(JTCHER),15)
C
  999 RETURN
      END
#endif
