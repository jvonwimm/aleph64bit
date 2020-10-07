      SUBROUTINE SRDBLE(IER)
C----------------------------------------------------------------------
C! Try double arm fit
C!
C!    Author:     H. Meinhard       06-Feb-1990
C!    Modified:   H. Meinhard       08-Mar-1990  (3)
C!
C!    Input:      - bank 'SWPA'
C!    Output:     - IER       /I    error code:
C!                                  0 = no error
C!                                  1 = input bank(s) missing
C!                                  2 = no space for new bank
C!                                 -1 = fit unsuccessful
C!                - banks 'SFTR','STCP'
C!
C!    Description
C!    ===========
C!    For combinations of sets of wire parallels on both
C!    sides which are selected by subroutine SRWPA2, try to make a
C!    common fit using fit routine SRFIT2. If the
C!    fit fails, try to drop one coordinate, and repeat the fit unless
C!    there are less than five coordinates on one side.
C----------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "sftrjj.h"
#include "sgeomc.h"
#include "srecpc.h"
#include "stcpjj.h"
#include "swpajj.h"
C NTMAX is the maximum number of fitted track combinations
      PARAMETER (NTMAX = 100)
C IPOI: array for combinations of KPAR numbers out of NPAR numbers
C 1...NPAR; IMAX: array for combination with greatest chisquare prob.,
C NCN, KCN: arrays for coordinate numbers
      INTEGER IPOI(2*MLASG+1),IMAX(2*MLASG),NCN(2*MLASG),KCN(2*MLASG+1)
C NR is the number of fitted parameters
      PARAMETER (NR = 4)
      REAL XFIT(NR),COVX(NR,NR),XMAX(NR),COMAX(NR,NR)
      EXTERNAL PROB,PROXIM
#include "bmacro.h"
C----------------------------------------------------------------------
C link to input bank
      JSWPA = IW(NASWPA)
      IF (JSWPA .EQ. 0)                                     GOTO 901
C create bank for fitted tracks, and for pointers from tracks to
C coordinates
      CALL AUBOS('SFTR',0,LMHLEN+2*NTMAX*LSFTRA,JSFTR,IGARB)
      IF (JSFTR .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) JSWPA = IW(NASWPA)
      IW(JSFTR+LMHCOL) = LSFTRA
      CALL AUBOS('STCP',0,LMHLEN+2*NTMAX*LSTCPA,JSTCP,IGARB)
      IF (JSTCP .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) THEN
        JSWPA = IW(NASWPA)
        JSFTR = IW(NASFTR)
      ENDIF
      IW(JSTCP+LMHCOL) = LSTCPA
      IER = -1
C top of loop over wire parallel samples
      DO 400 IWPA1 = 1, LROWS(JSWPA), 2
        NPAR1 = ITABL(JSWPA,IWPA1,JSWPNP)
        IF (NPAR1 .LT. NPA2SR)                              GOTO 400
        DO 300 IP = 1, NPAR1
          NCN(IP) = ITABL(JSWPA,IWPA1,JSWPMC+IP-1) - 1
  300   CONTINUE
        IWPA2 = IWPA1 + 1
        NPAR2 = ITABL(JSWPA,IWPA2,JSWPNP)
        IF (NPAR2 .LT. NPA2SR)                              GOTO 400
        DO 310 IP = 1, NPAR2
          NCN(NPAR1+IP) = ITABL(JSWPA,IWPA2,JSWPMC+IP-1) - 1
  310   CONTINUE
        NPAR = NPAR1 + NPAR2
C we start with the full set of wire parallels
        KPAR = NPAR
C top of loop over wire parallels in fit
  320   CONTINUE
        IF (KPAR .LT. 2*NPA2SR .OR. KPAR .LT. NPAR-NDR2SR)  GOTO 400
        CHMAX = 0.
        CALL VZERO(IMAX,2*MLASG)
C top of loop over combinations of wire parallels
        IPOI(1) = 0
  330   CONTINUE
        CALL COMBI(IPOI,NPAR,KPAR)
        IF (IPOI(1) .EQ. 0)                                 GOTO 360
C enough wire parallels on both sides?
        KPAR1 = 0
        DO 340 I = 1, KPAR
          IF (IPOI(I) .LE. NPAR1) KPAR1 = KPAR1 + 1
  340   CONTINUE
        KPAR2 = KPAR - KPAR1
        IF (KPAR1 .LT. NPA2SR .OR. KPAR2 .LT. NPA2SR)       GOTO 330
C fill input vector for track fit subroutine
        CALL VZERO(KCN,2*MLASG+1)
        DO 350 I = 1, KPAR
          KCN(I) = NCN(IPOI(I))
  350   CONTINUE
C perform fit
        IFAIL = 0
        CALL SRFIT2(KCN,CHI2,XFIT,COVX,IFAIL)
C if track fit impossible, take next wire parallel sample. Otherwise,
C look for chisquare probability
        IF (IFAIL .EQ. 0) THEN
          IF (CHI2 .LE. 0.) CHI2 = 1.E-12
          CHIPR = PROB(CHI2,KPAR-NR)
          IF (CHIPR .GT. MAX(CHPRSR,CHMAX)) THEN
            CALL RVCPY(NR,XFIT(1),XFIT(2),XMAX(1),XMAX(2))
            CALL RMCPY(NR,NR,COVX(1,1),COVX(1,2),COVX(2,1),
     +        COMAX(1,1),COMAX(1,2),COMAX(2,1))
            CALL RVCPY(2*MLASG,IPOI(1),IPOI(2),IMAX(1),IMAX(2))
            CHMAX = CHIPR
            C2MAX = CHI2
          ENDIF
        ENDIF
C try next combination
        GOTO 330
C come here if there is no combination left
  360   CONTINUE
C if chisquare probability is not okay, try with one parallel less
        IF (IMAX(1) .EQ. 0) THEN
          KPAR = KPAR - 1
          GOTO 320
        ENDIF
C chisquare probability is okay. Fill candidate in bank
        IER = 0
        IF (LFRROW(JSFTR) .LT. 2) THEN
          CALL AUBOS('SFTR',0,IW(JSFTR)+2*NTMAX*LSFTRA,JSFTR,IGARB)
          IF (JSFTR .EQ. 0 .OR. IGARB .EQ. 2)               GOTO 902
          CALL AUBOS('STCP',0,IW(JSTCP)+2*NTMAX*LSTCPA,JSTCP,IGARB)
          IF (JSFTR .EQ. 0 .OR. IGARB .EQ. 2)               GOTO 902
          JSFTR = IW(NASFTR)
          JSTCP = IW(NASTCP)
          JSWPA = IW(NASWPA)
        ENDIF
        KSFTR = KNEXT(JSFTR)
        IW(KSFTR+JSFTNF) = KPAR - NR
        RW(KSFTR+JSFTC2) = C2MAX
        IW(KSFTR+JSFTQF) = 2
        RW(KSFTR+JSFTTH) = XMAX(1)
        RW(KSFTR+JSFTPH) = XMAX(2)
        RW(KSFTR+JSFTX0) = XMAX(3)
        RW(KSFTR+JSFTY0) = XMAX(4)
        N = 0
        DO 380 I = 1, NR
          DO 370 J = I, NR
            RW(KSFTR+JSFTCM+N) = COMAX(I,J)
            N = N + 1
  370     CONTINUE
  380   CONTINUE
        KSFT2 = KSFTR+LSFTRA
        IW(KSFT2+JSFTNF) = KPAR - NR
        RW(KSFT2+JSFTC2) = C2MAX
        IW(KSFT2+JSFTQF) = 2
        RW(KSFT2+JSFTTH) = PI - XMAX(1)
        RW(KSFT2+JSFTPH) = PROXIM(XMAX(2)+PI,PI)
        RW(KSFT2+JSFTX0) = XMAX(3)
        RW(KSFT2+JSFTY0) = XMAX(4)
        CALL RVCPY(10,RW(KSFTR+JSFTCM),RW(KSFTR+JSFTCM+1),
     +    RW(KSFT2+JSFTCM),RW(KSFT2+JSFTCM+1))
        IW(JSFTR+LMHROW) = LROWS(JSFTR) + 2
        KSTCP = KNEXT(JSTCP)
        KSTC2 = KSTCP + LSTCPA
        N1 = 0
        N2 = 0
        DO 390 I = 1, KPAR
          IF (IMAX(I) .LE. NPAR1) THEN
            N1 = N1 + 1
            IW(KSTCP+N1) = NCN(IMAX(I)) + 1
          ELSE
            N2 = N2 + 1
            IW(KSTC2+N2) = NCN(IMAX(I)) + 1
          ENDIF
  390   CONTINUE
        IW(JSTCP+LMHROW) = LROWS(JSTCP) + 2
C bottom of loop over wire parallel samples
  400 CONTINUE
C compress banks SFTR and STCP to actual size
      CALL AUBPRS('SFTRSTCP')
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRDBLE',-IER,'No space for new bank SFTR or STCP')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
