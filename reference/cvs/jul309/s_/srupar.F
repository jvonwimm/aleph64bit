      SUBROUTINE SRUPAR(IER)
C----------------------------------------------------------------------
C! For sides without tracks,
C! find parameters of clusters
C!
C!    Author:     H. Meinhard       29-Mar-1988
C!    Modified:   H. Meinhard       03-Jul-1989  (2)
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    For each side, look whether tracks are found; if not, get para-
C!    meters (theta, phi) of patches and their covariance matrix
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "ssppjj.h"
#include "sstpjj.h"
#include "ssupjj.h"
#include "sucpjj.h"
#include "supajj.h"
      DOUBLE PRECISION A,X,Y,XX,YY,XY,X1,Y1,X2,Y2,X3,Y3,ATRI,ACUT,
     +  XMEAN,YMEAN,XMEA2,YMEA2,XYMEA,COVXX,COVYY,COVXY,
     +  THETA,PHI,COVTT,COVPP,COVTP,
     +  D2,D3,D6,DPI,D2PI
      PARAMETER (D2=2.,D3=3.,D6=6.,DPI=PI,D2PI=TWOPI)
      INTEGER NTRK(2)
C ACUT is the minimal patch area, preliminarily set with DATA statmt
      DATA ACUT/2.D-9/
#include "bmacro.h"
C----------------------------------------------------------------------
C look whether bank SUPA exists; if yes, return
      IF (IW(NASUPA) .NE. 0)                                GOTO 999
C
C link to patches and tracks
      KSPAT = IW(NASPAT)
      KSPCP = IW(NASPCP)
      KSSPP = IW(NASSPP)
      IF (KSPAT.EQ.0 .OR. KSPCP.EQ.0 .OR. KSSPP.EQ.0)       GOTO 901
      KSSTP = IW(NASSTP)
      IF (KSSTP .NE. 0) THEN
        NTRK(1) = ITABL(KSSTP,1,JSSTPE) - ITABL(KSSTP,1,JSSTPB) + 1
        NTRK(2) = ITABL(KSSTP,2,JSSTPE) - ITABL(KSSTP,2,JSSTPB) + 1
      ELSE
        NTRK(1) = 0
        NTRK(2) = 0
      ENDIF
C
C create banks for patches, for pointers for patches to coordinates
C and for pointers from sides to patches
      CALL AUBOS('SUPA',0,LMHLEN+LROWS(KSPAT)*LSUPAA,KSUPA,IGARB)
      IF (KSUPA.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSPAT = IW(NASPAT)
        KSPCP = IW(NASPCP)
        KSSPP = IW(NASSPP)
        KSUPA = IW(NASUPA)
      ENDIF
      IW(KSUPA+LMHCOL) = LSUPAA
C
      CALL AUBOS('SUCP',0,LMHLEN+LROWS(KSPAT)*LSUCPA,KSUCP,IGARB)
      IF (KSUCP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSPAT = IW(NASPAT)
        KSPCP = IW(NASPCP)
        KSSPP = IW(NASSPP)
        KSUPA = IW(NASUPA)
        KSUCP = IW(NASUCP)
      ENDIF
      IW(KSUCP+LMHCOL) = LSUCPA
C
      CALL AUBOS('SSUP',0,LMHLEN+2*LSSUPA,KSSUP,IGARB)
      IF (KSSUP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSPAT = IW(NASPAT)
        KSPCP = IW(NASPCP)
        KSSPP = IW(NASSPP)
        KSUPA = IW(NASUPA)
        KSUCP = IW(NASUCP)
        KSSUP = IW(NASSUP)
      ENDIF
      IW(KSSUP+LMHCOL) = LSSUPA
      IW(KSSUP+LMHROW) = 2
C
C top of loop over sides
      DO 340 ISIDE = 1, 2
        IW(KROW(KSSUP,ISIDE)+JSSUPB) = LROWS(KSUPA) + 1
C
C if tracks found, no action
        IF (NTRK(ISIDE) .GT. 0)                             GOTO 330
C
C top of loop over patches on side
        DO 320 IPAT = ITABL(KSSPP,ISIDE,JSSPPB),
     +                ITABL(KSSPP,ISIDE,JSSPPE)
          A = 0.
          X = 0.
          Y = 0.
          XX = 0.
          YY = 0.
          XY = 0.
          NC = ITABL(KSPAT,IPAT,JSPANC)
          X1 = DBLE(RTABL(KSPAT,IPAT,JSPAAX))
          Y1 = DBLE(RTABL(KSPAT,IPAT,JSPAAY))
C
C loop over constituent triangles
          DO 300 ITRI = 1, NC-2
            X2 = DBLE(RTABL(KSPAT,IPAT,JSPAAX+ITRI))
            Y2 = DBLE(RTABL(KSPAT,IPAT,JSPAAY+ITRI))
            X3 = DBLE(RTABL(KSPAT,IPAT,JSPAAX+ITRI+1))
            Y3 = DBLE(RTABL(KSPAT,IPAT,JSPAAY+ITRI+1))
            ATRI = ABS(X1*Y2+X2*Y3+X3*Y1-X1*Y3-X2*Y1-X3*Y2)/D2
            A = A + ATRI
            X = X + (X1+X2+X3)*ATRI/D3
            Y = Y + (Y1+Y2+Y3)*ATRI/D3
            XX = XX + (X1**2+X2**2+X3**2+X1*X2+X2*X3+X3*X1)*ATRI/D6
            YY = YY + (Y1**2+Y2**2+Y3**2+Y1*Y2+Y2*Y3+Y3*Y1)*ATRI/D6
            XY = XY + (X1*Y1+X2*Y2+X3*Y3+
     +            (X1*Y2+X2*Y3+X3*Y1+X1*Y3+X2*Y1+X3*Y2)/D2)*ATRI/D6
  300     CONTINUE
C
C if patch area is too small, go to next patch
          IF (A .LT. ACUT)                                  GOTO 320
C
C get cartesian parameters of patch
          XMEAN = X/A
          YMEAN = Y/A
          XMEA2 = XMEAN**2
          YMEA2 = YMEAN**2
          XYMEA = XMEAN*YMEAN
          COVXX = XX/A - XMEA2
          COVYY = YY/A - YMEA2
          COVXY = XY/A - XYMEA
C  Kill patch if covariance is zero -
C  May result from 2 identical points being used in triangle,
C  if so, should really be suppressed more directly earlier
          IF(COVXX.LT.1.E-10.OR.COVYY.LT.1.E-10) GO TO 320
C
C convert to sperical coordinates
          THETA = ATAN(SQRT(XMEA2+YMEA2))
          PHI   = MOD(ATAN2(YMEAN,XMEAN)+D2PI,D2PI)
          COVTT = (COVXX*XMEA2 + D2*COVXY*XYMEA + COVYY*YMEA2)/THETA**2
          COVTP = (-COVXX*XYMEA + COVXY*(XMEA2-YMEA2) + COVYY*XYMEA) /
     +            THETA**3
          COVPP = (COVXX*YMEA2 - D2*COVXY*XYMEA + COVYY*XMEA2)/THETA**4
          IF (ISIDE .EQ. 2) THETA = DPI-THETA
C
C fill in patch bank
          NP = ITABL(KSPAT,IPAT,JSPANW)
          IW(KNEXT(KSUPA)+JSUPNP) = NP
          RW(KNEXT(KSUPA)+JSUPC2) = 0.
          RW(KNEXT(KSUPA)+JSUPTH) = REAL(THETA)
          RW(KNEXT(KSUPA)+JSUPPH) = REAL(PHI)
          RW(KNEXT(KSUPA)+JSUPTT) = REAL(COVTT)
          RW(KNEXT(KSUPA)+JSUPTP) = REAL(COVTP)
          RW(KNEXT(KSUPA)+JSUPPP) = REAL(COVPP)
          DO 310 I2 = 1, NP
  310     IW(KNEXT(KSUCP)+I2) = ITABL(KSPCP,IPAT,I2)
          IW(KNEXT(KSUCP)+JSUCUP) = IPAT
          IW(KSUPA+LMHROW) = LROWS(KSUPA)+1
          IW(KSUCP+LMHROW) = LROWS(KSUCP)+1
C
C bottom of loop over patches on side
  320   CONTINUE
C
C bottom of loop over sides
  330   IW(KROW(KSSUP,ISIDE)+JSSUPE) = LROWS(KSUPA)
  340 CONTINUE
C
C compress SUPA and SUCP banks to actual size
      CALL AUBPRS('SUPASUCP')
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRUPAR',-IER,
     +  'No space for new bank SUPA, SUCP, or SSUP')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
