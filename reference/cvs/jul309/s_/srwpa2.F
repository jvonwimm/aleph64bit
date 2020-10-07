      SUBROUTINE SRWPA2(IER)
C----------------------------------------------------------------------
C! Pattern recognition for double arm fit
C!
C!    Author:     H. Meinhard       26-Feb-1990
C!    Modified:   H. Meinhard       08-Mar-1990  (1)
C!
C!    Input:      - banks 'SACO','SPAT'
C!    Output:     - IER       /I    error code
C!                                  1 = missing input banks
C!                                  2 = no space for new bank
C!                                  3 = bank overflow
C!                - bank 'SWPA'
C!
C!    Description
C!    ===========
C!    For each pair of patches, correct the coordiates of the patches
C!    for misalignment and for magnetic field, calculate the common
C!    area, and look which wire parallels match this common area
C----------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sacojj.h"
#include "saligc.h"
#include "sanamc.h"
#include "sgeomc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "srecpc.h"
#include "ssppjj.h"
#include "swpajj.h"
C MXCOR is the maximum number of patch corners
      PARAMETER (MXCOR=8)
      REAL XCOR1(2,MXCOR),XCOR2(2,MXCOR),XCOM(2,2*MXCOR),
     +  XCUT(2,2*MXCOR+4)
C MXWPA is the maximum number of sets of wire parallels
      PARAMETER (MXWPA=800)
      INTEGER IBUFF(2,MLASG),IBSUM(2),IPAT(2)
#include "bosext.h"
      DATA ICALL /0/
#include "bmacro.h"
C----------------------------------------------------------------------
C initialization: set name indices
      IF (ICALL .EQ. 0) THEN
        ZREF = ZZERSG - ZOFFSG - REAL(MLAYSG-1)*ZDELSG/2.
        ICALL = 1
      ENDIF
C link to banks needed
      JSPAT = IW(NASPAT)
      JSSPP = IW(NASSPP)
      JSACO = IW(NASACO)
      JSPCP = IW(NASPCP)
      IF (JSPAT .EQ. 0 .OR. JSSPP .EQ. 0 .OR. JSACO .EQ. 0 .OR.
     +    JSPCP .EQ. 0)                                     GOTO 901
C do nothing if we don't have patches on both sides
      IF (ITABL(JSSPP,1,JSSPPE) .LT. ITABL(JSSPP,1,JSSPPB) .OR.
     +    ITABL(JSSPP,2,JSSPPE) .LT. ITABL(JSSPP,2,JSSPPB)) GOTO 999
C create the SWPA bank
      CALL AUBOS('SWPA',0,LMHLEN+MXWPA*LSWPAA,JSWPA,IGARB)
      IF (JSWPA .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) THEN
        JSPAT = IW(NASPAT)
        JSSPP = IW(NASSPP)
        JSACO = IW(NASACO)
        JSPCP = IW(NASPCP)
      ENDIF
      IW(JSWPA+LMHCOL) = LSWPAA
C magnetic field correction
C note that FIELRC is in kGauss, not Tesla!
      DPHI = DPHBSR* ZREF
      SDP  = SIN(DPHI)
      CDP  = COS(DPHI)
C top of loop over patches on side A
      DO 430 IPAT1 = ITABL(JSSPP,1,JSSPPB), ITABL(JSSPP,1,JSSPPE)
        IPAT(1) = IPAT1
C get number of edges and edge coordinates
        NCOR1 = ITABL(JSPAT,IPAT(1),JSPANC)
        DO 300 ICOR1 = 1, NCOR1
          XCOR1(1,ICOR1) = RTABL(JSPAT,IPAT(1),JSPAAX+ICOR1-1)
          XCOR1(2,ICOR1) = RTABL(JSPAT,IPAT(1),JSPAAY+ICOR1-1)
  300   CONTINUE
C alignment correction
        IMOD = 1
        IF (XCOR1(1,1) .LT. 0.) IMOD = 2
        DO 310 ICOR1 = 1, NCOR1
          XUNC = XCOR1(1,ICOR1) * ZREF
          YUNC = XCOR1(2,ICOR1) * ZREF
          ZUNC = ZREF
          XCOR = XUNC + XSHISA(IMOD) - XYANSA(IMOD)*YUNC +
     +         ZXANSA(IMOD)*(ZUNC-ZZERSG)
          YCOR = YUNC + YSHISA(IMOD) - YZANSA(IMOD)*(ZUNC-ZZERSG) +
     +         XYANSA(IMOD)*XUNC
          ZCOR = ZUNC + ZSHISA(IMOD)-ZZERSG - ZXANSA(IMOD)*XUNC +
     +         YZANSA(IMOD)*YUNC
          XCOR1(1,ICOR1) = XCOR / ZCOR
          XCOR1(2,ICOR1) = YCOR / ZCOR
  310   CONTINUE
C magnetic field correction
        DO 320 ICOR1 = 1, NCOR1
          AXUNC = XCOR1(1,ICOR1)
          AYUNC = XCOR1(2,ICOR1)
          XCOR1(1,ICOR1) =  AXUNC * CDP + AYUNC * SDP
          XCOR1(2,ICOR1) = -AXUNC * SDP + AYUNC * CDP
  320   CONTINUE
C later on, one might implement here a correction due to vertex displa-
C cement and smearing
C top of loop over patches on side B
        DO 420 IPAT2 = ITABL(JSSPP,2,JSSPPB), ITABL(JSSPP,2,JSSPPE)
          IPAT(2) = IPAT2
C get number of edges and edge coordinates
          NCOR2 = ITABL(JSPAT,IPAT(2),JSPANC)
          DO 330 ICOR2 = 1, NCOR2
            XCOR2(1,ICOR2) = RTABL(JSPAT,IPAT(2),JSPAAX+ICOR2-1)
            XCOR2(2,ICOR2) = RTABL(JSPAT,IPAT(2),JSPAAY+ICOR2-1)
  330     CONTINUE
C alignment correction
          IMOD = 3
          IF (XCOR2(1,1) .LT. 0.) IMOD = 4
          DO 340 ICOR2 = 1, NCOR2
            XUNC = XCOR2(1,ICOR2) * ZREF
            YUNC = XCOR2(2,ICOR2) * ZREF
            ZUNC = -ZREF
            XCOR = XUNC + XSHISA(IMOD) - XYANSA(IMOD)*YUNC +
     +         ZXANSA(IMOD)*(ZUNC+ZZERSG)
            YCOR = YUNC + YSHISA(IMOD) - YZANSA(IMOD)*(ZUNC+ZZERSG) +
     +         XYANSA(IMOD)*XUNC
            ZCOR = ZUNC + ZSHISA(IMOD)+ZZERSG - ZXANSA(IMOD)*XUNC +
     +         YZANSA(IMOD)*YUNC
            XCOR2(1,ICOR2) = XCOR / ZCOR
            XCOR2(2,ICOR2) = YCOR / ZCOR
  340     CONTINUE
C magnetic field correction
          DO 350 ICOR2 = 1, NCOR2
            AXUNC = XCOR2(1,ICOR2)
            AYUNC = XCOR2(2,ICOR2)
            XCOR2(1,ICOR2) =  AXUNC * CDP - AYUNC * SDP
            XCOR2(2,ICOR2) =  AXUNC * SDP + AYUNC * CDP
  350     CONTINUE
C later on, one might implement here a correction due to vertex displa-
C cement and smearing
C now, find the common area of the two patches
          CALL SRCPAT(NCOR1,XCOR1,NCOR2,XCOR2,NCOR,XCOM)
          IF (NCOR .EQ. 0)                                  GOTO 420
C find matching wire parallels
          CALL VZERO(IBUFF,2*MLASG)
          CALL VZERO(IBSUM,2)
          IBPRO = 1
C top of loop over sides
          DO 380 ISIDE = 1, 2
C top of loop over hits of patch
            DO 370 IHIT = 1, ITABL(JSPAT,IPAT(ISIDE),JSPANW)
C get coordinate number
              ICOOR = ITABL(JSPCP,IPAT(ISIDE),IHIT)
C top of loop over wire parallels
              DO 360 ITH = 1, 2
C clip at zone around wire parallel
                KSACO = KROW(JSACO,(ICOOR-1)*2+ITH)
                RAD   = RW(KSACO+JSACRA)
                RAD1  = RAD - ZREF*DTHCSR/2.
                RAD2  = RAD + ZREF*DTHCSR/2.
                XLOT1 = RW(KSACO+JSACBF)   + RAD1*RW(KSACO+JSACBR)
                YLOT1 = RW(KSACO+JSACBF+1) + RAD1*RW(KSACO+JSACBR+1)
                ZLOT1 = RW(KSACO+JSACBF+2) + RAD1*RW(KSACO+JSACBR+2)
                XLOT2 = RW(KSACO+JSACBF)   + RAD2*RW(KSACO+JSACBR)
                YLOT2 = RW(KSACO+JSACBF+1) + RAD2*RW(KSACO+JSACBR+1)
                ZLOT2 = RW(KSACO+JSACBF+2) + RAD2*RW(KSACO+JSACBR+2)
                DELX  = RW(KSACO+JSACBW)
                DELY  = RW(KSACO+JSACBW+1)
                DELZ  = RW(KSACO+JSACBW+2)
                XCOR2(1,1) = (XLOT1-DELX)/(ZLOT1-DELZ)
                XCOR2(2,1) = (YLOT1-DELY)/(ZLOT1-DELZ)
                XCOR2(1,2) = (XLOT1+DELX)/(ZLOT1+DELZ)
                XCOR2(2,2) = (YLOT1+DELY)/(ZLOT1+DELZ)
                XCOR2(1,3) = (XLOT2+DELX)/(ZLOT2+DELZ)
                XCOR2(2,3) = (YLOT2+DELY)/(ZLOT2+DELZ)
                XCOR2(1,4) = (XLOT2-DELX)/(ZLOT2-DELZ)
                XCOR2(2,4) = (YLOT2-DELY)/(ZLOT2-DELZ)
                CALL SRCPAT(NCOR,XCOM,4,XCOR2,NCUT,XCUT)
                IF (NCUT .NE. 0) IBUFF(ISIDE,IHIT) = IBUFF(ISIDE,IHIT) +
     +            ITH
C bottom of loop over wire parallels
  360         CONTINUE
C bottom of loop over hits on side
              IB = IBUFF(ISIDE,IHIT)
              IF (IB .NE. 0) IBSUM(ISIDE) = IBSUM(ISIDE) + 1
              IF (IB .EQ. 3) IBPRO = 2*IBPRO
  370       CONTINUE
C bottom of loop over sides
  380     CONTINUE
C are there matching wire parallels? If yes, fill BOS bank
          IF (IBSUM(1) .NE. 0 .AND. IBSUM(2) .NE. 0) THEN
            DO 410 IWPA = 1, IBPRO
              IF (LFRROW(JSWPA) .LT. 2)                     GOTO 903
              NJUMP = 0
              DO 390 ISIDE = 1, 2
                KSWPA = KNEXT(JSWPA)
                IW(KSWPA+JSWPSI) = ISIDE
                IW(KSWPA+JSWPNP) = IBSUM(ISIDE)
                IW(KSWPA+JSWPMP) = IPAT(ISIDE)
                NHT = 0
                DO 400 IHIT = 1, ITABL(JSPAT,IPAT(ISIDE),JSPANW)
                  IB = IBUFF(ISIDE,IHIT)
                  IF (IB .EQ. 0)                            GOTO 400
                  NHT = NHT + 1
                  IF (IB .LT. 3) THEN
                    IW(KSWPA+JSWPMC+NHT-1) =
     +                ITABL(JSPCP,IPAT(ISIDE),IHIT)*2 + IB - 1
                  ELSE
                    IW(KSWPA+JSWPMC+NHT-1) =
     +                ITABL(JSPCP,IPAT(ISIDE),IHIT)*2 +
     +                IBITS(IWPA-1,NJUMP,1)
                    NJUMP = NJUMP + 1
                  ENDIF
  400           CONTINUE
                IW(JSWPA+LMHROW) = LROWS(JSWPA) + 1
  390         CONTINUE
  410       CONTINUE
          ENDIF
C bottom of loop over patches on side B
  420   CONTINUE
C bottom of loop over patches on side A
  430 CONTINUE
C compress 'SWPA' to actual size
      CALL AUBPRS('SWPA')
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRWPA2',-IER,'No space for new bank SWPA')
      GOTO 999
  903 IER = 3
      CALL RERROR('SRWPA2',IER,
     +  'Maximum number of sets of wire parallels exceeded')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
