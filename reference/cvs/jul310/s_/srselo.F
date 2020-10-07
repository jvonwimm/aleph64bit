      SUBROUTINE SRSELO(SOBJ,SOCP,SSOP,DCUT,IER)
C----------------------------------------------------------------------
C! Select objects
C!
C!    Author:     H. Meinhard       21-Mar-1987
C!    Modified:   H. Meinhard       03-Jul-1989  (5)
C!    Modified:   D.Smith    991027 Double precision fixes for Linux
C!
C!    Input:      - SOBJ      /C4   name of object bank
C!                - SOCP      /C4   name of bank pointing from objects
C!                                  to coordinates
C!                - SSOP      /C4   name of bank pointing from sides to
C!                                  objects
C!                - DCUT      /F    cut on distance measure
C!    Output:     - IER       /I    error flag: .ne. 0 if error
C!
C!    Description
C!    ===========
C!    Look for objects (track candidates or patches) lying so close
C!    together that they must come from one original object; within such
C!    a group of objects belonging together, get best one (for tracks)
C!    or mean values (for patches) for the parameters.
C?    Loop over sides
C?      Loop over pairs of objects
C?        Compute distance measure for pair of objects --> SRDTRK
C?      End of loop over pairs of objects
C?      Compute minimal spanning tree for object distances
C?      Loop over edges of MST
C?        Associate both objects to a group of objects
C?      End of loop over MST edges
C?      Loop over groups of objects
C?        Get mean value of parameters
C?      End of loop over groups of objects
C?    End of loop over sides
C?
C!======================================================================
#ifndef DOC
#include "sparam.h"
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "sgeomc.h"
#include "spatjj.h"
#include "srecpc.h"
#include "sstpjj.h"
#include "stcpjj.h"
#include "strkjj.h"
#include "sucpjj.h"
#include "swrkbk.h"
      CHARACTER*(*) SOBJ,SOCP,SSOP
      CHARACTER*8 OBOC
      DOUBLE PRECISION A,X,Y,XX,YY,XY,X1,X2,X3,Y1,Y2,Y3,ATRI,ACUT,
     +  XTIMY,COVXX,COVYY,COVXY,THM,PHM,COVTT,COVTP,COVPP,
     +  D2,D3,D6,DPI,D2PI
      PARAMETER (D2=2D0,D3=3D0,D6=6D0,
     +           DPI=3.14159274101257324219D0,D2PI=2D0*DPI)
      INTEGER MST(2,MNOSP), ISET(MNOSP), ISTRT(2), ISTOP(2)
      REAL    DM(MNOSP,MNOSP)
#include "bosext.h"
      EXTERNAL SRDTRK
C ACUT is the minimal patch area, preliminarily set by DATA statement
      DATA ACUT/2.D-9/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to objects and pointers
      KSOBJ = IW(NAMIND(SOBJ))
      KSOCP = IW(NAMIND(SOCP))
      KSSOP = IW(NAMIND(SSOP))
      IF (KSOBJ.EQ.0 .OR. KSOCP.EQ.0 .OR. KSSOP.EQ.0)       GOTO 901
      IF (SOBJ .EQ. 'SUPA') THEN
        KSPAT = IW(NASPAT)
        IF (KSPAT .EQ. 0)                                   GOTO 901
      ENDIF
      ISTRT(1) = ITABL(KSSOP,1,JSSTPB)
      ISTRT(2) = ITABL(KSSOP,2,JSSTPB)
      ISTOP(1) = ITABL(KSSOP,1,JSSTPE)
      ISTOP(2) = ITABL(KSSOP,2,JSSTPE)
C
C copy objects and pointers to work bank, reset named banks
      CALL BKTOW(IW,SOBJ,0,IW,KWK1SW,*903)
      IW(KWK1SW-3) = INTCHA(SOBJ)
      CALL BKTOW(IW,SOCP,0,IW,KWK2SW,*903)
      IW(KWK2SW-3) = INTCHA(SOCP)
      IW(KSOBJ+LMHROW) = 0
      IW(KSOCP+LMHROW) = 0
C
C top of loop over sides
      DO 410 ISIDE = 1, 2
        IW(KROW(KSSOP,ISIDE)+JSSTPB) = LROWS(KSOBJ)+1
C
C NSET is the number of object sets, NOBJ the number of objects
        NSET = 0
        ISTAR = ISTRT(ISIDE)
        NOBJ  = ISTOP(ISIDE)-ISTAR+1
C
C no object on side: no action
        IF (NOBJ.EQ.0)                                      GOTO 400
C
C one object on side: copy to object bank
        IF (NOBJ.EQ.1) THEN
          DO 300 I1 = 1, LCOLS(KSOBJ)
  300     IW(KNEXT(KSOBJ)+I1) = ITABL(KWK1SW,ISTAR,I1)
          DO 310 I2 = 1, LCOLS(KSOCP)
  310     IW(KNEXT(KSOCP)+I2) = ITABL(KWK2SW,ISTAR,I2)
          IW(KSOBJ+LMHROW) = LROWS(KSOBJ)+1
          IW(KSOCP+LMHROW) = LROWS(KSOCP)+1
          GOTO 400
        ENDIF
C
C more then one object: start selection procedure
C calculate distance measures
        IF (NOBJ.GT.MNOSP) THEN
          IER = 4
          CALL RERROR('SRSELO',IER,'Maximum number of nodes exceeded')
          GOTO 400
        ENDIF
        CALL SRDTRK(ISTAR,NOBJ,DM)
C
C calculate minimal spanning tree
        CALL SRMSTR(DM,NOBJ,MST,IMST,CST)
C
C clear buffer which contains the object groups (sets)
        CALL VZERO(ISET,MNOSP)
C
C loop over MST edges
        DO 320 IEDGE = 1, IMST
          IF (DM(MST(1,IEDGE),MST(2,IEDGE)) .LE. DCUT) THEN
C MST edge is okay, both objects associated to same set
            IF (ISET(MST(1,IEDGE)) .NE. 0) THEN
              ISET(MST(2,IEDGE)) = ISET(MST(1,IEDGE))
            ELSE IF (ISET(MST(2,IEDGE)) .NE. 0) THEN
              ISET(MST(1,IEDGE)) = ISET(MST(2,IEDGE))
            ELSE
              NSET = NSET + 1
              ISET(MST(1,IEDGE)) = NSET
              ISET(MST(2,IEDGE)) = NSET
            ENDIF
          ELSE
C MST edge is too long, objects associated to different sets
            IF (ISET(MST(1,IEDGE)) .EQ. 0) THEN
              NSET = NSET + 1
              ISET(MST(1,IEDGE)) = NSET
            ENDIF
            IF (ISET(MST(2,IEDGE)) .EQ. 0) THEN
              NSET = NSET + 1
              ISET(MST(2,IEDGE)) = NSET
            ENDIF
          ENDIF
  320   CONTINUE
C
C top of loop over sets of objects
        DO 390 JSET = 1, NSET
          NT = 0
C
C loop over objects
          NMAX = 0
          CHMIN = 1000.
          IMAX = 0
          DO 330 IOBJ = 1, NOBJ
            IF (ISET(IOBJ) .EQ. JSET) THEN
              NT = NT + 1
              JOBJ = ISTAR + IOBJ - 1
              NP = ITABL(KWK1SW,JOBJ,JSTRNP)
              C2 = RTABL(KWK1SW,JOBJ,JSTRC2)
              IF (NP .GT. NMAX .OR.
     +          (NP .EQ. NMAX .AND. C2 .LT. CHMIN)) THEN
                NMAX = NP
                CHMIN = C2
                IMAX = JOBJ
              ENDIF
            ENDIF
  330     CONTINUE
C
C only one object in set, or best track found: copy object into object
C bank
          IF (NT .EQ. 1 .OR. SOBJ .EQ. 'STRK') THEN
            DO 340 I1 = 1, LCOLS(KSOBJ)
  340       IW(KNEXT(KSOBJ)+I1) = ITABL(KWK1SW,IMAX,I1)
            DO 350 I2 = 1, LCOLS(KSOCP)
  350       IW(KNEXT(KSOCP)+I2) = ITABL(KWK2SW,IMAX,I2)
            IW(KSOBJ+LMHROW) = LROWS(KSOBJ)+1
            IW(KSOCP+LMHROW) = LROWS(KSOCP)+1
          ELSE
C
C calculate mean value of parameters for this set
            A  = 0D0
            X  = 0D0
            Y  = 0D0
            XX = 0D0
            YY = 0D0
            XY = 0D0
C
C loop over objects
            DO 360 IOBJ = 1, NOBJ
              IF (ISET(IOBJ) .EQ. JSET) THEN
                JOBJ = ISTAR + IOBJ - 1
                IPAT = ITABL(KWK2SW,JOBJ,JSUCUP)
                NC = ITABL(KSPAT,IPAT,JSPANC)
                X1 = DBLE(RTABL(KSPAT,IPAT,JSPAAX))
                Y1 = DBLE(RTABL(KSPAT,IPAT,JSPAAY))
C
C loop over constituent triangles of patch
                DO 365 ITRI = 1, NC-2
                  X2 = DBLE(RTABL(KSPAT,IPAT,JSPAAX+ITRI))
                  Y2 = DBLE(RTABL(KSPAT,IPAT,JSPAAY+ITRI))
                  X3 = DBLE(RTABL(KSPAT,IPAT,JSPAAX+ITRI+1))
                  Y3 = DBLE(RTABL(KSPAT,IPAT,JSPAAY+ITRI+1))
                  ATRI = ABS(X1*Y2+X2*Y3+X3*Y1-X1*Y3-X2*Y1-X3*Y2)/D2
                  A = A + ATRI
                  X = X + (X1+X2+X3)*ATRI/D3
                  Y = Y + (Y1+Y2+Y3)*ATRI/D3
                  XX = XX +
     +              (X1**2+X2**2+X3**2+X1*X2+X2*X3+X3*X1)*ATRI/D6
                  YY = YY +
     +              (Y1**2+Y2**2+Y3**2+Y1*Y2+Y2*Y3+Y3*Y1)*ATRI/D6
                  XY = XY + (X1*Y1+X2*Y2+X3*Y3+
     +              (X1*Y2+X2*Y3+X3*Y1+X1*Y3+X2*Y1+X3*Y2)/D2)*ATRI/D6
  365           CONTINUE
              ENDIF
  360       CONTINUE
C
C if patch set area is too small, go to next set of objects
            IF (A .LT. ACUT)                                GOTO 390
C
C get cartesian parameters of patch set
            X = X / A
            Y = Y / A
            X2 = X**2
            Y2 = Y**2
            XTIMY = X*Y
            COVXX = XX / A - X2
            COVYY = YY / A - Y2
            COVXY = XY / A - XTIMY
C
C kill patch set if covariance is zero
            IF (COVXX .LT. 1.D-10 .OR. COVYY .LT. 1.D-10)   GOTO 390
C
C convert to sperical coordinates
            THM = ATAN(SQRT(X2+Y2))
            PHM = MOD(DATAN2(Y,X)+D2PI,D2PI)
            COVTT = (COVXX*X2 + D2*COVXY*XTIMY + COVYY*Y2) / THM**2
            COVPP = (COVXX*Y2 - D2*COVXY*XTIMY + COVYY*X2) / THM**4
            COVTP = (-COVXX*XTIMY + COVXY*(X2-Y2) + COVYY*XTIMY) /
     +        THM**3
            IF (ISIDE .EQ. 2) THM = DPI - THM
C
C fill mean value in BOS bank
            IW(KNEXT(KSOBJ)+JSTRNP) = NT*2
            RW(KNEXT(KSOBJ)+JSTRC2) = 0.
            RW(KNEXT(KSOBJ)+JSTRTH) = REAL(THM)
            RW(KNEXT(KSOBJ)+JSTRPH) = REAL(PHM)
            RW(KNEXT(KSOBJ)+JSTRTT) = REAL(COVTT)
            RW(KNEXT(KSOBJ)+JSTRTP) = REAL(COVTP)
            RW(KNEXT(KSOBJ)+JSTRPP) = REAL(COVPP)
            DO 380 I2 = 1, LCOLS(KSOCP)
  380       IW(KNEXT(KSOCP)+I2) = 0
            IW(KSOBJ+LMHROW) = LROWS(KSOBJ)+1
            IW(KSOCP+LMHROW) = LROWS(KSOCP)+1
          ENDIF
C
C bottom of loop over sets of objects
  390   CONTINUE
C
C bottom of loop over sides
  400   IW(KROW(KSSOP,ISIDE)+JSSTPE) = LROWS(KSOBJ)
  410 CONTINUE
C
C compress SOBJ and SOCP banks to actual size, drop work banks
      OBOC = SOBJ//SOCP
      CALL AUBPRS(OBOC)
      CALL WDROP(IW,KWK1SW)
      CALL WDROP(IW,KWK2SW)
      CALL WGARB(IW)
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  903 IER = 3
      CALL RERROR('SRSELO',-IER,'No space for new work banks')
      CALL WDROP(IW,KWK1SW)
      CALL WDROP(IW,KWK2SW)
      CALL WGARB(IW)
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
