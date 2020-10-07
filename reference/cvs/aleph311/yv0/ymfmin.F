      SUBROUTINE YMFMIN(NFRPI,NFRNJ,IV0NU,VERT,VERM,PV0,PV1,PV2,
     &                  PVMAT,CPV,PX,PXMS,CHI2,NDF,CHIT,PSII,PSIJ,
     &                  IERCO)
C----------------------------------------------------------------
C! V0 QUANTITIES FOR MINI
CKEY YV0 MINI
C
C   AUTHOR: M.A.CIOCCI 28/1/93
C
C   NEED : FRFT , FRTL, PYER (OR JSUM),YV1C (DATA BASE OR
C           CARDS)
C
C
C     CALLED: MINYV0
C
C            DESCRIPTION
C            ===========
C   Given two tracks (the julia numbers in the FRFT bank)
C   performing a V0 candidate fill all the quantities
C   needs to reconstruct V0 bank.
C
C
C
C       INPUT:
C                NFRPI/integer         positive julia frft number
C                NFRNJ/integer         negative julia frft number
C                IV0NU/integer         number of current v0 candidate
C       OUTPUT:
C
C                VERT/R         Coordinates of the V0 vertex
C                VERM/R         Covariance matrix of the vertex
C                PV0 /R         V0 momentum
C                PV1/R          Refitted momenta of positive charged
C                               particle
C                PV2/R          Refitted momenta of negative charged
C                PVMAT/D        Covariance matrix of V0 momentum
C                               particle
C                CPV/D          Covariance matrix of pv1 pv2
C                PX/R           Mass constaints (r,b of aleph note...)
C                PXMS/R         Covariance matrix of the mass constraint
C                CHI2/R         Chi square of the v0 vertex fit
C                NDF/R          (FIT HYPOTHESIS*10+ABS(IND))*SIGN
C                               IND IN RANGE -2 TO 2 (SEE YV0
C                               PACKAGE WRITE-UP). IND=-2 OR -1
C                               IF TRACKS DO NOT INTERSECT IN XY.
C                               IND=0 - INTERSECT AT ONE POINT.
C                               IND=1 OR 2 - INTERSECT TWICE.
C                               SIGN IS POSITIVE IF IND IS
C                               POSITIVE. FIT HYP.=1 IF TRACKS
C                               NOT PARALLEL AT VERTEX (IE. IND=1
C                               OR DIFF. IN TAN(DIP) NOT SMALL).
C                               FIT HYP=2 IF TRACKS APPROX
C                               PARALLEL AT VERTEX (IE. IND.NE.1
C                               AND DIFF.IN TAN(DIP) IS SMALL).
C                CHIT/R         Minimum of distance on space
C                               between helices
C                PSII/R         Psi angle for the positive charged
C                               particle coming from V0
C                               between helices
C                PSIJ/R         Psi angle for the negative charged
C                               particle coming from V0
C                IERCO/INTEGER         code for error:
C                                      if 0 all is ok
C                                      if 1 no yv1c on dbase or cards
C                                      if 2, problems with covariance
C                                      matrix of charged tracks in ynv0ve
C                                      if 3, problems with coovariance
C                                      matrix of charged tracks in ynvonf
C                                      if 4, the number of V0 candidates
C                                      is greater than 50
C                                      IF 5 THE FRFT BANK IS MISSING
C                                      IF 6 LESS THAN TWO TRACKS IN FRFT
C------------------------------------------------------
#ifndef DOC
      SAVE
C
      EXTERNAL ALFIEL
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "pyerjj.h"
#include "jsumjj.h"
#include "yv1cjj.h"
#include "yv0vjj.h"
      COMMON/YV0CUT/IYV0IV,YV0CO1,YV0CO2,YV0CCT,YV0CPS,YV0CRV,YV0CZV,
     $              YV0DIP,YV0CC2,YV0CCS,YV0CC0,YV0CMA,YV0CPM,IYV0NT,
     $              IYV0CB,IYV0NC,YV0CMD,YV0CDZ,YV0CZZ,YV0CKW,YV0CLW,
     $              YV0CGW
CIF DOC
C
C     This common contains the cuts for V0 finding. It is filled
C     in YV0INI from the direct acces  bank 'YV0C'
C     IYV0IV  = Flag for take reconstructed vertex from JSUM bank
C               (IYV0IV=2),or from PYER bank (IYV0IV=1)
C                or vertex in (0.,0.,0.) (IYV0IV=0)
C
C     YV0CO1  = Min value of chi square increase constraining only
C                                     one track to the main vertex
C     YV0CO2  = Min value of chi square increase constraining both
C                                        tracks to the main vertex
C     YV0CCT  = Max value of distance between
C               starting points of fit
C     YV0CPS  = Minimum value for the psi angle
C     YV0CRV  = Max value of the V0 vertex radius
C     YV0CZV  = Max value of the V0 vertex abs(Z)
C     YV0DIP  = Max value of difference between tg of dip
C               (for test on parallelism between two tracks)
C     YV0CC2  = Max value of the V0 fit chi square
C     YV0CCS  = Minimum value of the cosinus of the angle between
C                                       V0 vertex and V0 momentum
C     YV0CC0  = Minimum value of the chi square increase constraining
C                                     V0 vertex to the primary vertex
C     YV0CMA  = Maximum value of the chi square of the mass constraint
C     YV0CPM  = Minimum value of daughter track momenta
C     IYV0NT  = Minimum no. of TPC hits on each daughter track
C     IYV0CB  = Flag for testing on coords before vx.
C                =0 no test; =1 at least one track to have less than NC
C                coords before vx; =2 both tracks to have less than NC
C                coords before vx.
C     IYV0NC  = Max. no. of coords. before vx.
C     YV0CMD  = Minimum distance of vx from primary vx (cm)
C     YV0CDZ  = Maximum d0 of V0
C     YV0CZZ  = Maximum z0 of V0
C     YV0CKW  = Keep candidate if pi-pi mass within YV0CKW GeV of k0
C     YV0CLW  =  "       "      "  p-pi  "     "    YV0CLW GeV of lambda
C     YV0CGW  =  "       "      "  e-e   "     "    YV0CGW GeV of zero
C
C---------------------------------------------------------------------
CEI
      INTEGER  MAXNV0
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
C
C+   INPUT DIMENSION
C
       INTEGER NGAPII,NGANJJ,IV0NU
C
C+  OUTPUT DIMENSION
C
       INTEGER NDF,IERCO

       REAL VERT(3),VERM(3,3),PV0(3),PV1(3),PV2(3),PX(2),PXMS(2,2),CHI2,
     &     CHIT,PSII,PSIJ
       DOUBLE PRECISION PVMAT(3,3),CPV(6,6)
C
C+  END OUTPUT DIMENSION
C
      LOGICAL FIRST
      INTEGER INDE
      REAL COORI(3),COORJ(3)
      REAL FIELM
      REAL VPRIM(3),SIVPR(3,3)
      DOUBLE PRECISION VVMAT(3,3),PXM(2,2)
      DATA MAXNV0/50/
      DATA FIRST/.TRUE./
      INTEGER NFRFT, NFRTL, NPYER
#include "bmacro.h"
C
C+     STARTING VALUE FOR ERROR CODE
C
            IERCO=0
C
C+ Field
C
      FIELM = ALFIEL(DUM)
      FIELM = ABS(FIELM)
C
C
C+        READ YV1C BANK AND FILL YV0CUT BANK
C+        YV1C CONTAINS THE STANDARD CUTS FOR V0 RECONSTRUCTION
C+        TO CHANGE CUTS -> CHANGE YV1C BANK
C
             IF(FIRST)THEN
C
      NFRFT = NAMIND( 'FRFT' )
      NFRTL = NAMIND( 'FRTL' )
C
C+    DEFINE LOGICAL UNIT FOR DATA BASE
C
               LDB=JUNIDB(IDUM)
               CALL YV1INI(LDB,IFLAG,NRYV1)
                 IF(IFLAG.NE.0)THEN
                     IERCO=1
                     GO TO 80
                 ENDIF
              FIRST=.FALSE.
             ENDIF
C  GET THE PRIMARY VERTEX COORDINATES
        IF(IYV0IV.EQ.0)GO TO 18
        IF(IYV0IV.EQ.1)GO TO 16
C---------------------------------------------------------------
C
C   HERE WE USE THE MEAN VALUE FOR PRIMARY VERTEX
C
C---------------------------------------------------------------
          KJSUM=NLINK('JSUM',IRUN)
           IF(KJSUM.LE.0) GO TO 16
           CALL YVPRIM(KJSUM,VPRIM,SIVPR)
           GO TO 19
  16    CONTINUE
C
C+     HERE WE USE THE PRIMARY VERTEX RECONSTRUCTED IN THE EVENT:
C+     PYER BANK
C
        KYVXL=NLINK('PYER',0)
        IF (KYVXL.EQ.0) GOTO 18
        NVERT=LROWS(KYVXL)
        IF(NVERT.EQ.0)GO TO 18
        DO 15 IVERT=1,NVERT
        IF (ITABL(KYVXL,IVERT,JPYETY).NE.1) GOTO 15
        VPRIM(1)=RTABL(KYVXL,IVERT,JPYEVX)
        VPRIM(2)=RTABL(KYVXL,IVERT,JPYEVY)
        VPRIM(3)=RTABL(KYVXL,IVERT,JPYEVZ)
        SIVPR(1,1)=RTABL(KYVXL,IVERT,JPYEVM)
        SIVPR(1,2)=RTABL(KYVXL,IVERT,JPYEVM+1)
        SIVPR(1,3)=RTABL(KYVXL,IVERT,JPYEVM+3)
        SIVPR(2,1)=RTABL(KYVXL,IVERT,JPYEVM+1)
        SIVPR(2,2)=RTABL(KYVXL,IVERT,JPYEVM+2)
        SIVPR(2,3)=RTABL(KYVXL,IVERT,JPYEVM+4)
        SIVPR(3,1)=RTABL(KYVXL,IVERT,JPYEVM+3)
        SIVPR(3,2)=RTABL(KYVXL,IVERT,JPYEVM+4)
        SIVPR(3,3)=RTABL(KYVXL,IVERT,JPYEVM+5)
        GOTO 19
   15   CONTINUE
   18   CONTINUE
C---------------------------------------
C   HERE: YV0CIV=0
C                OR
C   PYER AND JSUM NOT PRESENT
C--------------------------------------
        VPRIM(1)=0.
        VPRIM(2)=0.
        VPRIM(3)=0.
        DO I=1,3
        DO J=1,3
              SIVPR(I,J) = 0.
        ENDDO
        ENDDO
   19   CONTINUE
C
C+       READ THE FRFT BANK
C+       KFRFT, 0 POINTER AT THE FRFT BANK
C+       KFRTL, 0 POINTER AT THE FRFT BANK
C
        KFRFT = IW( NFRFT )
        KFRTL = IW( NFRTL )
C
C  IF NO TRACK BANK WE SKIP THE V0 RECONSTRUCTION !!!
C
        IF (KFRFT.EQ.0) then
            ierco=5
            GOTO 80
        endif
C
C+      NTRFR,NUMBER OF TRACKS IN FRFT BANK
C
        NTRFR=LROWS(KFRFT)
        IF (NTRFR.LT.2) then
            ierco=6
            GOTO 80
        endif
C
C+ WE LOOK NOW A "SELECTED" PAIRS
C+      NFRPI, NUMBER OF THE POSITIVE TRACK IN THE FRFT BANK
C+            FOR ANY TWO TRACKS,I AND J,FINDS
C+            THE RESPECTIVES PSI ANGLES (PSII,PSIJ),AND THE
C+            COORDINATES AT THE DISTANCE OF MINIMUM APPROACH ON
C+            THE X Y PLANE
C
              CALL  YFPSIN(NFRPI,NFRNJ,VPRIM,
     $  PSII,PSIJ,COORI,COORJ,INDE,CHIT)
C
C+      IF INDE=6 THE TWO CIRCLES IN X-Y HAVE THE SAME CENTER...
C+
C
        IF(INDE.GE.6)GO TO 80
C
C
        DELDI=ABS(RTABL(KFRFT,NFRPI,JFRFTL)-RTABL(KFRFT,NFRNJ,JFRFTL))
                IF(INDE.EQ.1.OR.DELDI.GT.YV0DIP)THEN
C
C+     First fit performance , TRACKS NOT PARALLEL
C+     DIFFERENCE BETWEEN DIP ANGLE.GT.YV0DIP
C
        IFAIL=0
        CALL YNV0VE(NFRPI,NFRNJ,PSII,PSIJ,PV0,PVMAT,PV1,
     $  PV2,CPV,VERT,VVMAT,PX,PXM,CHI2,IFAIL)
              IF (IFAIL.NE.0)THEN
                IERCO=2
                GO TO 80
               ENDIF
        NDF=SIGN(1.,1.*INDE)*(ABS(INDE*1.)+10.)
C+
C+  HERE WE MAKE THE TEST ON THE VERTEX WE HAVE FOUND
C+
C
                ENDIF
C
C+     Second  fit performance , TRACKS  PARALLEL IN XY PLANE
C+                         AND
C+     DIFFERENCE BETWEEN DIP ANGLE.LT.YV0DIP
C
             IF(INDE.NE.1.AND.DELDI.LE.YV0DIP)THEN
        IFAIL=0
        CALL YNV0NF(NFRPI,NFRNJ,PSII,PSIJ,PV0,PVMAT,PV1,
     $   PV2,CPV,VERT,VVMAT,PX,PXM,CHI2,IFAIL)
              IF (IFAIL.NE.0)THEN
                IERCO=3
                GOTO 80
              ENDIF
        NDF=SIGN(1.,1.*INDE)*(ABS(INDE*1.)+20.)
C
             ENDIF
C
C       coovariance matrix of reconstructed v0 vertex candidate
C
        DO 100 LS=1,3
        DO 110 LJ=1,3
        VERM(LS,LJ)=VVMAT(LS,LJ)
  110   CONTINUE
  100   CONTINUE
C
C+ PXMS(2,2) IS IN SINGLE PRECISION
C
        DO 76 I1=1,2
        DO 75 J1=1,2
        PXMS(I1,J1)=PXM(I1,J1)
 75     CONTINUE
 76     CONTINUE
C----------------------------------------------------
C WE HAVE NOW A REAL V0 AND WE HAVE TO STORE IT
C ----------------------------------------------------
C
C
C+  AT MAXIMUM MAXNV0 V0 CANDIDATES
C
                 IF(IV0NU.GT.MAXNV0)THEN
                   IERCO=4
                   GO TO 80
                 ENDIF
  80    CONTINUE
        RETURN
        END
#endif
