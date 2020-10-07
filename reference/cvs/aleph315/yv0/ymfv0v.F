      SUBROUTINE YMFV0V(IBOOK,IV0BK,IERCO,NUMV0,CHIOR,QGA,CHQGA,QK0,
     $                  CHQK0,QLA,CHQLA,QAL,CHQAL,XMGA,CXMGA,XMK0,
     $                  CXMK0,XMLA,CXMLA,XMAL,CXMAL,NCOR,DISCO)
C----------------------------------------------------------------
C!Main V0 Vertex
CKEY YV0 MAIN /INTERNAL
C   AUTHOR: M.A.CIOCCI   L.ROLANDI  20/9/87
C   MODIFIED: M.A.Ciocci 20/1/90
C   MODIFIED: M.A.Ciocci 10/6/90
C             change on dimension of output from 20 to 50,
C             remove cut on chi square of invariant mass
C   Modified: J.Sedgbeer 09/04/91 Common/YV0CUT/ extended, more cuts.
C             More Histos. Entry point YMFV00 to set max allowed no. V0.
C   Modified: M.A.Ciocci 20/01/93.Introduction of the covariance matrix
C             of the primary vertex (sivpr(3,3)), used now from YCHIV2
C   Modified: M.Cattaneo 28/01/98 Protect against precision problem
C
C   NEEED : FRFT , FRTL, PYER (OR JSUM),ITCO,TPCO,YV1C (DATA BASE OR
C           CARDS)
C   INSIDE ALPHA MAKE UNPACKING OF TP IT (TPC COORDINATES
C   ITC COORDINATES)
C
C   OUTPUT BANKS : 'YV0V'
C
C
C     CALLED: USER
C     CALLS:  YFPSIN,YV0VER,YFQERQ,YCHIV2
C
C            DESCRIPTION
C            ===========
C   V0 RECONSTRUCTION STEERING ROUTINE
C
C   OUTPUT: YV0V BANK
C
C   LOOPS ON ALL TRACKS TO RECONSTRUCT V0'S
C   DESCRIPTION OF THE METHOD IS IN THE ALEPH NOTE ....
C   V0 RECONSTRUCTION ALGORITHM
C
C
C       INPUT:
C                IBOOK/LOGICAL FLAG    if true histograms for V0
C                                      will be filled
C                IV0BK/INTEGER         Histograms for V0 will start
C                                      from IV0BK+200
C       OUTPUT:
C                IERCO/INTEGER         code for error:
C                                      if 0 all is ok
C                                      if 1, problems with covariance matrix
C                                      of charged tracks in ychiv2
C                                      if 2, problems with covariance matrix
C                                      of charged tracks in ynv0ve
C                                      if 3, problems with covariance matrix
C                                      of charged tracks in ynvonf
C                                      if 4, problems in yvcame with frft
C                                      if 5, problems inverting covariance
C                                      matrix of reconstructed V0 vertex
C                                      candidate
C                                      if 6, the number of V0 candidates is
C                                      greater than MAXV
C                                      if 7, no booked enlarged space for yv0v
C                                      if 8, problems with working banks
C                                      if 9 no space for yv0v bank
C                                      if 10 no yv1c on dbase or cards
C
C                NUMV0/INTEGER         Number of V0 candidates (at
C                                      maximum MAXV)
C
C                CHIOR(2,*)/REAL       chi2 increasing constraining
C                                      the track coming from V0 candidate
C                                      to the origin
C                QGA(*)/REAL           Q value in the gamma hypothesis
C                CHQGA(*)/REAL         chi2 of Q value in the gamma hypothesis
C                QLA(*)/REAL           Q value in the lambda hypothesis
C                CHQLA(*)/REAL         chi2 of Q value in the lambda hypothesis
C                QAL(*)/REAL           Q value in the antilambda hypothesis
C                CHQAL(*)/REAL         chi2 of Q value in the antilambda
C                                      hypothesis
C                QK0(*)/REAL           Q value in the k0 hypothesis
C                CHQK0(*)/REAL         chi2 of Q value in the k0 hypothesis
C                XMGA(*)/REAL          invariant mass in the gamma hypothesis
C                CXMGA(*)/REAL         chi2 of invariant mass in the gamma
C                                      hypothesis
C                XMLA(*)/REAL          invariant mass in the lambda hypothesis
C                CXMLA(*)/REAL         chi2 of the invariant mass in the \
C                                      lambda hypothesis
C                XMAL(*)/REAL          invariant mass in the antilambda
C                                      hypothesis
C                CXMAL(*)/REAL         chi2 of invariant mass  in the
C                                      antilambda hypothesis
C                XMK0(*)/REAL          invariant mass in the k0 hypothesis
C                CXMK0(*)/REAL         chi2 of the invariant mass in the
C                                      k0 hypothesis
C
C                NCOR(2,*)/I           number of coordinates before the
C                                      reconstructed vertex
C                DISCO(2,*)/R          distance (cm) in radius that the
C                                      furthest in coordinate is inside
C                                      of the radius of the vertex
C------------------------------------------------------
#ifndef DOC
      SAVE
C
      EXTERNAL YCHIV2,ALFIEL
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
      COMMON/YV0WOR/KIPPXX,KIPNXX,KICPXX,KICNXX
C
C+  OUTPUT DIMENSION
C
      REAL CHIOR(2,*),QGA(*),CHQGA(*),QK0(*),CHQK0(*),QLA(*),
     $CHQLA(*),QAL(*),CHQAL(*),XMGA(*),CXMGA(*),XMK0(*),CXMK0(*),
     $XMLA(*),CXMLA(*),XMAL(*),CXMAL(*),NCOR(2,*),DISCO(2,*)
C
C+  END OUTPUT DIMENSION
C
      INTEGER INDE,NDF
      DOUBLE PRECISION CHI0
      REAL COORI(3),COORJ(3),V1(3),V2(3),WR(6),PXMS(2,2),EP(6,6)
C
      REAL FIELM,ALFAC,AMK0,AMLA
      PARAMETER (ALFAC=0.29979E-3,AMK0=0.497671,AMLA=1.11563)
      REAL VPRIM(3),SIVPR(3,3),VERT(3),PV0(3),PX(2),VERM(3,3),
     $PV1(3),PV2(3),VD0(3)
      DOUBLE PRECISION VVMAT(3,3),PVMAT(3,3),PXM(2,2),
     $     CPV(6,6)
C
      LOGICAL REAV0,ISK0,ISLA,ISAL,ISGA
C
      PARAMETER (NV0M = 10, NDV0L= LYV0VA*NV0M+LMHLEN,
     +           NXV0L= 3*LYV0VA+LMHLEN)
C
         LOGICAL IBOOK,FIRST,FBAN
         DATA FIRST/.TRUE./
         DATA FBAN/.TRUE./
         DATA MAXNV0/50/
#include "bmacro.h"
C      STARTING VALUE FOR ERROR CODE
C
            IERCO=0
C
C+     STARTING VALUE OF NUMBER V0 CANDIDATES
C
             NUMV0=0
c
C+ histograms will start from joff+200
C
            JOFF=IV0BK
              IF(FIRST)THEN
C
C+     BOOKING STARTING FROM JOFF+....
C
                    IF(IBOOK)CALL YV0BOK(JOFF)
C
C+   STANDARD  CUTS FROM DATA BASE
C    See current Database for default values.
C
C           YV0CIV=1.
C           YV0CO1=0.
C           YV0CO2=0.
C           YV0CCT=2.
C           YV0CPS=0.
C           YV0CRV=180.
C           YV0CZV=220.
C           YV0DIP=0.06
C           YV0CC2=13.
C           YV0CCS=0.92
C           YV0CC0=0.
C           YV0CMA=50.
C
C+        READ YV1C BANK AND FILL YV0CUT BANK
C+        YV1C CONTAINS THE STANDARD CUTS FOR V0 RECONSTRUCTION
C+
C+        TO CHANGE CUTS -> CHANGE YV1C BANK
C+
C
C+    DEFINE LOGICAL UNIT FOR DATA BASE
C
               LDB=JUNIDB(0)
               CALL YV1INI(LDB,IFLAG,NRYV1)
                 IF(IFLAG.NE.0)THEN
                     IERCO=10
                     GO TO 90
                 ENDIF
                    FIRST=.FALSE.
              ENDIF
C
C+  TEST IF THE YV0V BANK EXIST IF IT EXIST THEN DROP YV0V
C
            KYV0V=IW(NAMIND('YV0V'))
                IF(KYV0V.GT.0)THEN
                  NRYV0=IW(KYV0V-2)
                  IF(NRYV0.LT.NRYV1)THEN
                      CALL BDROP(IW,'YV0V')
                  ELSE
                      RETURN
                  ENDIF
                ENDIF
                IF(FBAN)THEN
                      NAYV0V=NAMIND('YV0V')
                      CALL BKFMT('YV0V','2I,(2I,24F,I,30F)')
                      CALL BLIST(IW,'E+','YV0V')
                ENDIF
C
C+     CHECK IF BOOKING IS REQUESTED
C
C
C
C  WE BOOK NOW THE SPACE FOR THE OUTPUT BANK.
C
                IF(FBAN)THEN
                     KYV0V=IW(NAYV0V)
                       IF (KYV0V.EQ.0) THEN
                           IW(1)=1
                           CALL AUBOS('YV0V',NRYV1,NDV0L,KYV0V,IGARB)
                           IF (IGARB.EQ.2) GOTO 998
                           IW(KYV0V+LMHCOL)=LYV0VA
                           IW(KYV0V+LMHROW)=0
                       ENDIF
                ENDIF
C
C+ Field
      FIELM = ALFIEL(DUM)
      FIELM = ABS(FIELM)
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
         KFRFT=IW(NAMIND('FRFT'))
         KFRTL=IW(NAMIND('FRTL'))
C
C  IF NO TRACK BANK WE SKIP THE V0 RECONSTRUCTION !!!
C
        IF (KFRFT.EQ.0) GOTO 90
C
C+      NTRFR,NUMBER OF TRACKS IN FRFT BANK
C
        NTRFR=LROWS(KFRFT)
        IF (NTRFR.LT.2) GOTO 90
C
C+   DIVIDE TRACKS INTO POSITIVE AND NEGATIVE
C+   SELECTING CANDIDATES FOR A V0
C+
C+    THE INFORMATION IS STORED IN TWO WORK BANKS
C+
C
        KIPPXX=0
        KIPNXX=0
        KICPXX=0
        KICNXX=0
        CALL WBANK(IW,KIPPXX,NTRFR,*999)
        CALL WBANK(IW,KIPNXX,NTRFR,*999)
        CALL WBANK(IW,KICPXX,NTRFR,*999)
        CALL WBANK(IW,KICNXX,NTRFR,*999)
C
        NP=0
        NN=0
        DO 22 ISTRA=1,NTRFR
C
C+ WE ASK AT LEAST IYV0NT POINTS IN TPC and minimum momentum
C
        NCOTP=ITABL(KFRTL,ISTRA,JFRTNT)
        NCOIT=ITABL(KFRTL,ISTRA,JFRTNI)
        IF(NCOTP.LT.IYV0NT) GO TO 22
        PT = ALFAC*FIELM/ABS(RTABL(KFRFT,ISTRA,JFRFIR))
        TL = RTABL(KFRFT,ISTRA,JFRFTL)
        PP = PT*SQRT(1.+TL**2)
        IF(PP.LT.YV0CPM) GOTO 22
        R1=YCHIV2(ISTRA,VPRIM,SIVPR,IER0)
          IF(IER0.NE.0)THEN
              IERCO=1
              GO TO 22
          ENDIF
C
C+      IF THER'ARE PROBLEM ON COVARIANCE MATRIX R1.LT.0
C
             IF(R1.LT.0.)THEN
                IERCO=1
                GO TO 22
             ENDIF
C
C
C     HERE WE CUT EVENTUALLY ON THE CHI2 OF THE SINGLE
C     TRACK RESPECT TO THE ORIGIN , BUT IT IS DANGEROUS
C
C
               IF(IBOOK)THEN
                         CALL HF1(JOFF+200,R1,1.)
               ENDIF
        IF (R1.LT.YV0CO1) GOTO 22
           IF (RTABL(KFRFT,ISTRA,JFRFIR).LT.0.) THEN
                 NP=NP+1
                 IW(KIPPXX+NP)=ISTRA
                 RW(KICPXX+NP)=R1
           ELSE
                NN=NN+1
                IW(KIPNXX+NN)=ISTRA
                RW(KICNXX+NN)=R1
           ENDIF
 22     CONTINUE
C
C
C  WE LOOK NOW AN ALL "SELECTED" PAIRS
C
        DO 70 I=1,NP
        NFRPI=IW(KIPPXX+I)
C
C+      NFRPI, NUMBER OF THE POSITIVE TRACK IN THE FRFT BANK
C
        DO 80 J=1,NN
C
C+  APPLY A CUT ON THE SUM OF THE TWO CHI2 AT THE ORIGIN
C+   THIS IS MORE SAFE
C
              NFRNJ=IW(KIPNXX+J)
                 IF(IBOOK)THEN
                     CALL HF1(JOFF+201,RW(KICPXX+I)+RW(KICNXX+J),1.)
                 ENDIF
          IF (RW(KICPXX+I)+RW(KICNXX+J).LT.YV0CO2) GOTO 80
C
C+      NFRNJ, NUMBER OF THE NEGATIVE  TRACK IN THE FRFT BANK
C
C
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
                   IF(IBOOK)THEN
                        CALL HF1(JOFF+202,CHIT,1.)
                   ENDIF
C
C+      CUT ON dISTANCE BETWEEN THE TWO
C+      STARTING (ALONG HELICES) FIT POINT
C
        IF (CHIT.GT.YV0CCT) GOTO 80
              IF(IBOOK)THEN
                 CALL HF1(JOFF+203,PSII,1.)
                 CALL HF1(JOFF+203,PSIJ,1.)
              ENDIF
C
C  TEST IF THE PSI ANGLES CALCULATED RESPECT TO THE PRIMARY
C  ARE IN THE "PHISICAL REGION"
C  OF THE TRAJECTORY (I.E. PSI > 0)
C
C
        CALL YPSIVE(VPRIM,NFRPI,PSIIN)
        CALL YPSIVE(VPRIM,NFRNJ,PSIJN)
        PSINEW=PSII-PSIIN
        PSJNEW=PSIJ-PSIJN
        IF (PSINEW.LT.YV0CPS.OR.PSJNEW.LT.YV0CPS) GOTO 80
C
C  TEST NOW THAT THIS APPROXIMATE VERTEX POSITION ( THE MEDIUM POINT
C  BETWEEN THE COORDINATES) IS NOT OUTSIDE THE TRACKING DETECTORS
C
        CALL VLINE(COORI,0.5,COORJ,0.5,V2,3)
        DIST2=VMOD(V2,2)
C
                 IF(IBOOK)THEN
                         CALL HF1(JOFF+204,DIST2,1.)
                 ENDIF
        IF (DIST2.GT.YV0CRV) GOTO 80
C
                 IF(IBOOK)THEN
                         CALL HF1(JOFF+205,ABS(V2(3)),1.)
                 ENDIF
        IF (ABS(V2(3)).GT.YV0CZV) GOTO 80
C
C NOW WE HAVE A VERTEX CANDIDATE : WE TRY THE VERTEX FIT
C
        DELDI=ABS(RTABL(KFRFT,NFRPI,JFRFTL)-RTABL(KFRFT,NFRNJ,JFRFTL))
                 IF(IBOOK)THEN
                         CALL HF1(JOFF+206,DELDI,1.)
                 ENDIF
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
              IF(IBOOK)THEN
                         CALL HF1(JOFF+207,CHI2,1.)
              ENDIF
C
C TEST ON CHISQUARE OF THE FIT
C
C
         IF (CHI2.GT.YV0CC2) GOTO 80
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
                      IF(IBOOK)THEN
                        CALL HF1(JOFF+208,CHI2,1.)
                      ENDIF
C
C TEST ON CHISQUARE OF THE FIT
C
         IF (CHI2.GT.YV0CC2*100.) GOTO 80
C
             ENDIF
C
C+   TEST IF THER'ARE COORDINATES ASSOCIATED TO THE TRACKS BEFORE VERTEX
C
        DISD=VMOD(VERT,2)
      CALL YVCAME(NFRPI,DISD,NCO1,DISI,IER1)
      CALL YVCAME(NFRNJ,DISD,NCO2,DISJ,IER2)
             IF (IER1.NE.0.OR.IER2.NE.0)THEN
                IERCO=4
                GOTO 80
              ENDIF
         IF(IBOOK)THEN
              CALL HF1(JOFF+209,FLOAT(NCO1),1.)
              CALL HF1(JOFF+209,FLOAT(NCO2),1.)
         ENDIF
C
C If 'CB' =1 require at least one track with <= NC coords before vx.
C         =2   "     both tracks <= NC coords before vx.
       IF(IYV0CB.EQ.1) THEN
         IF(NCO1.GT.IYV0NC.AND.NCO2.GT.IYV0NC) GOTO 80
       ELSEIF(IYV0CB.EQ.2) THEN
         IF(NCO1.GT.IYV0NC.OR.NCO2.GT.IYV0NC) GOTO 80
       ENDIF
C
C  TEST ON COLLINEARITY RESPECT TO THE MAIN VERTEX
C  POSITION

C
        CALL VSUB(VERT,VPRIM,V1,3)
        COSS=VDOTN(PV0,V1,3)
C
                     IF(IBOOK)THEN
                         CALL HF1(JOFF+210,COSS,1.)
                     ENDIF
        IF (COSS.LT.YV0CCS) GOTO 80
C
C Test on d0 and Z0 of V0 wrt the primary vx.
C
        PP2 = VMOD(PV0,2)
        ALF = -1.0*VDOT(V1,PV0,2)/PP2**2
        CALL VSCALE(PV0,ALF,VD0,3)
        CALL VADD(V1,VD0,VD0,3)
        V0D0 = VMOD(VD0,2)
        V0Z0 = ABS(VD0(3))
                     IF(IBOOK)THEN
                         CALL HF1(JOFF+211,V0D0,1.)
                     ENDIF
        IF(V0D0.GT.YV0CDZ) GOTO 80
                     IF(IBOOK)THEN
                         CALL HF1(JOFF+212,V0Z0,1.)
                     ENDIF
        IF(V0Z0.GT.YV0CZZ) GOTO 80
C
C Test distance of vx from primary vx in 3-D.
C If no good prim vx. then test in 2-D (xy) with Cut/2.
C
        IF(vprim(1).eq.0..and.vprim(2).eq.0..and.vprim(3).eq.0.) THEN
          DIS3=VMOD(V1,3)
                     IF(IBOOK)THEN
                         CALL HF1(JOFF+213,DIS3,1.)
                     ENDIF
          IF(DIS3.LT.YV0CMD) GOTO 80
        ELSE
          DIS2=VMOD(V1,2)
                     IF(IBOOK)THEN
                         CALL HF1(JOFF+214,DIS2,1.)
                     ENDIF
          IF(DIS2.LT.(YV0CMD*0.5)) GOTO 80
        ENDIF
C
C       covariance matrix of reconstructed v0 vertex candidate
C
        DO 100 LS=1,3
        DO 110 LJ=1,3
        VERM(LS,LJ)=VVMAT(LS,LJ)
  110   CONTINUE
  100   CONTINUE
C
C       covariance matrix of reconstructed momenta of particles
C       coming from v0 vertex candidate
C
        DO 121 LS=1,6
           DO 122 LJ=1,6
           EP(LS,LJ)=SNGL(CPV(LS,LJ))
  122   CONTINUE
  121   CONTINUE
C
C+ The covariance matrix of (VERT-VPRIM) is given by
C+ the addition of vvmat and sivpr
C

        DO 123 II=1,3
        DO 124 JJ=1,3
        VVMAT(II,JJ)=VVMAT(II,JJ)+DBLE(SIVPR(II,JJ))
 124    CONTINUE
 123    CONTINUE
C
C+ INVERT THE MATRIX VVMAT
C
#ifndef BIT64
        CALL DINV(3,VVMAT,3,WR,IFAIL)
#else
        CALL RINV(3,VVMAT,3,WR,IFAIL)
#endif
                    IF (IFAIL.NE.0) THEN
              IERCO=5
              GOTO 80
                    ENDIF
C
C   TEST THE CHISQUARE INCREASE WHEN CONSTRAINING THE
C   VERTEX TO THE ORIGIN
C
        CHI0=0
        DO 74 I1=1,3
        DO 73 J1=1,3
        CHI0=CHI0+DBLE(VERT(I1)-VPRIM(I1))
     $     *VVMAT(I1,J1)
     $     *DBLE(VERT(J1)-VPRIM(J1))
   73   CONTINUE
   74   CONTINUE
C
        CHI00=CHI0
                    IF(IBOOK)THEN
                         CALL HF1(JOFF+215,CHI00,1.)
                    ENDIF
        IF (CHI00.LT.YV0CC0) GOTO 80
C
C TEST NOW THE COMPATIBILITY WITH THE "MASS" HYPOTHESIS
C
        DO 76 I1=1,2
        DO 75 J1=1,2
        PXMS(I1,J1)=PXM(I1,J1)
 75     CONTINUE
 76     CONTINUE

        ISK0=.FALSE.
        CALL YFQERQ('K0',PX,PXMS,PV0,QVAL,ERQ2)
        QVK0=QVAL
        CVK0=QVAL*QVAL/ERQ2
                IF(IBOOK)THEN
                   CALL HF1(JOFF+216,QVAL,1.)
                   CALL HF1(JOFF+217,CVK0,1.)
                ENDIF
        IF (CVK0.LT.YV0CMA) ISK0=.TRUE.

        ISLA=.FALSE.
        CALL YFQERQ('LA',PX,PXMS,PV0,QVAL,ERQ2)
        QVLA=QVAL
        CVLA=QVAL*QVAL/ERQ2
                  IF(IBOOK)THEN
                    CALL HF1(JOFF+218,QVAL,1.)
                    CALL HF1(JOFF+219,CVLA,1.)
                  ENDIF
        IF (CVLA.LT.YV0CMA) ISLA=.TRUE.

        ISAL=.FALSE.
        CALL YFQERQ('AL',PX,PXMS,PV0,QVAL,ERQ2)
C
        QVAL=QVAL
        CVAL=QVAL*QVAL/ERQ2
                   IF(IBOOK)THEN
                       CALL HF1(JOFF+218,QVAL,1.)
                       CALL HF1(JOFF+219,CVAL,1.)
                   ENDIF
        IF (CVAL.LT.YV0CMA) ISAL=.TRUE.

        ISGA=.FALSE.
        CALL YFQERQ('GA',PX,PXMS,PV0,QVAL,ERQ2)
        QVGA=QVAL
C In Gamma case, protect against precision problem
        IF( ERQ2 .EQ. 0. ) THEN
           CVGA=QVAL/(4.*PXMS(2,2))
        ELSE
           CVGA=QVAL*QVAL/ERQ2
        ENDIF
                   IF(IBOOK)THEN
                      CALL HF1(JOFF+220,QVAL,1.)
                      CALL HF1(JOFF+221,CVGA,1.)
                   ENDIF
        IF (CVGA.LT.YV0CMA) ISGA=.TRUE.

          CALL YV0MAS('K0',PX,PXMS,PV1,PV2,EP,XM,CXM)
          XK0=XM
          CXK0=CXM
          CALL YV0MAS('AL',PX,PXMS,PV1,PV2,EP,XM,CXM)
          XAL=XM
          CXAL=CXM
          CALL YV0MAS('LA',PX,PXMS,PV1,PV2,EP,XM,CXM)
          XLA=XM
          CXLA=CXM
          CALL YV0MAS('GA',PX,PXMS,PV1,PV2,EP,XM,CXM)
          XGA=XM
          CXGA=CXM
                     IF(IBOOK)THEN
                       CALL HF1(JOFF+222,XK0,1.)
                       CALL HF1(JOFF+223,CXK0,1.)
                       CALL HF2(JOFF+224,XK0,CXK0,1.)
                       CALL HF1(JOFF+225,XLA,1.)
                       CALL HF1(JOFF+226,CXLA,1.)
                       CALL HF2(JOFF+227,XLA,CXLA,1.)
                       CALL HF1(JOFF+225,XAL,1.)
                       CALL HF1(JOFF+226,CXAL,1.)
                       CALL HF2(JOFF+227,XAL,CXAL,1.)
                       CALL HF1(JOFF+228,XGA,1.)
                       CALL HF1(JOFF+229,CXGA,1.)
                       CALL HF2(JOFF+230,XGA,CXGA,1.)
                     ENDIF
CCA         REAV0=ISGA.OR.ISK0.OR.ISLA.OR.ISAL
CCA
C+A   REJECT V0 CANDIDATES IF (CHI2 OF QVAL).GT.YV0CMA  FOR ALL MASS
C+A   HYPOTHESIS
CCA
CCA            IF (.NOT.REAV0) GOTO 80
C
C Reject if mass outside window
C
        IF   (ABS(XK0-AMK0).GT.YV0CKW .AND.
     $        ABS(XLA-AMLA).GT.YV0CLW .AND.
     $        ABS(XAL-AMLA).GT.YV0CLW .AND.
     $                  XGA.GT.YV0CGW) GOTO 80
C
C+      CHI SQUARE THAT THE I,J TRACKS  COME
C+      FROM THE VERTEX
C
        CHISI=YCHIV2(NFRPI,VPRIM,SIVPR,IER1)
            IF(IER1.NE.0.OR.IER2.NE.0)THEN
                   IERCO=1
                   GO TO 80
            ENDIF
        CHISJ=YCHIV2(NFRNJ,VPRIM,SIVPR,IER2)
        CHISM=CHISI
        IF (CHISJ.LT.CHISI) CHISM=CHISJ
                  IF(IBOOK)THEN
                      CALL HF2(JOFF+231,CHI00,CHISM,1.)
                      CALL HF2(JOFF+232,CHI00,CHISI+CHISJ,1.)
                  ENDIF
              RDIST=SQRT(VERT(1)**2+VERT(2)**2)
                  IF(IBOOK)THEN
                      CALL HF1(JOFF+233,RDIST,1.)
                      CALL HF1(JOFF+234,ABS(VERT(3)),1.)
                      CALL HF2(JOFF+235,RDIST,ABS(VERT(3)),1.)
                      CALL HF1(JOFF+236,FLOAT(NDF),1.)
                      CALL HF2(JOFF+237,PX(1),PX(2),1.)
                  ENDIF
C----------------------------------------------------
C WE HAVE NOW A REAL V0 AND WE HAVE TO STORE IT
C ----------------------------------------------------
C
             NUMV0=NUMV0+1
C
C+  AT MAXIMUM MAXNV0 V0 CANDIDATES
C
                 IF(NUMV0.GT.MAXNV0)THEN
                   IERCO=6
                   GO TO 80
                 ENDIF
C
C+  FILL THE OUTPUT
C
             CHIOR(1,NUMV0)=RW(KICPXX+I)
             CHIOR(2,NUMV0)=RW(KICNXX+J)
             QGA(NUMV0)=QVGA
             CHQGA(NUMV0)=CVGA
             QK0(NUMV0)=QVK0
             CHQK0(NUMV0)=CVK0
             QLA(NUMV0)=QVLA
             CHQLA(NUMV0)=CVLA
             QAL(NUMV0)=QVAL
             CHQAL(NUMV0)=CVAL
             XMGA(NUMV0)=XGA
             CXMGA(NUMV0)=CXGA
             XMK0(NUMV0)=XK0
             CXMK0(NUMV0)=CXK0
             XMLA(NUMV0)=XLA
             CXMLA(NUMV0)=CXLA
             XMAL(NUMV0)=XAL
             CXMAL(NUMV0)=CXAL
             NCOR(1,NUMV0)=NCO1
             NCOR(2,NUMV0)=NCO2
             DISCO(1,NUMV0)=DISI
             DISCO(2,NUMV0)=DISJ
C
C+  TRAY TO FILL BANK YV0V
C
C
C  TEST IF ENOUGH SPACE IN THE BOOKED BANK
C
        IF(FBAN)THEN
            IF (LYV0VA*(IW(KYV0V+LMHROW)+1)+LMHLEN.GT.IW(KYV0V)) THEN
                          NDATV=IW(KYV0V)+NXV0L
                          IW(1)=1
                          CALL AUBOS('YV0V',NRYV1,NDATV,KYV0V,IGARB)
                IF (IGARB.EQ.2) THEN
                         IERCO=7
                         GOTO 120
                ENDIF
                        IF (IGARB.NE.0) THEN
                             KFRTL=IW(NAMIND('FRTL'))
                             KFRFT=IW(NAMIND('FRFT'))
                             KYVXL=NLINK('PYER',0)
                        ENDIF
            ENDIF
C
C STORE THE INFORMATION
C
C
        IPO=KNEXT(KYV0V)
        IW(IPO+JYV0K1)=IW(KIPPXX+I)
        IW(IPO+JYV0K2)=IW(KIPNXX+J)
        L1=0
        DO 78 I1=1,3
        RW(IPO+I1+JYV0VX-1)=VERT(I1)
        RW(IPO+I1+JYV0PX-1)=PV0(I1)
        RW(IPO+I1+JYV0P1-1)=PV1(I1)
        RW(IPO+I1+JYV0P2-1)=PV2(I1)

        DO 77 J1=1,I1
        L1=L1+1
        RW(IPO+L1+JYV0VM-1)=VERM(J1,I1)
        RW(IPO+L1+JYV0PM-1)=SNGL(PVMAT(J1,I1))
  77    CONTINUE
  78    CONTINUE
        L2=0
        DO 88 I1=1,6
        DO 87 J1=1,I1
        L2=L2+1
        RW(IPO+L2+JYV0EP-1)=SNGL(CPV(J1,I1))
  87    CONTINUE
  88    CONTINUE
        RW(IPO+JYV0X1)=PX(1)
        RW(IPO+JYV0X2)=PX(2)
        RW(IPO+JYV0XM)=PXMS(1,1)
        RW(IPO+JYV0XM+1)=PXMS(1,2)
        RW(IPO+JYV0XM+2)=PXMS(2,2)
        RW(IPO+JYV0C2)=CHI2
        IW(IPO+JYV0IC)=NDF
        RW(IPO+JYV0DM)=CHIT
        RW(IPO+JYV0S1)=PSII
        RW(IPO+JYV0S2)=PSIJ
        IW(KYV0V+LMHROW)=LROWS(KYV0V)+1
        ENDIF
C  GO TO THE NEXT VERTEX
C
  80    CONTINUE
  70    CONTINUE
C
C       FIX THE LENGTH OF OUTPUT BANK
C
            IF(FBAN)THEN
        KYV0V=IW(NAYV0V)
        NDATA=IW(KYV0V+LMHCOL)*IW(KYV0V+LMHROW)+LMHLEN
        CALL AUBPRS('YV0V')
            ENDIF
 120    CONTINUE
C
C DROP THE SPACE OF THE WORK BANKS
C
        CALL WDROP(IW,KIPPXX)
        CALL WDROP(IW,KIPNXX)
        CALL WDROP(IW,KICPXX)
        CALL WDROP(IW,KICNXX)
  90    CONTINUE
               IF(FBAN)THEN
        CALL AUBPRS('YV0V')
               ENDIF
        RETURN
  999   CONTINUE
        IERCO=8
               IF(FBAN)THEN
        CALL AUBPRS('YV0V')
               ENDIF
        RETURN
 998   CONTINUE
        IERCO=9
        RETURN
C
      ENTRY YMFV00(NVMAX)
      MAXNV0 = NVMAX
      RETURN
        END
#endif
