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
C                                      matrix of charged tracks in ynv0v
C                                      if 3, problems with coovariance
C                                      matrix of charged tracks in ynvon
C                                      if 4, the number of V0 candidates
C                                      is greater than 50
C                                      IF 5 THE FRFT BANK IS MISSING
C                                      IF 6 LESS THAN TWO TRACKS IN FRFT
C------------------------------------------------------
      SAVE
C
      EXTERNAL ALFIEL
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER(JJSUNT=1,JJSUNV=2,JJSUNZ=3,JJSUNL=4,JJSUNB=5,JJSUVT=6,
     +          JJSUVV=7,JJSUVZ=8,JJSUVL=9,JJSUVB=10,JJSUTT=11,
     +          JJSUTZ=12,JJSUTB=13,JJSULI=14,JJSUIZ=15,JJSULO=16,
     +          JJSULZ=17,JJSUXV=18,JJSUYV=19,JJSUZV=20,JJSUXS=21,
     +          JJSUYS=22,JJSUZS=23,JJSUKB=24,JJSUKW=25,JJSUTN=26,
     +          JJSUTS=27,JJSUTV=28,JJSUA0=29,JJSUA1=30,JJSUA2=31,
     +          JJSUA3=32,JJSUA4=33,JJSUA5=34,JJSUA6=35,JJSUA7=36,
     +          JJSUA8=37,JJSUA9=38,JJSUB0=39,JJSUB1=40,JJSUB2=41,
     +          JJSUB3=42,JJSUB4=43,JJSUB5=44,JJSUB6=45,JJSUB7=46,
     +          JJSUB8=47,JJSUB9=48,LJSUMA=48)
      PARAMETER(JYV1ID=1,JYV1VR=2,JYV1IV=4,JYV1O1=5,JYV1O2=6,JYV1CT=7,
     +          JYV1PS=8,JYV1RV=9,JYV1ZV=10,JYV1DI=11,JYV1CF=12,
     +          JYV1CS=13,JYV1CO=14,JYV1MA=15,JYV1PM=16,JYV1NT=17,
     +          JYV1CB=18,JYV1NC=19,JYV1MD=20,JYV1DZ=21,JYV1ZZ=22,
     +          JYV1KW=23,JYV1LW=24,JYV1GW=25,LYV1CA=25)
      PARAMETER(JYV0K1=1,JYV0K2=2,JYV0VX=3,JYV0VY=4,JYV0VZ=5,JYV0VM=6,
     +          JYV0PX=12,JYV0PY=13,JYV0PZ=14,JYV0PM=15,JYV0X1=21,
     +          JYV0X2=22,JYV0XM=23,JYV0C2=26,JYV0IC=27,JYV0P1=28,
     +          JYV0P2=31,JYV0EP=34,JYV0DM=55,JYV0S1=56,JYV0S2=57,
     +          LYV0VA=57)
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
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
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
C
C+    DEFINE LOGICAL UNIT FOR DATA BASE
C
               LDB=JUNIDB(DUMMY)
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
         KFRFT=IW(NAMIND('FRFT'))
         KFRTL=IW(NAMIND('FRTL'))
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
