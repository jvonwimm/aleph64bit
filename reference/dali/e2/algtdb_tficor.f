C*HE 10/18/94 17:14:03
C*DK TFICOR
      SUBROUTINE TFICOR(IEND,IROW,R,PHI,Z,RCR,PHICR,ZCR)
C
C----------------------------------------------------------------------
C! Correct TPC Coordinates for field distortions in the Rphi-plane.
C! This correction was developped for the data where a short
C! in the fieldcage caused large coordinate distortions.
C!
C!
C!  Author    :   W. Wiedenmann  91/11/18
C!  Modified  :   I. Tomalin     94/01/24
C!                Allow original polynomial parameterization
C!                to be multiplied by a specified function of the
C!                coordinates, which could for example constrain
C!                corrections to be zero at the endplates.
C!
CKEY TPC FIELD-CORRECTION
C!
C!  Input     :
C!                IEND  /I  : TPC side A (=1), B (=2)
C!                IROW  /I  : TPC pad row number
C!                R     /R  : radius of TPC coordinate  [cm]
C!                PHI   /R  : angle  of TPC coordinate  [radian]
C!                Z     /R  : z of TPC coordinate [cm]
C!
C!  Output     :  TCR   /R  : corrected R coordinate
C!                PHICR /R  : corrected PHI coordinate
C!                ZCR   /R  : corrected Z coordinate
C!
C----------------------------------------------------------------------
C*IF .NOT.DOC
      SAVE
C
C*CA ALCONS
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
C*IF DOC
C*EI
C*CC ALCONS
C*CA EVEHJJ
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
C*CC EVEHJJ
C
      INCLUDE 'A_BCS.INC'
C
C*CC BCS
C*CA TPGPAR
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
C*IF DOC
C*EI
C*CC TPGPAR
C*CA TPGEOM
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
C*IF DOC
C*EI
C*CC TPGEOM
C*CA T3FCJJ
      PARAMETER(JT3FSI=1,JT3FND=2,JT3FNC=3,JT3FIP=4,JT3FIO=5,JT3FIC=6,
     +          JT3FCO=156,LT3FCA=205)
C*CC T3FCJJ
C*CA T3RRJJ
      PARAMETER (JT3RR1=1,JT3RR2=2,JT3RBK=3,JT3RS1=4,JT3RS2=5,LT3RRA=5)
C*CC T3RRJJ
C
C++   Definitions
C
      PARAMETER (NDMAX=3,NCOMAX=LT3FCA-JT3FCO+1)
      DIMENSION COEFF(NCOMAX,2),IBASFT(NDMAX,NCOMAX,2)
      DIMENSION ND(2),NCO(2),IPOLY(2),IFUNC(2),ICOCOR(2)
      DIMENSION XCOR(3),XN(3)
      DOUBLE PRECISION COEFF,P,TFIPOL,DRPHI8
      LOGICAL FIRST, FCORR
      DATA FIRST/.TRUE./
C
C*CA BMACRO
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
C*IF ETA
C*EI
C*CC BMACRO
C ---------------------------------------------------------------------
C - 1st entry
      IF (FIRST) THEN
        FIRST=.FALSE.
        NT3FC=NAMIND('T3FC')
        NT3RR=NAMIND('T3RR')
        NEVEH=NAMIND('EVEH')
        IRLST=0
        NBNK  = 0
        FCORR = .FALSE.
        LDBAS = JUNIDB(0)
        JT3RR = MDARD (IW,LDBAS,'T3RR',1)
      ENDIF
C - next entry ======================================================
C
C - Initialisation
      RCR   = R
      PHICR = PHI
      ZCR   = Z
C
C - If T3RR does not exist Return
      JT3RR = IW(NT3RR)
      IF (JT3RR.EQ.0) RETURN
C
C++   Get the current run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) RETURN
      IRUN=IW(KEVEH+JEVERN)
C
C++   IF it is a new run THEN Link to the TPC Rphi-correction bank
C
      IF (IRLST.NE.IRUN) THEN
         IRLST=IRUN
         DO 10 I=1,LROWS(JT3RR)
            JROW = I
            IRUN1 = ITABL(JT3RR,I,JT3RR1)
            IRUN2 = ITABL(JT3RR,I,JT3RR2)
            IF (IRUN.GE.IRUN1 .AND. IRUN.LE.IRUN2) GOTO 20
 10      CONTINUE
C - run # IRUN is not in the run range of corrections - Return
         FCORR = .FALSE.
         RETURN
C
C - run # IRUN is in the run range of row # JROW
 20      CONTINUE
         FCORR = .TRUE.
         IF (NBNK.NE.ITABL(JT3RR,JROW,JT3RBK)) THEN
             KT3FC = NDROP ('T3FC',NBNK)
             NBNK  = ITABL(JT3RR,JROW,JT3RBK)
             KT3FC = MDARD (IW,LDBAS,'T3FC',NBNK)
             IF (KT3FC.EQ.0) THEN
                WRITE (IW(6),*) ' TFICOR: missing bank T3FC,NR= ',NBNK
     &                          ,IRUN,IRUN1,IRUN2
                STOP
             ELSE
                DO 30 IR=1,LROWS(KT3FC)
                   IS         = ITABL(KT3FC,IR,JT3FSI)
                   ND(IS)     = ITABL(KT3FC,IR,JT3FND)
                   NCO(IS)    = ITABL(KT3FC,IR,JT3FNC)
                   IFPOL      = ITABL(KT3FC,IR,JT3FIP)
                   IPOLY(IS)  = MOD(IFPOL,10)
                   IFUNC(IS)  = IFPOL/10
                   ICOCOR(IS) = ITABL(KT3FC,IR,JT3FIO)
                   DO 31 IC=1,NCOMAX
                     COEFF(IC,IS) = RTABL(KT3FC,IR,JT3FCO-1+IC)
                     DO 32 ID=1,NDMAX
                        IBASFT(ID,IC,IS) =
     >                      ITABL(KT3FC,IR,JT3FIC-1+(ID-1)*NCOMAX+IC)
32                   CONTINUE
31                 CONTINUE
30              CONTINUE
             ENDIF
         ENDIF
      ENDIF
C
C - normal entry   ===================================================
C
C++   Check if coordinate has to be corrected
C
      IF (.NOT.FCORR) RETURN
      IF (IEND.NE.ITABL(JT3RR,JROW,JT3RS1+IEND-1)) RETURN
C
C++   Coordinate sequence for correction and use scaled coordinates
C
      XCOR(1) = R/RTPCMX
      XCOR(2) = PHI
      XCOR(3) = Z/ZTPCMX
      I1 = ICOCOR(IEND)/100
      I2 = (ICOCOR(IEND)-I1*100)/10
      I3 = ICOCOR(IEND)-I1*100-I2*10
      XN(1) = XCOR(I1)
      XN(2) = XCOR(I2)
      XN(3) = XCOR(I3)
C
C++   Calculate Rphi correction
C++   (see also HRVAL in HBOOK lib for multidimensional regression)
C
      DRPHI8=0.
      DO 100 K=1,NCO(IEND)
         P=1.
         DO 200 I=1,ND(IEND)
            NUM=IBASFT(I,K,IEND)/10
            ITYP=IBASFT(I,K,IEND)-NUM*10
            IF (NUM.NE.0) THEN
               IF (ITYP.EQ.0) P=P*TFIPOL(IPOLY(IEND),NUM,XN(I))
            ENDIF
  200    CONTINUE
         DRPHI8=DRPHI8+COEFF(K,IEND)*P
  100 CONTINUE
C
C Multiply polynomial correction by addition function if requested.
      IECON = MOD(IFUNC(IEND),10)
      IF (IECON.EQ.1) THEN
C Function to make correction and its first derivative w.r.t. z
C zero at endcap. (Use if fitting TPC halves with independent
C polynomials).
        DRPHI8 = DRPHI8*(1.0 - ABS(XCOR(3)))**2
      ELSE IF (IECON.EQ.2) THEN
C Function to make correction and its first derivative w.r.t. z
C zero at both endcaps. (Use if fitting TPC halves with same
C polynomial).
        DRPHI8 = DRPHI8*(1.0 - XCOR(3)**2)**2
      END IF
C Function to dampen fitted polynomial at small polar angles where
C there was no data.
      IDAMP = IFUNC(IEND)/10
      IF (IDAMP.EQ.1) THEN
        DRPHI8 = DRPHI8*EXP(-ABS(XCOR(3))/XCOR(1))
      END IF
C
C++   Correct coordinates
C
      PHICR = PHI - DRPHI8/R
      IF (PHICR.GT.TWOPI) THEN
        PHICR=PHICR-TWOPI
      ELSEIF (PHICR.LT.0.) THEN
        PHICR=PHICR+TWOPI
      ENDIF
C
      RETURN
      END
