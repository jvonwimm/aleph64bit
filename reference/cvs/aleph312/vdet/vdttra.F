      SUBROUTINE VDTTRA(ITRAC,FIELD,
     +                  VV0IN,CHI2IN,NCOTPC,NCOITC,
     +                  LISTPC,LISITC,VV0,CC0,CHI2,NDEG)
C----------------------------------------------------------------
CKEY COMPUTE FIT
C! Steering routine for the VDET pattern recognition
C!              ===>  based on extended kalman filtering
C!              ===>  calls UFTKAL
C!
C! This is to be used for the pattern recognition in VDET,
C! using TPC, ITC. 
C! This routine takes into account correlations
C! between all measuring planes which result from multiple
C! scattering.
C! This routine is a copy of the VDET pattern recognition part
C! of UFTTRA/alephlib_285 where references to VDET coordinates have
C! all been removed.
C!
C!    
C!    AUTHOR: F.Ranjard  - 970312   
C!    from UFTTRA (T.Lohse, B.Mours, G.Taylor)
C!
C!    Modified: D.Casper 27-05-97  Support for Kalman Filter tracing:
C!                                 Give UFTR card NR=event, with a list
C!                                 of track numbers to dump the state
C!                                 for during the final fit.
C!
C!    INPUT:  ITRAC  = FRFT track number of track being fitted
C!            FIELD  = Magnetic field strength in kG
C!            VV0IN  = 5 INPUT TRACK PARAMETERS FROM
C!                     A PRELIMINARY FIT
C!              1 : 1/R         [1/CM]   NEG. IF CLOCKWISE
C!              2 : TAN(LAMBDA)  =DZ/DS} TAN(ANGLE TO X,Y PLANE)
C!              3 : PHI0        [0,2PI]} ANGLE TO X-AXIS
C!              4 : D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,
C!                                       sign OF ANGULAR MOM. LZ
C!              5 : Z0           [CM]    Z POS AT R=D0
C!            CHI2IN = CHI**2 OF PRELIMINARY FIT
C!
C!            NCOTPC = NUMBER OF TPC COORDINATES
C!            NCOITC = NUMBER OF ITC COORDINATES
C!            LISTPC = COORDINATE NUMBERS IN TPCO
C!            LISITC = COORDINATE NUMBERS IN ICCO
C!
C!  OUTPUT: - VV0 = 6 FINAL TRACK PARAMETERS
C!              1 : 1/R          [1/CM]  NEG. IF CLOCKWISE
C!              2 : TAN(LAMBDA)  =DZ/DS} TAN(ANGLE TO X,Y PLANE)
C!              3 : PHI0        [0,2PI]} ANGLE TO X-AXIS
C!              4 : D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,
C!                                       sign OF ANGULAR MOM. LZ
C!              5 : Z0           [CM]    Z POS AT R=D0
C!              6 : ALPHA      [-PI,PI]: SCATTERING ANGLE
C!                                       AT ITC WALL (IN X-Y)
C!          - CC0 = COVARIANCE MATRIX IN LOWER TRIANG. FORM
C!                     1
C!                     2  3
C!                     4  5  6
C!                     7  8  9 10
C!                    11 12 13 14 15
C!                    16 17 18 19 20 21
C!          - CHI2= CHI SQUARED FROM LAST KALMAN FILTER STEP
C!          - NDEG= NUMBER OF DEGREES OF FREEDOM
C!          - VTMA bank
C!
C!  NOTE:     FIT DONE FOR THE 5 HELIX PARAMETERS ONLY.
C!            THE ANGLE ALPHA IS COMPUTED BUT ELEMENTS
C!            16,...,21 OF CC0 ARE DUMMY.
C!
C----------------------------------------------------------------
#ifndef DOC
#include "vtmajj.h"
      INTEGER  NVDIN, NVDOU, ISAVD, ISBVD
      REAL     CI2VD
#include "vtkrec.h"
#include "vtrpar.h"
#include "alcons.h"
#include "bcs.h"
#include "tpcojj.h"
#include "iccojj.h"
C
      INTEGER IRET
      INTEGER UFTKAL,UFVDIN,UFVDOU,UFTRAC
      REAL FILT_PROB
      PARAMETER (MPT=40,MXLRIT=8)
      REAL   VV0IN(*)
      REAL   VV0(*),CC0(*)
      REAL   RF(MPT), UF(MPT), ZF(MPT), COSTHN(MXLRIT),ZLYR(MXLRIT),
     +       SIGU(MPT), SIGZ(MPT), CORUZ(MPT)
      INTEGER LISTPC(*), LISITC(*)
      LOGICAL TRACE
      DATA NTPCO, NICCO, NVTMA /3*0/
C
#include "bmacro.h"
C----------------------------------------------------------------
C
C     remove coordinate filtering
      FILT_PROB = 0.
      CALL UFTFIL(FILT_PROB)
C
      IF (NTPCO.EQ.0) THEN
        NTPCO = NAMIND('TPCO')
        NICCO = NAMIND('ICCO')
        NVTMA = NAMIND('VTMA')
      END IF
C
      TRACE = .FALSE.
      CALL ABRUEV(IRUN,IEVT)
      KUFTR=NLINK('UFTR',IEVT)
      IF (KUFTR.GT.0) THEN
        DO KEVTR = 1, IW(KUFTR)
            IF (ITRAC.EQ.IW(KUFTR+KEVTR)) TRACE=.TRUE.
        ENDDO
      ENDIF
C
      N = NCOTPC + NCOITC
      IF  (N.GT.MPT)                    GOTO 999
      IF  (N.LT.3)                      GOTO 999
      IF  (NCOTPC .GT. 21)              GOTO 999
      IF  (NCOITC .GT. MXLRIT)          GOTO 999
C
C---> Input data
C
      J = 0
C
C---> TPC coordinates
C
      KTPCO=IW(NTPCO)
      IF ( KTPCO .GT. 0 ) THEN
        DO 10 I = 1, NCOTPC
          J = J + 1
          KSTRT     = KROW(KTPCO,LISTPC(NCOTPC-I+1))
          RF(J)     = RW(KSTRT+JTPCRV)
          UF(J)     = RW(KSTRT+JTPCPH) * RF(J)
          ZF(J)     = RW(KSTRT+JTPCZV)
          SIGU(J)   = RW(KSTRT+JTPCSR)
          SIGZ(J)   = RW(KSTRT+JTPCSZ)
          CORUZ(J)  = 0.
   10   CONTINUE
      ENDIF
C
C---> ITC coordinates
C
      KICCO=IW(NICCO)
      IF ( KICCO .GT. 0 ) THEN
        DO  20  I = 1, NCOITC
          J = J + 1
          KSTRT     = KROW(KICCO,IABS(LISITC(I)))
          RF(J)     = RW(KSTRT+JICCRV)
          UF(J)     = RW(KSTRT+JICCPH) * RF(J)
          ZF(J)     = RW(KSTRT+JICCZV)
          SIGU(J)   = RW(KSTRT+JICCSR)
          SIGZ(J)   = RW(KSTRT+JICCSZ)
          CORUZ(J)  = 0.
   20   CONTINUE
      ENDIF
C
C---> Call Kalman filter
        KVTMA = IW(NVTMA)
C
C---> Here we loop over all VDET cluster sets found
C
        ISAVD = 1
        ISBVD = 0
        IBEST = 0
        JSTRT = J
        CBEST = 0.
C
C
C-- first call the KALMAN fitter just for the ITC+TPC part
C
        CALL VDMSUP(ITRAC,0)
        IRET = UFVDIN(0,ISAVD,ISBVD)
        IRET = UFTKAL(FIELD,
     +            J, RF, UF, ZF, SIGU, SIGZ, CORUZ, VV0IN, CHI2IN,
     +            VV0, CC0, CHI2, NDEG )
        IRET = UFVDOU(NVDOU,CBEST)
        CITPC = CBEST
        ISBVD = 1
C
C-- now loop over all clusters combinaisons found for one tracks
C
        DO 50 ICOMB = 1, LROWS(KVTMA)
          J = JSTRT
          NULINK  = 0
          NWLINK  = 0
C
          JVTMA = KROW(KVTMA,ICOMB)
          DO 40 IL=1,IW(JVTMA+JVTMNL)
            IF(J.GE.MPT) GO TO 40
            SIGMU = RW(JVTMA+JVTMSU+IL-1)
            SIGMZ = RW(JVTMA+JVTMSW+IL-1)
            IF(SIGMU.GT.HBIGER .AND. SIGMZ.GT.HBIGER)  GO TO 40
            IF(SIGMU.LT.HBIGER) NULINK = NULINK+1
            IF(SIGMZ.LT.HBIGER) NWLINK = NWLINK+1
C
C  We may check here that the hits are still not used
C
            J = J+1
            RF(J)     = RW(JVTMA+JVTMR0+IL-1)
            UF(J)     = RW(JVTMA+JVTMPH+IL-1) * RF(J)
            ZF(J)     = RW(JVTMA+JVTMZ0+IL-1)
            SIGU(J)   = SIGMU
            SIGZ(J)   = SIGMZ
            CORUZ(J)  = RW(JVTMA+JVTMCO+IL-1)
 40       CONTINUE

C
C-- compute an upper value of CI2VD
C
          CI2VD = CITPC-(NULINK+NWLINK)*C2PRCL
C
C-- Save nonsense for real chisquare (if it is not overwritten,
C    it means UFTKAL was not called for this VTMA pattern)
          RW(JVTMA+JVTMC2)=-999.
C
C-- Save augmented chisquare for later ambiguity check
          RW(JVTMA+JVTMFR)=CI2VD
C
C-- Don't spend time in UFTKAL if this pattern could not be better
C    than present best one (because it uses fewer hits)
C
          IF (CI2VD.GE.CBEST) GO TO 50
C
C-- call Kalman filter
C
          NVDIN = J - JSTRT
          IRET = UFVDIN(NVDIN,ISAVD,ISBVD)
          CALL VDMSUP(ITRAC,ICOMB)
          IRET = UFTKAL(FIELD,
     +            J, RF, UF, ZF, SIGU, SIGZ, CORUZ, VV0IN, CHI2IN,
     +            VV0, CC0, CHI2, NDEG )
          IRET = UFVDOU(NVDOU,CI2VD)
          IF(NULINK+NWLINK.NE.NVDOU) GO TO 50
C
C-- Save real UFTKAL chisquare
          RW(JVTMA+JVTMC2)=CI2VD
C-- Calculate augmented chisquare
          CI2VD = CI2VD-MIN(NULINK+NWLINK,NVDOU)*C2PRCL
C-- Save augmented chisquare for later ambiguity check
          RW(JVTMA+JVTMFR)=CI2VD
C-- Update CBEST and pointer if this pattern is better
          IF (CI2VD.GE.CBEST) GO TO 50
          CBEST = CI2VD
          IBEST = ICOMB
 50     CONTINUE
        NULINK = 0
        NWLINK = 0
        IF(IBEST .EQ. 0)              GO TO 999
C
C-- final refit
C
        JVTMA = KROW(KVTMA,IBEST)
        ISAVD = 0
        J = JSTRT
        DO 60 IL=1,IW(JVTMA+JVTMNL)
          IF(J.GE.MPT) GO TO 60
          SIGMU = RW(JVTMA+JVTMSU+IL-1)
          SIGMZ = RW(JVTMA+JVTMSW+IL-1)
          IF(SIGMU.GT.HBIGER .AND. SIGMZ.GT.HBIGER)  GO TO 60
          IF(SIGMU.LT.HBIGER) NULINK = NULINK+1
          IF(SIGMZ.LT.HBIGER) NWLINK = NWLINK+1
          J = J+1
          RF(J)     = RW(JVTMA+JVTMR0+IL-1)
          UF(J)     = RW(JVTMA+JVTMPH+IL-1) * RF(J)
          ZF(J)     = RW(JVTMA+JVTMZ0+IL-1)
          SIGU(J)   = SIGMU
          SIGZ(J)   = SIGMZ
          CORUZ(J)  = RW(JVTMA+JVTMCO+IL-1)
 60       CONTINUE
C
        NVDIN = J - JSTRT
        IRET = UFVDIN(NVDIN,ISAVD,ISBVD)
        CALL VDMSUP(ITRAC,IBEST)
        IF (TRACE) IRET = UFTRAC(.TRUE.)
        IRET = UFTKAL(FIELD,
     +            J, RF, UF, ZF, SIGU, SIGZ, CORUZ, VV0IN, CHI2IN,
     +            VV0, CC0, CHI2, NDEG )
        IF ( CHI2 .GE. 1.E10 )     GOTO 999
        IRET = UFVDOU(NVDOU,CI2VD)
        CI2VD = CI2VD-(NULINK+NWLINK)*C2PRCL
        IF(CI2VD.GT.CITPC)         GO TO 999
C
C-- fill VDET banks
C
        CALL VTFILL (ITRAC,IBEST)
C
      ISAVD = 0
      ISBVD = 0
      IRET = UFVDIN(0,ISAVD,ISBVD)
C     restore coordinate filtering
      CALL UFTFIL(FILT_PROB)
      RETURN
C
999   CONTINUE
      ISAVD = 0
      ISBVD = 0
      IRET = UFVDIN(0,0,0)
C
C---> Fit failed . . . leave old parameters
C
      CALL UCOPY( VV0IN, VV0, 5 )
      VV0(6) = 0.
      CHI2 = 1.0E30
C     restore coordinate filtering
      CALL UFTFIL(FILT_PROB)
      RETURN
      END
#endif
