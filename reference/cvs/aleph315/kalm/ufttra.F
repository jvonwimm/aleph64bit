      INTEGER FUNCTION UFTTRA(ITRAC,FIELD,
     +                  VV0IN,CHI2IN,NCOTPC,NCOITC,NCOVD,
     +                  LISTPC,LISITC,LISVD,VV0,CC0,CHI2,NDEG)
C----------------------------------------------------------------
C! FITTING ROUTINE FOR HELICES IN ALEPH
C!              ===>  INCLUDING MULTIPLE SCATTERING
C!              ===>  BASED ON EXTENDED KALMAN FILTERING
C!              ===>  CALLS UFTKAL
C!
C! This is to be used for the final fit of ALEPH tracks,
C! including TPC, ITC and VDET.  The preliminary fits are made
C! by another program, UFITMS, which is a few times faster but
C! does not handle the errors and covariance matrices strictly
C! correctly.  This routine takes into account correlations
C! between all measuring planes which result from multiple
C! scattering.
C!
CKEY COMPUTE FIT
C!
C!    AUTHOR:   T. LOHSE     16-10-89
C!    Modified: R. Johnson, W. Atwood, to improve use of ITC
C!                                     coordinates in the fit.
C!    Modified: J.Sedgbeer 15/01/90 Replace IFCO bank with ICCO
C!    Modified: B.Mours 11/03/91 Add VDET pattern recon. logic
C!    Modified: TSM     18/09/91 track ambiguity
C!    Modified: B.Mours 13/02/92 add chi2 cut on final fit
C!    Modified: G.Taylor 29/9/92 add itrac arguement and change
C!                               name UFTTRK -> UFTTRA
C!                               change treatment of multiple
C!                               scattering in VDET
C!    Modified: D.Casper 08/2/96 Remove old VDET patrec code and
C!                               pass KF error code as function
C!                               value.
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
C!            NCOVD  = NUMBER OF VD  COORDINATES
C!            LISTPC = COORDINATE NUMBERS IN TPCO
C!            LISITC = COORDINATE NUMBERS IN ICCO
C!            LISVD  = COORDINATE NUMBERS IN VDCO
C!
C!  OUTPUT:   VV0 = 6 FINAL TRACK PARAMETERS
C!              1 : 1/R          [1/CM]  NEG. IF CLOCKWISE
C!              2 : TAN(LAMBDA)  =DZ/DS} TAN(ANGLE TO X,Y PLANE)
C!              3 : PHI0        [0,2PI]} ANGLE TO X-AXIS
C!              4 : D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,
C!                                       sign OF ANGULAR MOM. LZ
C!              5 : Z0           [CM]    Z POS AT R=D0
C!              6 : ALPHA      [-PI,PI]: SCATTERING ANGLE
C!                                       AT ITC WALL (IN X-Y)
C!            CC0 = COVARIANCE MATRIX IN LOWER TRIANG. FORM
C!                     1
C!                     2  3
C!                     4  5  6
C!                     7  8  9 10
C!                    11 12 13 14 15
C!                    16 17 18 19 20 21
C!            CHI2= CHI SQUARED FROM LAST KALMAN FILTER STEP
C!            NDEG= NUMBER OF DEGREES OF FREEDOM
C!
C!  NOTE:     FIT DONE FOR THE 5 HELIX PARAMETERS ONLY.
C!            THE ANGLE ALPHA IS COMPUTED BUT ELEMENTS
C!            16,...,21 OF CC0 ARE DUMMY.
C!
C----------------------------------------------------------------
#ifndef DOC
#include "vtmajj.h"
      INTEGER  UFTKAL
      REAL     CI2VD
#include "vtkrec.h"
#include "vtrpar.h"
#include "alcons.h"
#include "bcs.h"
#include "tpcojj.h"
#include "iccojj.h"
#include "vdcojj.h"
C
      PARAMETER (MPT=40,MXLRIT=8)
      REAL   VV0IN(*)
      REAL   VV0(*),CC0(*)
      REAL   RF(MPT), UF(MPT), ZF(MPT), COSTHN(MXLRIT),ZLYR(MXLRIT),
     +       SIGU(MPT), SIGZ(MPT), CORUZ(MPT)
      INTEGER LISTPC(*), LISITC(*), LISVD(*)
      DATA NTPCO, NICCO, NVDCO, NVTMA /4*0/
C
#include "bmacro.h"
C
      IF (NTPCO.EQ.0) THEN
        NTPCO = NAMIND('TPCO')
        NICCO = NAMIND('ICCO')
        NVDCO = NAMIND('VDCO')
        NVTMA = NAMIND('VTMA')
      END IF
C
      UFTTRA = 100
      N = NCOTPC + NCOITC + MAX(0,NCOVD)
      IF  (N.GT.MPT)                    GOTO 999
      IF  (N.LT.3)                      GOTO 999
      IF  (NCOTPC .GT. 21)              GOTO 999
      IF  (NCOITC .GT. MXLRIT)          GOTO 999
      IF  (NCOVD  .GT. 4 )              GOTO 999
C
C---> Input data
C
      J = 0
C
C---> TPC coordinates
C
      KTPCO=IW(NTPCO)
      IF ( KTPCO .GT. 0) THEN
        DO 10 I = 1, NCOTPC
          J = J + 1
          KSTRT     = KROW(KTPCO,LISTPC(I))
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
          KSTRT     = KROW(KICCO,IABS(LISITC(I)))
          IF (RW(KSTRT+JICCRV).NE.0) THEN
            J = J + 1
            RF(J)     = RW(KSTRT+JICCRV)
            UF(J)     = RW(KSTRT+JICCPH) * RF(J)
            ZF(J)     = RW(KSTRT+JICCZV)
            SIGU(J)   = RW(KSTRT+JICCSR)
            SIGZ(J)   = RW(KSTRT+JICCSZ)
            CORUZ(J)  = 0.
          ENDIF
   20   CONTINUE
      ENDIF
C
C---> Get VDET coordinates
C
      KVDCO=IW(NVDCO)
      IF ( KVDCO .GT. 0 ) THEN
        DO  30  I = 1, NCOVD
          J = J + 1
          KSTRT     = KROW(KVDCO,LISVD(I))
          RF(J)     = RW(KSTRT+JVDCR0)
          UF(J)     = RW(KSTRT+JVDCPH) * RF(J)
          ZF(J)     = RW(KSTRT+JVDCZ0)
          SIGU(J)   = RW(KSTRT+JVDCSR)
          SIGZ(J)   = RW(KSTRT+JVDCSZ)
          CORUZ(J)  = 0.
   30   CONTINUE
      ENDIF
C
C---> Call Kalman filter
C
C  Trap ITC bug
      IF (J.LT.3) THEN
        UFTTRA = 200
        GO TO 999
      ENDIF
      CALL VDMSUP(ITRAC,0)
      UFTTRA = UFTKAL(FIELD,
     +         N, RF, UF, ZF, SIGU, SIGZ, CORUZ, VV0IN, CHI2IN,
     +            VV0, CC0, CHI2, NDEG )
      NVDFUL=0
      IF ( CHI2 .GE. 9.E29 .OR. UFTTRA.NE.0) GOTO 999
      RETURN
C
999   CONTINUE
C
C---> Fit failed . . . leave old parameters
C
      CALL UCOPY( VV0IN, VV0, 5 )
      VV0(6) = 0.
      CHI2 = 1.0E30
      RETURN
      END
#endif
