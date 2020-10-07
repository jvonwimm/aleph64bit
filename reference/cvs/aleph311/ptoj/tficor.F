      SUBROUTINE TFICOR(IEND,IROW,R,PHI,Z,RCR,PHICR,ZCR)
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for field distortions.
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
C!  Modified  :   I. Tomalin     94/06/29
C!                Allow several T3FC corrections to be applied in series
C!                to the same data.
C!                Extend T3FC bank to allow for corrections to r and z.
C!                For distortions which can't be well parameterized by
C!                T3FC alone (shown by JT3FIP column), call TFIMUL to
C!                multiply T3FC corrections by some additional function
C!                of phi, TPC current etc.
C!  Modified  :   W.Wiedenmann 971017
C!                Don't make corrections for new alignment 
C!                (already incorpoated in TNFC bank)
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
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "evehjj.h"
#include "bcs.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "t3fcjj.h"
#include "t3rrjj.h"
C
C++   Definitions
C
      PARAMETER (NDMAX=3,NCOMAX=JT3FCW-JT3FCO,MAXT3=4)
      DIMENSION COEFF(NCOMAX,2,MAXT3),IBASFT(NDMAX,NCOMAX,2,MAXT3)
      DIMENSION NT3S(2),ND(2,MAXT3),NCO(2,MAXT3),IPOLY(2,MAXT3),
     +          IFUNC(2,MAXT3),ICOCOR(2,MAXT3),ITOCOR(2,MAXT3),NUMT3(2)
      DIMENSION XCOR(3),XN(3)
      DOUBLE PRECISION COEFF,P,TFIPOL,SHIFT
      LOGICAL FIRST, FCORR
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C ---------------------------------------------------------------------
C - 1st entry
      IF (FIRST) THEN
        FIRST=.FALSE.
        NT3FC = NAMIND('T3FC')
        NT3RR = NAMIND('T3RR')
        NEVEH = NAMIND('EVEH')
        NTNLO = NAMIND('TNLO')
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
C - Don't make corrections for new alignment 
      IF (IW(NTNLO).GT.0) RETURN
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
 20      CONTINUE
C - run # IRUN is in the run range of row # JROW
         FCORR = .TRUE.
C
         IF (NBNK.NE.ITABL(JT3RR,JROW,JT3RBK)) THEN
C
C Determine number of corrections to be applied to Sides A and B.
C Ensure code is backwards compatible with old type of T3RR bank, which
C only had 5 columns.
             IF (ITABL(JT3RR,JROW,JT3RS1).EQ.1) THEN
               NT3S(1) = 1
               IF (LCOLS(JT3RR).EQ.LT3RRA)
     +           NT3S(1) = ITABL(JT3RR,JROW,JT3RN1)
             ELSE
               NT3S(1) = 0
             END IF
             IF (ITABL(JT3RR,JROW,JT3RS2).EQ.2) THEN
               NT3S(2) = 1
               IF (LCOLS(JT3RR).EQ.LT3RRA)
     +           NT3S(2) = ITABL(JT3RR,JROW,JT3RN2)
             ELSE
               NT3S(2) = 0
             END IF
C
C - Check that we are not overflowing the arrays.
             IF (NT3S(1).GT.MAXT3.OR.NT3S(2).GT.MAXT3) THEN
               WRITE(IW(6),22) IRUN,NT3S,MAXT3
 22            FORMAT(' FATAL TFICOR ERROR: Please increase array ',
     +         ' size. ',4I7)
                CALL EXIT
             END IF
C
             KT3FC = NDROP ('T3FC',NBNK)
             NBNK  = ITABL(JT3RR,JROW,JT3RBK)
             KT3FC = MDARD (IW,LDBAS,'T3FC',NBNK)
             IF (KT3FC.EQ.0) THEN
                WRITE (IW(6),25) NBNK,IRUN,IRUN1,IRUN2
 25             FORMAT(' TFICOR ERROR: Missing T3FC bank ',4I7)
                 CALL EXIT
             ELSE
                NUMT3(1) = 0
                NUMT3(2) = 0
                DO 30 IR=1,LROWS(KT3FC)
                   JT3FC          = KROW(KT3FC,IR)
                   IS             = IW(JT3FC + JT3FSI)
                   NUMT3(IS)      = NUMT3(IS) + 1
                   IT3            = NUMT3(IS)
                   ND(IS,IT3)     = IW(JT3FC + JT3FND)
                   NCO(IS,IT3)    = IW(JT3FC + JT3FNC)
                   IFPOL          = IW(JT3FC + JT3FIP)
                   IPOLY(IS,IT3)  = MOD(IFPOL,10)
                   IFUNC(IS,IT3)  = IFPOL/10
                   ICOCOR(IS,IT3) = IW(JT3FC + JT3FIO)
                   DO 31 IC=1,NCOMAX
                     COEFF(IC,IS,IT3) = RW(JT3FC + JT3FCO-1+IC)
                     DO 32 ID=1,NDMAX
                        IBASFT(ID,IC,IS,IT3) =
     >                      IW(JT3FC + JT3FIC-1+(ID-1)*NCOMAX+IC)
 32                  CONTINUE
 31                CONTINUE
C Note whether correction is to be applied to r,phi or z.
C Ensure backwards compatibility with old T3FC bank which only
C had 205 columns.
                   IF (LCOLS(KT3FC).EQ.LT3FCA) THEN
                     ITOCOR(IS,IT3) = IW(JT3FC + JT3FCW)
                   ELSE
                     ITOCOR(IS,IT3) = 2
                   END IF
 30             CONTINUE
C
                IF (NUMT3(1).LT.NT3S(1).OR.NUMT3(2).LT.NT3S(2)) THEN
                  WRITE (IW(6),35) NBNK,IRUN,NUMT3,NT3S
 35               FORMAT(' TFICOR ERROR: T3FC has wrong number of ',
     +            'rows ',6I7)
                  CALL PRTABL('T3RR',0)
                  CALL PRTABL('T3FC',NBNK)
                   CALL EXIT
                END IF
C
             ENDIF
         ENDIF
      ENDIF
C
C - normal entry   ===================================================
C
C++   Check if coordinate has to be corrected
C
      IF (.NOT.FCORR) RETURN
      IF (NT3S(IEND).EQ.0) RETURN
C
C++   Coordinate sequence for correction and use scaled coordinates
C
      XCOR(1) = R/RTPCMX
      XCOR(2) = PHI
      XCOR(3) = Z/ZTPCMX
C
C++   Loop over the T3FC corrections banks for this end of the TPC.
C++   (Normally only one).
C
      DO 500 IT3 = 1,NT3S(IEND)
        I1 = ICOCOR(IEND,IT3)/100
        I2 = (ICOCOR(IEND,IT3) - I1*100)/10
        I3 = ICOCOR(IEND,IT3) - I1*100 - I2*10
        XN(1) = XCOR(I1)
        XN(2) = XCOR(I2)
        XN(3) = XCOR(I3)
C
C++   Calculate Rphi correction
C++   (see also HRVAL in HBOOK lib for multidimensional regression)
C
        SHIFT=0.
        DO 100 K=1,NCO(IEND,IT3)
           P=1.
           DO 200 I=1,ND(IEND,IT3)
              NUM=IBASFT(I,K,IEND,IT3)/10
              ITYP=IBASFT(I,K,IEND,IT3) - NUM*10
              IF (NUM.NE.0) THEN
                 IF (ITYP.EQ.0) P=P*TFIPOL(IPOLY(IEND,IT3),NUM,XN(I))
              ENDIF
  200      CONTINUE
           SHIFT=SHIFT+COEFF(K,IEND,IT3)*P
  100   CONTINUE
C
C Multiply polynomial correction by addition function if requested.
        IECON = MOD(IFUNC(IEND,IT3),10)
        IF (IECON.EQ.1) THEN
C Function to make correction and its first derivative w.r.t. z
C zero at endcap. (Use if fitting TPC halves with independent
C polynomials).
          SHIFT = SHIFT*(1.0 - ABS(XCOR(3)))**2
        ELSE IF (IECON.EQ.2) THEN
C Function to make correction and its first derivative w.r.t. z
C zero at both endcaps. (Use if fitting TPC halves with same
C polynomial).
          SHIFT = SHIFT*(1.0 - XCOR(3)**2)**2
        END IF
C Function to dampen fitted polynomial at small polar angles where
C there was no data.
        IDAMP = MOD(IFUNC(IEND,IT3)/10,10)
        IF (IDAMP.EQ.1) THEN
          SHIFT = SHIFT*EXP(-ABS(XCOR(3))/XCOR(1))
        END IF
C
C See if T3FC correction needs to be multiplied by more complicated one.
        IMUL = IFUNC(IEND,IT3)/100
        SSHIFT = SNGL(SHIFT)
        IF (IMUL.GT.0) SHIFT = SHIFT*
     +                  TFIMUL(IRUN,NBNK,IMUL,IEND,IT3,IROW,XCOR,SSHIFT)
C
C++   Correct coordinates
C
        IF (ITOCOR(IEND,IT3).EQ.1) THEN
          RCR = RCR - SHIFT
        ELSE IF (ITOCOR(IEND,IT3).EQ.2) THEN
          PHICR = PHICR - SHIFT/R
        ELSE IF (ITOCOR(IEND,IT3).EQ.3) THEN
          ZCR = ZCR - SHIFT
        END IF
C
 500  CONTINUE
C
      IF (PHICR.GT.TWOPI) THEN
        PHICR=PHICR-TWOPI
      ELSEIF (PHICR.LT.0.) THEN
        PHICR=PHICR+TWOPI
      ENDIF
C
      RETURN
      END
C
#endif
