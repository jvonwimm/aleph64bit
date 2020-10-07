      SUBROUTINE QFREFIT
C
C-----------------------------------------------------------------------
C! Refit tracks and overwrite the old fit results
C
C  Author  :  R. Johnson   6-10-89
C  Modified:  W. Wiedenmann 6-4-92
C
C  Modified:  W. Wiedenmann 5-2-93
C 
C  Modified:  W. Wiedenmann 17-6-93
C
C
C  Banks needed:  coordinate banks TPCO, ITCO (and ICCO), and VDCO
C                 are needed (for the detectors which you fit).
C                 These generally are not present on the POT but
C                 must be filled by unpacking routines which
C                 access the POT banks and do what calculations
C                 are necessary to fill the coordinate banks.
C                 For TPC and ITC coordinates, you must add the
C                 options TP and IT to the UNPK data card.  See
C                 the ALPHA manual for more information on this
C                 card.  In addition, the track bank FRFT and the
C                 pointer banks FRTL, FTCL, FICL, FVCL are needed,
C                 which requires the FT option (present by default)
C                 in the UNPK card.
C
C  NOTE:   if anything goes wrong in the fit, such as a non-positive-
C          definite error matrix, then the old fit is retained and
C          NFIT is not incremented.
C
C----------------------------------------------------------------------
*IF .NOT.DOC
C
      Integer Ufttra  
C
#include "itcojj.h"
#include "vdcojj.h"
#include "tpcojj.h"
#include "qcde.h"
      INTEGER NMATCH(2)
C++----- Common for steering of QREFIT
      PARAMETER (RINOU=95.)
      COMMON/QFITCO/IFLTPC
C
      DIMENSION HP(7),COV(28),EE0(28),PN(3),CHIAC(32)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL FLOCK
C
      PARAMETER(NHMAXT=21)
      INTEGER ICTPC(NHMAXT)
C
#include "qmacro.h"
C
      NFIT=0
      IF (FIRST) THEN
        FIRST=.FALSE.
        NFRTL=NAMIND('FRTL')
        NFTCL=NAMIND('FTCL')
        NFICL=NAMIND('FICL')
        NITCO=NAMIND('ITCO')
        NICCO=NAMIND('ICCO')
        NTPCO=NAMIND('TPCO')
        NFVCL=NAMIND('FVCL')
        NVDCO=NAMIND('VDCO')
      ENDIF
      KFRTL=IW(NFRTL)
      KFRFT=NLINK('FRFT',0)
C++
C++--- Set the lock flag: if METHOD < 0 only tracks which are not
C++    locked will be refitted
C++
 
      IF (KFRFT.EQ.0 .OR. KFRTL.EQ.0) THEN
C        CALL QWMESE('QREFIT: FRFT and FRTL are not present.')
        RETURN
      ENDIF
      KITCO=IW(NICCO)
      KFICL=IW(NFICL)
      KFTCL=IW(NFTCL)
      NTK=LROWS(KFRFT)
      DO 500 JTK=1,NTK
        IOITC=ITABL(KFRTL,JTK,JFRTII)
        IOTPC=ITABL(KFRTL,JTK,JFRTIT)
        IOVDT=ITABL(KFRTL,JTK,JFRTIV)
        NTPC=ITABL(KFRTL,JTK,JFRTNT)
        NITC=ITABL(KFRTL,JTK,JFRTNI)
        NVDET=0
C
        NDOF=2*(NTPC+NVDET+NITC) -5
        IF (NDOF.LT.1) GO TO 500
C
C++         This is the KALMAN filter fitting routine with the
C++         improved VDET material description. The CHI2 returned
C++         is very large (10**30) if the fit fails.
C
          IERR=0
          IOPT=9
          NMULT=0
          ierfit = UFTTRA(JTK,QMFLD,RW(KROW(KFRFT,JTK)+JFRFIR),
     &                RW(KROW(KFRFT,JTK)+JFRFC2),
     &                NTPC,NITC,NVDET,
     &                IW(KFTCL+LMHLEN+IOTPC+1),
     &                IW(KFICL+LMHLEN+IOITC+1),
     &                IW(KFVCL+LMHLEN+IOVDT+1),
     &                HP,COV,CHI2,NDOF)
          IF (CHI2.GT.1.0E10) GO TO 500
          IF (HP(1).EQ.0.) GO TO 500
          IF (COV(1).LE.0.) GO TO 500
          IF (COV(3).LE.0.) GO TO 500
          IF (COV(6).LE.0.) GO TO 500
          IF (COV(10).LE.0.) GO TO 500
          IF (COV(15).LE.0.) GO TO 500
          IF (COV(21).LT.0.) GO TO 500
C
C Protection against very straight tracks.
          IF (ABS(HP(1)).LT.1.0E-7) HP(1) = SIGN(1.0E-7,HP(1))
C
C++     Overwrite the unpacked track bank
C
          DO 60 I=1,6
            RW(KROW(KFRFT,JTK)+JFRFIR-1+I)=HP(I)
   60     CONTINUE
          DO 70 I=1,21
            RW(KROW(KFRFT,JTK)+JFRFEM-1+I)=COV(I)
   70     CONTINUE
          RW(KROW(KFRFT,JTK)+JFRFC2)=CHI2
          IW(KROW(KFRFT,JTK)+JFRFDF)=NDOF
          IW(KROW(KFRFT,JTK)+JFRFNO)=NMULT*100+IOPT*10+IERR
          NFIT=NFIT+1
  500 CONTINUE
C
C++   Redo Vdet if needed
      IF(IW(NAMIND('FRF0')) .eq. 0)THEN
         CALL VTRKEX(QMFLD,IERR)
         CALL VTRFIT(1,QMFLD,IERR,IGARB,NMATCH)
      ENDIF
      RETURN
      END









