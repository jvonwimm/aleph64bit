      SUBROUTINE FIT_DMIN(ITYPE,NUMVIEW,OUTPAR,OUTERR,CHISQ,NOENT)
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C! Fit the dmin/sig spectrum to the standard function
C  Called from BTAG_FIT
C  Author  Dave Brown  29-1-93
C  Modified Ian Tomalin 7-7-95: Limits changed for simple fit to
C                               prevent crashing.
C  Modified Ian Tomalin 8-3-99: Old fit_dmin96 code greatly streamlined
C                               and included directly here.
C  Modified P-F Giraud 17-8-99: New histogram numbering convention.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
C
C  Input variables; Track type and number of views
C
      INTEGER ITYPE,NUMVIEW
C
C  Output variables
C
      REAL OUTPAR(5),OUTERR(5),CHISQ
      INTEGER NOENT

C  Common with histogram offset.
#include "btpar.h"
C
C  Fit variables.    Notice that we now constrain the
C  fit amplitude to the number of entries in the plot; this eliminates
C  1 variable from the fit, and also eliminates a strong correlation
C  between the first 4 variables, thus making the fit more robust.
C
      REAL STEP(NFIT),STEPS(NFIT)
      REAL PMIN(NFIT),PMAX(NFIT),PMINS(NFIT),PMAXS(NFIT),PERR(NFIT)
      REAL FITPAR(NFIT),SUM
      COMMON/DMIN_FITPAR/FITPAR,SUM
      REAL DMINF,DMINS,HI
      EXTERNAL DMINF,DMINS,DMINF2,DMINS2,HI
C
C  HBOOK fit common
C
      INTEGER IQUEST(100)
      COMMON/QUEST/IQUEST
C
C  Local variables
C
      INTEGER IPAR,IERFLG,MFIT,ID
      REAL*8 DUMMY(10)
      CHARACTER*8 CHOPT
      LOGICAL FIRST_ZERO
      DATA STEP/2*.1,2*.1,.5/
      DATA PMIN/2*0.,3*.25/
      DATA PMAX/2*1.,3*10./
      DATA STEPS/.1,.1,.5,2*0./
      DATA PMINS/0.,2*.25,2*0./
      DATA PMAXS/1.,2*10.,2*0./
      DATA CHOPT/'BRV'/
C  logical for new code
      LOGICAL NEWCOD
C
C  Inline functions
C
      INTEGER ITYP
      LOGICAL SIMPLE_OLD,SIMPLE_NEW
C Old QIPBTAG track types
      SIMPLE_OLD(ITYP) = (NOENT.LT.10000).OR.ITYP.EQ.7
C New QIPBTAG track types.
C     SIMPLE_NEW(ITYP) = (NOENT.LT.10000).OR.ITYPE.EQ.3.OR.ITYPE.GT.5
      SIMPLE_NEW(ITYP) = (NOENT.LT.10000)
C ---------------------------------------------------------------------
C See which version of the code is used, John's modifications or 
C standard?
      CALL CODSEL(NEWCOD)
C
C  Normalize to the number of entries. 
C
      ID = ICHIS+ITYPE
      CALL HNOENT(ID,NOENT)
      IF(NOENT .LE. 100)THEN
        WRITE (IW(6),*)' Not enough entries for histogram ',ID
        RETURN
      END IF
      SUM = NOENT
C
C  Limit the fit range
C
      IQUEST(11) = 1
      IQUEST(12) = 10
      FIRST_ZERO = .TRUE.
      DO WHILE(FIRST_ZERO .OR. HI(ID,IQUEST(12)).GT.0.0)
        IF(HI(ID,IQUEST(12)).LE.0.0)FIRST_ZERO = .FALSE.
        IQUEST(12) = IQUEST(12) + 1
      END DO
C
C  For V0s and no-vdet tracks, use a simpler fit
C
      IF (     NEWCOD.AND.SIMPLE_NEW(ITYPE).OR.
     &    .NOT.NEWCOD.AND.SIMPLE_OLD(ITYPE))
     &THEN
        MFIT = NFIT-2
        FITPAR(1) = OUTPAR(1)
        FITPAR(2) = OUTPAR(3)
        FITPAR(3) = OUTPAR(4)
        IF(NUMVIEW.EQ.2)THEN
          CALL HFITH(ID,DMINS,CHOPT,MFIT,FITPAR,STEPS,PMINS,PMAXS,
     &         PERR,CHISQ)
        ELSE
          CALL HFITH(ID,DMINS2,CHOPT,MFIT,FITPAR,STEPS,PMINS,PMAXS,
     &         PERR,CHISQ)
        END IF
        OUTPAR(1) = FITPAR(1)
        OUTPAR(2) = 0.0
        OUTPAR(3) = FITPAR(2)
        OUTPAR(4) = FITPAR(3)
        OUTPAR(5) = 5.0
      ELSE
        MFIT = NFIT
        DO IPAR=1,5
          FITPAR(IPAR) = OUTPAR(IPAR)
        END DO
        IF(NUMVIEW.EQ.2)THEN
          CALL HFITH(ID,DMINF,CHOPT,MFIT,FITPAR,STEP,PMIN,PMAX,
     &         PERR,CHISQ)
        ELSE
          CALL HFITH(ID,DMINF2,CHOPT,MFIT,FITPAR,STEP,PMIN,PMAX,
     &         PERR,CHISQ)
        END IF
        DO IPAR=1,5
          OUTPAR(IPAR) = FITPAR(IPAR)
        END DO
      END IF
C
      RETURN
      END
C
C Fit function
C
      REAL FUNCTION DMINF(X)
C
      IMPLICIT NONE
      REAL*8 XXXX,P1,P2,P3
      REAL*8 RESOL
      COMMON/DMIN_FITPAR/FITPAR,SUM
      REAL FITPAR(5),SUM
      REAL X
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = XXXX*EXP(-(XXXX/FITPAR(3))**2/2.)/FITPAR(3)**2
      P2(XXXX) = XXXX*EXP(-XXXX/FITPAR(4))/FITPAR(4)**2
      P3(XXXX) = XXXX*EXP(-XXXX/FITPAR(5))/FITPAR(5)**2
      RESOL(XXXX) = (1.-FITPAR(1)-FITPAR(2))*P1(XXXX)+
     &  FITPAR(1)*P2(XXXX)+FITPAR(2)*P3(XXXX)
C
      XXXX = X
      DMINF = SUM*RESOL(XXXX)
      RETURN
      END
C
C  Low-statistics fit function
C
      REAL FUNCTION DMINS(X)
C
      IMPLICIT NONE
      REAL*8 XXXX,P1,P2
      REAL*8 RESOL
      COMMON/DMIN_FITPAR/FITPAR,SUM
      REAL FITPAR(5),SUM
      REAL X
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = XXXX*EXP(-(XXXX/FITPAR(2))**2/2.)/FITPAR(2)**2
      P2(XXXX) = XXXX*EXP(-XXXX/FITPAR(3))/FITPAR(3)**2
      RESOL(XXXX) = (1.-FITPAR(1))*P1(XXXX)+FITPAR(1)*P2(XXXX)
C
      XXXX = X
      DMINS = SUM*RESOL(XXXX)
      RETURN
      END
C
C  2-d fit
C
      REAL FUNCTION DMINF2(X)
C
      IMPLICIT NONE
      REAL*8 XXXX,P1,P2,P3
      REAL*8 RESOL
      COMMON/DMIN_FITPAR/FITPAR,SUM
      REAL FITPAR(5),SUM
      REAL X
      REAL R2OVPI/0.79788456/
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = R2OVPI*EXP(-(XXXX/FITPAR(3))**2/2.)/FITPAR(3)
      P2(XXXX) = EXP(-XXXX/FITPAR(4))/FITPAR(4)
      P3(XXXX) = EXP(-XXXX/FITPAR(5))/FITPAR(5)
      RESOL(XXXX) = (1.-FITPAR(1)-FITPAR(2))*P1(XXXX)+
     &  FITPAR(1)*P2(XXXX)+FITPAR(2)*P3(XXXX)
C
      XXXX = X
      DMINF2 = SUM*RESOL(XXXX)
      RETURN
      END
C
C  Low-statistics 2-d fit function
C
      REAL FUNCTION DMINS2(X)
C
      IMPLICIT NONE
      REAL*8 XXXX,P1,P2
      REAL*8 RESOL
      COMMON/DMIN_FITPAR/FITPAR,SUM
      REAL FITPAR(5),SUM
      REAL X
      REAL R2OVPI/0.79788456/
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = R2OVPI*EXP(-(XXXX/FITPAR(2))**2/2.)/FITPAR(2)
      P2(XXXX) = EXP(-XXXX/FITPAR(3))/FITPAR(3)
      RESOL(XXXX) = (1.-FITPAR(1))*P1(XXXX)+FITPAR(1)*P2(XXXX)
C
      XXXX = X
      DMINS2 = SUM*RESOL(XXXX)
      RETURN
      END
#endif
