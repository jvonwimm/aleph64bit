      SUBROUTINE FIT_DMIN(ITYPE,NVIEW,OUTPAR,OUTERR,CHISQ)
CKEY   QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------
C! Fit the dmin/sig spectrum to the standard function
C  Called from BTAG_FIT
C  Author  Dave Brown  29-1-93
C  Modified Ian Tomalin 7-7-95 Limits changed for simple fit to
C                              prevent crashing.
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
C
C  Input variables; Track type and number of views
C
      INTEGER ITYPE,NVIEW
C
C  Output variables
C
      REAL OUTPAR(5),OUTERR(5),CHISQ
C
C  Fit variables.    Notice that we now constrain the
C  fit amplitude to the number of entries in the plot; this eliminates
C  1 variable from the fit, and also eliminates a strong correlation
C  between the first 4 variables, thus making the fit more robust.
C
      INTEGER NFIT
      PARAMETER(NFIT=5)
      REAL STEP(NFIT)
      REAL PMIN(NFIT),PMAX(NFIT),PMINS(NFIT),PMAXS(NFIT),PERR(NFIT)
      REAL FITP(NFIT)
      COMMON/DMIN_FITP/FITP,SUM
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
      INTEGER NOENT,IPAR,IERFLG,MFIT,ID
      REAL SUM
      REAL*8 DUMMY(10)
      CHARACTER*8 CHOPT
      LOGICAL FIRST_ZERO
      DATA STEP/2*5.,3*.001/
      DATA PMIN/2*0.,3*.25/
      DATA PMAX/2*0.,3*10./
      DATA PMINS/0.,2*.25,2*0./
      DATA PMAXS/0.,2*10.,2*0./
      DATA CHOPT/'R'/
C  logical for new code
      LOGICAL NEWCOD
C
C  Inline functions
C
      INTEGER ITYP
      LOGICAL SIMPLE
      SIMPLE(ITYP) = (NOENT.LT.10000).OR.ITYP.EQ.7
C ------------------------------------------------------------------------------
C see which version of the code is to be used,
C John's modifications or standard?
C
      CALL CODSEL(NEWCOD)
      IF (NEWCOD) THEN
        WRITE (IW(6),*)'FITDMIN: improved QIPBTAG fitting procedure'
        CALL FIT_DMIN96(ITYPE,NVIEW,OUTPAR,OUTERR,CHISQ)
        RETURN
      ENDIF
C  Normalize to the number of entries.  The 4-pi is just the
C  phase space factor for having integrated over angles in 3 dimensions
C  (dmin is essentially a radius).
C
      ID = 150+ITYPE
      CALL HNOENT(ID,NOENT)
      IF(NOENT .LE. 100)THEN
        WRITE (IW(6),*)' Not enough entries for histogram ',ID
        RETURN
      END IF
      SUM = NOENT*.08
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
      IF(.NOT.SIMPLE(ITYPE))THEN
        MFIT = NFIT
        DO IPAR=1,2
          FITP(IPAR) = OUTPAR(IPAR)*SUM
        END DO
        DO IPAR=3,5
          FITP(IPAR) = OUTPAR(IPAR)
        END DO
        IF(NVIEW.EQ.2)THEN
          CALL HFITH(ID,DMINF,CHOPT,MFIT,FITP,STEP,PMIN,PMAX,
     &         PERR,CHISQ)
        ELSE
          CALL HFITH(ID,DMINF2,CHOPT,MFIT,FITP,STEP,PMIN,PMAX,
     &         PERR,CHISQ)
        END IF
        DO IPAR=1,2
          OUTPAR(IPAR) = FITP(IPAR)/SUM
        END DO
        DO IPAR=3,5
          OUTPAR(IPAR) = FITP(IPAR)
        END DO
      ELSE
        MFIT = NFIT-2
        FITP(1) = OUTPAR(1)*SUM
        FITP(2) = OUTPAR(3)
        FITP(3) = OUTPAR(4)
        IF(NVIEW.EQ.2)THEN
          CALL HFITH(ID,DMINS,CHOPT,MFIT,FITP,STEP,PMINS,PMAXS,
     &         PERR,CHISQ)
        ELSE
          CALL HFITH(ID,DMINS2,CHOPT,MFIT,FITP,STEP,PMINS,PMAXS,
     &         PERR,CHISQ)
        END IF
        OUTPAR(1) = FITP(1)/SUM
        OUTPAR(2) = 0.0
        OUTPAR(3) = FITP(2)
        OUTPAR(4) = FITP(3)
        OUTPAR(5) = 1.0
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
      COMMON/DMIN_FITP/FITP,SUM
      REAL FITP(5),SUM
      REAL X
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = XXXX*EXP(-(XXXX/FITP(3))**2/2.)/FITP(3)**2
      P2(XXXX) = XXXX*EXP(-XXXX/FITP(4))/FITP(4)**2
      P3(XXXX) = XXXX*EXP(-XXXX/FITP(5))/FITP(5)**2
      RESOL(XXXX) = (SUM-FITP(1)-FITP(2))*P1(XXXX)+
     &  FITP(1)*P2(XXXX)+FITP(2)*P3(XXXX)
C
      XXXX = X
      DMINF = RESOL(XXXX)
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
      COMMON/DMIN_FITP/FITP,SUM
      REAL FITP(5),SUM
      REAL X
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = XXXX*EXP(-(XXXX/FITP(2))**2/2.)/FITP(2)**2
      P2(XXXX) = XXXX*EXP(-XXXX/FITP(3))/FITP(3)**2
      RESOL(XXXX) = (SUM-FITP(1))*P1(XXXX)+FITP(1)*P2(XXXX)
C
      XXXX = X
      DMINS = RESOL(XXXX)
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
      COMMON/DMIN_FITP/FITP,SUM
      REAL FITP(5),SUM
      REAL X
      REAL R2OVPI/0.79788456/
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = R2OVPI*EXP(-(XXXX/FITP(3))**2/2.)/FITP(3)
      P2(XXXX) = EXP(-XXXX/FITP(4))/FITP(4)
      P3(XXXX) = EXP(-XXXX/FITP(5))/FITP(5)
      RESOL(XXXX) = (SUM-FITP(1)-FITP(2))*P1(XXXX)+
     &  FITP(1)*P2(XXXX)+FITP(2)*P3(XXXX)
C
      XXXX = X
      DMINF2 = RESOL(XXXX)
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
      COMMON/DMIN_FITP/FITP,SUM
      REAL FITP(5),SUM
      REAL X
      REAL R2OVPI/0.79788456/
C
C  Inlines
C  Normalized functions used to fit the negative dmin/sig spectrum.
C
      P1(XXXX) = R2OVPI*EXP(-(XXXX/FITP(2))**2/2.)/FITP(2)
      P2(XXXX) = EXP(-XXXX/FITP(3))/FITP(3)
      RESOL(XXXX) = (SUM-FITP(1))*P1(XXXX)+FITP(1)*P2(XXXX)
C
      XXXX = X
      DMINS2 = RESOL(XXXX)
      RETURN
      END
#endif
