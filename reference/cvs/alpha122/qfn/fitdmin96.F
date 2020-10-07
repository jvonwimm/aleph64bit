      SUBROUTINE FIT_DMIN96(ITYPE,NVIEW,OUTPAR,OUTERR,CHISQ)
CKEY   QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------

C! Fit the dmin/sig spectrum to the standard function
C  Called from BTAG_FIT
C  Author  Dave Brown  29-1-93
C ------------------------------------------------------------------------------

*IF .NOT.DOC
      IMPLICIT NONE
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
      REAL PMIN(NFIT),PMAX(NFIT), PERR(NFIT)
C=========== manoj's modification
      REAL PMIN2(3), PMAX2(3), STEP2(3)
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
C  FUNCTIONS
C
      REAL HSUM
C
C  Local variables
C
      INTEGER NOENT,IPAR,IERFLG,MFIT,ID
      REAL SUM, AREA, XBIN, XHI, XLO
      REAL*8 DUMMY(10)
      CHARACTER*8 CHOPT
      LOGICAL FIRST_ZERO
      DATA STEP/2*.005,.001,2*0.0/
      DATA STEP2/.005,.001,0.0/
      DATA PMIN/2*0.,3*.25/
      DATA PMIN2/0.0, 2*0.25/
      DATA PMAX/2*10000.,3*30./
      DATA PMAX2/10000.0, 2*30.0/
C      DATA CHOPT/'R'/
C
C  Inline functions
C
      INTEGER ITYP
      LOGICAL SIMPLE
      SIMPLE(ITYP) = (NOENT.LT.1000).OR.ITYPE.EQ.3.OR.ITYPE.GT.4
C ------------------------------------------------------------------------------

C  Normalize to the number of entries.
C
      ID = 150+ITYPE
C  THIS CALL IS WRONG BECAUSE NOENT INCLUDES OVER/UNDER FLOWS
C      CALL HNOENT(ID,NOENT)
      AREA = HSUM(ID)
      NOENT = INT(AREA)
      WRITE(*,*) ' ID, AREA ', ID,AREA
      IF(AREA .LE. 100.0)THEN
        WRITE(6,*)' Not enough entries for histogram ',ID
        RETURN
      END IF
C  GET THE BIN WIDTH
      CALL HIX(ID,2,XHI)
      CALL HIX(ID,1,XLO)
      XBIN = XHI-XLO
      SUM = AREA*XBIN
C
C  for some types use a simpler fit
C
      IF(.NOT.SIMPLE(ITYPE))THEN
        MFIT = NFIT
        DO IPAR=1,2
          FITP(IPAR) = OUTPAR(IPAR)*SUM
        END DO
        DO IPAR=3,5
          FITP(IPAR) = OUTPAR(IPAR)
        END DO
        FITP(4) = 1.  
        FITP(5) = 4.
        IF(NVIEW.EQ.2)THEN
C=========== manoj's modification
          CHOPT = 'BL'
C make sure that the initial parameters are within limits
          DO IPAR=1,MFIT
            IF (FITP(IPAR).GT.PMAX(IPAR)) FITP(IPAR)=PMAX(IPAR)/2.0
            IF (FITP(IPAR).LT.PMIN(IPAR)) FITP(IPAR)=PMIN(IPAR)*2.0
          ENDDO
          WRITE(*,*) ' FIT USING DMINF '
          CALL HFITH(ID,DMINF,CHOPT,MFIT,FITP,STEP,PMIN,PMAX,
     &         PERR,CHISQ)
        ELSE
          CHOPT = 'BL'
C make sure that the initial parameters are within limits
          DO IPAR=1,MFIT
            IF (FITP(IPAR).GT.PMAX(IPAR)) FITP(IPAR)=PMAX(IPAR)/2.0
            IF (FITP(IPAR).LT.PMIN(IPAR)) FITP(IPAR)=PMIN(IPAR)*2.0
          ENDDO
          WRITE(*,*) ' FIT USING DMINF2 '
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
        FITP(1) = 0.2*SUM
        FITP(2) = 1.0
        FITP(3) = 1.0
        IF(NVIEW.EQ.2)THEN
          CHOPT = 'L'
          WRITE(*,*) ' FIT USING DMINS '
          CALL HFITH(ID,DMINS,CHOPT,MFIT,FITP,STEP2,PMIN,PMAX,
     &         PERR,CHISQ)
        ELSE
          CHOPT = 'BL'
C make sure that the initial parameters are within limits
          DO IPAR=1,MFIT
            IF (FITP(IPAR).GT.PMAX(IPAR)) FITP(IPAR)=PMAX2(IPAR)/2.0
            IF (FITP(IPAR).LT.PMIN(IPAR)) FITP(IPAR)=PMIN2(IPAR)*2.0
          ENDDO
          WRITE(*,*) ' FIT USING DMINS2 '
          CALL HFITH(ID,DMINS2,CHOPT,MFIT,FITP,STEP2,PMIN2,PMAX2,
     &         PERR,CHISQ)
        END IF
        OUTPAR(1) = FITP(1)/SUM
        OUTPAR(2) = 0.0
        OUTPAR(3) = FITP(2)
        OUTPAR(4) = FITP(3)
        OUTPAR(5) = 4.0
      END IF
      write(*,*) itype,sum
      write(*,*) fitp
      write(*,*) outpar
C
      RETURN
      END
