      SUBROUTINE BTAG_FIT(FILE)
CKEY   QIPBTAG / USER
C-----------------------------------------------------------------------
C! Fit the negative dmin/sig spectrum to determine the track resolution,
C  pack the parameters into the appropriate banks, and write them out.
C  May be called by the user at the end of his job
C  Author  Dave Brown  29-1-93
C
C     Input;  FILE   File name of the card file to be created.  If
C                    this is blank or equal to "NONE", the card file
C                    will appear on unit 32
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
#include "btpar.h"
      INTEGER IND,IPAR
      INTEGER ILOW,IHIGH
      INTEGER ITYPE
      CHARACTER*(*) FILE
      REAL ERR(NFIT),CHI
      INTEGER NOENT
      INTEGER NBANK
      REAL FITPAR(NFIT,MAXTYPE)
#include "bmacrod.h"
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C  Check that calibration was requested.
      IF (.NOT.CALIB) GOTO 999
C
C  Copy the existing parameters as initial values
C
      DO ITYPE=1,MAXTYPE
        DO IPAR=1,NFIT
          FITPAR(IPAR,ITYPE) = FITP(IPAR,ITYPE)
        END DO
      END DO
C
C     Limit the fit range, so as to model the tail better
C
      WRITE (IW(6),*)'BTAG_FIT: Calibration fit results'
      DO ITYPE=1,NTYPE
        CALL FIT_DMIN(ITYPE,NVIEW(ITYPE),FITPAR(1,ITYPE),ERR,CHI,NOENT)
        IF (NOENT.GT.0) WRITE (IW(6),10) ITYPE,NOENT,CHI
   10   FORMAT(' For tracks of type ',I2,' histogram has ',I7,
     &         ' entries and chisq/ndf of fit = ',f12.5)
      END DO
C
C  Save the values in a card file
C
      IND = NBANK('FITP',1,LMHLEN + MAXTYPE*NFIT)
      IF(IND .LE. 0 )THEN
        WRITE (IW(6),*)'BTAG_FIT: Not enough space to create FITP bank'
        RETURN
      END IF
      IW(IND+1) = NFIT
      IW(IND+2) = MAXTYPE
      DO ITYPE=1,MAXTYPE
        DO IPAR=1,NFIT
          RW(KROW(IND,ITYPE)+IPAR) = FITPAR(IPAR,ITYPE)
        END DO
      END DO
      CALL BKFMT('FITP','2I,(5F)')
C
C  Write out the bank as a cards
C
      IF(FILE(1:4) .NE. 'NONE' .AND. FILE(1:4) .NE. 'none')THEN
        WRITE (IW(6),*)' Saving BTAG calibration on file ',FILE
        OPEN(UNIT=32,STATUS='UNKNOWN',FORM='FORMATTED',FILE=FILE)
      ELSE
        WRITE (IW(6),*)' Saving BTAG calibration on unit 32'
      END IF
      CALL BUNIT(32,'TEXT',72)
      CALL BLIST(IW,'E=','FITP')
      CALL BWRITE(IW,32,'E')
      CALL BWRITE(IW,32,'0')
      CLOSE(32)

  999 CONTINUE
      RETURN
      END
#endif
