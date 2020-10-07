      SUBROUTINE SPREMC(IER)
C----------------------------------------------------------------------
C! Preprocess MC events
C!
C!    Author:     H. Meinhard       04-Sep-1987
C!    Modified:   H. Meinhard       28-Jun-1989 (3)
C!
C!    Output:     - IER       /I    error flag (.ne. 0 on error)
C!
C!    Description
C!    ===========
C!    This routine should be called if there are unprocessed raw data
C!    (bank SRAD or SADI), but no preprocessed data (bank SRTD). This
C!    will usually be the case for events from the GALEPH Monte Carlo.
C!    Create SRTD using pedestals and slopes given by DATA statement
C!    below.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "sgeomc.h"
#include "srtdjj.h"
      DATA PEDST/-500./, SLOPE/.5E-9/
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C look whether bank SRTD exists; if yes, return
      IF (IW(NASRTD) .NE. 0)                               GOTO 999
C
C link to raw data; NDAT is the number of raw data
C bank SRAD found: raw data bank without miniheader
      IF (IW(NASRAD) .NE. 0) THEN
        KSRAD = IW(NASRAD)
        NDAT = IW(KSRAD)
C
C bank SADI found: raw data bank with miniheader
      ELSE IF(IW(NASADI) .NE. 0) THEN
        KSRAD = IW(NASADI) + LMHLEN
        NDAT = LROWS(IW(NASADI))
C
C neither bank SRAD nor SADI exists; return with error stat.
      ELSE
                                                            GOTO 901
      ENDIF
C
C create bank for preprocessed raw data
      CALL AUBOS('SRTD',0,LMHLEN+NDAT*LSRTDA,KSRTD,IGARB)
      IF (KSRTD .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) THEN
        IF (IW(NASRAD) .NE. 0) THEN
          KSRAD = IW(NASRAD)
        ELSE
          KSRAD = IW(NASADI) + LMHLEN
        ENDIF
        KSRTD = IW(NASRTD)
      ENDIF
      IW(KSRTD+LMHCOL) = LSRTDA
      IW(KSRTD+LMHROW) = NDAT
C
C loop over data
      DO 300 IDAT = 1, NDAT
        IWORD = IW(KSRAD+IDAT)
        ITDCC = JTDCC(IWORD)
        TIME  = (FLOAT(ITDCC) - PEDST) * SLOPE
        IW(KROW(KSRTD,IDAT)+JSRTIR) = IWORD
        IW(KROW(KSRTD,IDAT)+JSRTTR) = INT(TIME*1.E12)
  300 CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SPREMC',-IER,'No space for new bank SRTD')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
