      SUBROUTINE IINRES(IRUN,IRET)
C-----------------------------------------------------------------------
C! Set up ITC drift-time corrections and resolutions.
C!
CKEY ITCDES ITC /INTERNAL
C!   Author          :-  J. Sedgbeer  89/03/03
C!   Modified        :-  I. Tomalin   89/09/01
C!   Modified        :-  J. Sedgbeer  89/10/12  Get IEDD bank from Dbase
C!   Modified        :-  J. Sedgbeer  89/11/02  Get IET0 bank from Dbase
C!   Modified        :-  J. Sedgbeer  90/01/04  Get IZRS bank from Dbase
C!   Modified        :-  J. Sedgbeer  91/10/24  Get IRRF bank from Dbase
C!   Modified        :-  J. Sedgbeer  92/02/04 Implement run-period
C!                       scheme for some dbase banks.
C!   Modified        :-  J. Sedgbeer  93/07/01 New Drift-time params.
C!                       from bank IDRP for 1993 onwards (run > 20000)
C!   Modified        :-  J. Sedgbeer  98/08/15 New Drift-time params.
C!                       (Spline) from IDSP bank for 1997 data onwards 
C!                       (run > 43000).   NB: MC will still use IDRP.
C!
C!   Input:
C!    IRUN    /I : Current run number
C!    params.:    IDTCJJ  for IDTC bank
C!                IDRPJJ  for IDRP
C!                IRESJJ  for IRES
C!                IZRSJJ  for IZRS
C!    commons:    /BCS/   for banks  IDTC,IDRP,IRES,IRRF,IZRS,IEDD,IET0
C!
C!   Output:
C!    IRET    /I : Error flag: (as for AGETDB)
C!                   IRET > 0  Existing values still valid
C!                   IRET = 0  Error. One or more banks missing for this
C!                             run - leave values unchanged or, if first
C!                             time use default values.
C!                   IRET < 0  1 or more banks reloaded
C!    commons:    /IDTCCC/  Drift-time corrections - old parametrisation.
C!                /IDRPCC/  Drift-time corrections - new parametrisation.
C!                /IRESCC/  Resolution vs. cell position coeffs.
C!                /IZRSCC/  Z Resolution coeffs.
C!                /IRESOL/  Mean DC resolution common.
C!
C!   calls     : AGETDB (Alephlib)
C!               GTSTUP (Alephlib)
C!
C!   libraries:  BOS
C!
C!   Description:
C! Set up ITC drift-relation constants.
C! Get data from a direct access file ( filled from D.B.) or from
C! data cards or via bank input wth data.
C! If no valid bank then use values already loaded into common or,
C! if first time, use default values.
C!
C? If data (run number > 2000) then
C?   run period = run number
C? else (MC)
C?   get run period from function GTSTUP
C?   if no set-up number found set run period = run number
C? endif
C?
C? If 1989-1992 data ( 2000 < run number < 20000) then
C?   Check validity of IDTC bank - AGETDB
C?   If (first and no IDTC bank) then fill /IDTCCC/ and /IDRPCC/ with
C?        default values. Note that /IDTCCC/ is kept for backwards compat.
C?   If (first and existing IDTC bank still valid) or (new IDTC bank) then
C?      get values from IDTC bank. Fill /IDTCCC/ and /IDRPCC/
C?   Endif
C? ElseIf (Monte Carlo or 1993-1996 data;  runs 20000-43000 or < 2000)
C?   Check validity of IDRP bank - AGETDB
C?   If (first and no IDRP bank) then fill /IDTCCC/ and /IDRPCC/ with
C?                                default values
C?   If (first and existing IDRP bank still valid) or (new IDRP bank) then
C?      get values from IDRP bank. Fill /IDTCCC/ and /IDRPCC/
C?   Endif
C? Else (1997 data onwards;  runs > 43000 )
C?   Check validity of IDSP bank - AGETDB
C?   If (first and no IDSP bank) then fill /IDRPCC/ with
C?                                default values
C?   If (first and existing IDSP bank still valid) or (new IDSP bank) then
C?      get values from IDSP bank. Fill /IDRPCC/
C?   Endif
C? Endif
C?
C? Check validity of IRES bank - AGETDB
C? If (first and no IRES bank) then fill /IRESCC/ and /IRESOL/ with
C?    default values
C? If (first and existing IRES bank still valid) or (new IRES bank) then
C?    get values from IRES bank. Fill /IRESCC/ and /IRESOL/
C? Endif
C?
C? Check validity of IZRS bank - AGETDB
C? If (first and no IZRS bank) then fill /IZRSCC/ and /IRESOL/ with
C?    default values
C? If (first and existing IZRS bank still valid) or (new IZRS bank) then
C?    get values from IZRS bank. Fill /IZRSCC/ and /IRESOL/
C? Endif
C?
C? Check validity of IEDD bank - AGETDB
C? Check validity of IET0 bank - AGETDB
C? Check validity of IRRF bank - AGETDB
C?
C? Set return flag
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C commons etc.
#include "bcs.h"
#include "idtcjj.h"
#include "idrpjj.h"
#include "idspjj.h"
#include "iresjj.h"
#include "izrsjj.h"
#include "idtccc.h"
#include "idrpcc.h"
#include "irescc.h"
#include "izrscc.h"
#include "iresol.h"
C-----------------------------------------------------------------------
      EXTERNAL AGETDB,NAMIND,GTSTUP
      INTEGER AGETDB,NAMIND,GTSTUP
      LOGICAL FIRST
      INTEGER IRET
      REAL VDRF,SDIF,SIGR(8),SIGZ(8),SPLI(5)
      DATA VDRF/0.0050/
      DATA SDIF/0.0080/,SIGR/8*0.0100/,SIGZ/8*3.0/
      DATA SPLI/ 120.0, 244.0, 1.5843, -1.3390, 0.4882/
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Set run-period for data/MC
C
      IF(IRUN.GT.2000) THEN
        IRUNP = IRUN
      ELSE
        IRUNP = GTSTUP('IT',IRUN)
C If no setup then just keep run number => pick up dbase bank number 1
        IF(IRUNP.EQ.-1) THEN
          IRUNP = IRUN
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C If 1989-1992 data use IDTC bank (polynom.)
C
      IF (IRUN.GT.2000 .AND. IRUN.LE.20000) THEN
C
C Check for validity of IDTC bank.
C
        IRETD = AGETDB('IDTC',IRUNP)
C
C If first call and no bank for this run then fill /IDTCCC/ and /IDRPCC/
C with default values for time offsets and polynom. coeffs (no spline).
C
        IF(FIRST.AND.IRETD.EQ.0) THEN
          DO 10 I=1,MLAYID
            TOFFID(I) = 0.0
            TTOFID(I) = 0.0
            DCOFID(1,I) = VDRF
            DTCOID(1,I) = VDRF
            DO 5  J=2,MCOFID
              IF(J.LE.JCOFID) DCOFID(J,I) = 0.0
              DTCOID(J,I) = 0.0
    5       CONTINUE
   10     CONTINUE
        ENDIF
C
C Get values from new IDTC bank. Fill /IDTCCC/ and /IDRPCC/ with polynom.
C
        IF((FIRST.AND.IRETD.GT.0).OR.(IRETD.LT.0)) THEN
          KIDTC = IW(NAMIND('IDTC'))
          DO 20 I=1,MLAYID
            TOFFID(I) = RTABL(KIDTC,I,JIDTTO)
            TTOFID(I) = TOFFID(I)
            DO 15 J=1,MCOFID
              IF(J.LE.JCOFID) THEN
                DCOFID(J,I) = RTABL(KIDTC,I,JIDTCO-1+J)
                DTCOID(J,I) = DCOFID(J,I)
              ELSE
                DTCOID(J,I) = 0.0
              ENDIF
   15       CONTINUE
   20     CONTINUE
        ENDIF
C
C Set flag in /IDRPCC/ to use polynomial
C
        IFLGID = 0
C
C Elseif for Monte Carlo and 1993-1996 data use IDRP bank (polynom)
C
      ELSEIF ( (IRUN.LE.2000) .OR. 
     +         (IRUN.GT.20000.AND.IRUN.LE.43000) ) THEN
C
C Check for validity of IDRP bank.
C
        IRETD = AGETDB('IDRP',IRUNP)
C
C If first call and no bank for this run then fill /IDTCCC/ and /IDRPCC/
C with default values for time offset and polynom. coeffs. (no spline)
C
        IF(FIRST.AND.IRETD.EQ.0) THEN
          DO 30 I=1,MLAYID
            TOFFID(I) = 0.0
            TTOFID(I) = 0.0
            DCOFID(1,I) = VDRF
            DTCOID(1,I) = VDRF
            DO 25 J=2,MCOFID
              IF(J.LE.JCOFID) DCOFID(J,I) = 0.0
              DTCOID(J,I) = 0.0
   25       CONTINUE
   30     CONTINUE
        ENDIF
C
C Get values from new IDTC bank. Fill /IDTCCC/ and /IDRPCC/ with polynom.
C
        IF((FIRST.AND.IRETD.GT.0).OR.(IRETD.LT.0)) THEN
          KIDRP = IW(NAMIND('IDRP'))
          DO 40 I=1,MLAYID
            TOFFID(I) = RTABL(KIDRP,I,JIDRTO)
            TTOFID(I) = TOFFID(I)
            DO 35 J=1,MCOFID
              DTCOID(J,I) = RTABL(KIDRP,I,JIDRCO-1+J)
              IF(J.LE.JCOFID) DCOFID(J,I) = DTCOID(J,I)
   35       CONTINUE
   40     CONTINUE
        ENDIF
C
C Set flag in /IDRPCC/ to use polynomial
C
        IFLGID = 0
C
C Else for the rest, i.e 1996 data onwards, use IDSP bank (spline)
C
      ELSE 
C
C Check for validity of IDSP bank.
C
        IRETD = AGETDB('IDSP',IRUNP)
C
C If first call and no bank for this run then fill /IDRPCC/ 
C with default values for time offsets and spline coeffs. only
C
        IF(FIRST.AND.IRETD.EQ.0) THEN
          DO 42 I=1,MLAYID
            TTOFID(I) = 0.0
            DO 41 J=1,MSPLID
              DSPLID(J,I) = SPLI(J)
   41       CONTINUE
   42     CONTINUE
        ENDIF
C
C Get values from new IDSP bank.  Fill /IDRPCC/ with spline 
C
        IF((FIRST.AND.IRETD.GT.0).OR.(IRETD.LT.0)) THEN
          KIDSP = IW(NAMIND('IDSP'))
          DO 44 I=1,MLAYID
            TTOFID(I) = RTABL(KIDSP,I,JIDSTO)
            DO 43 J=1,MSPLID
              DSPLID(J,I) = RTABL(KIDSP,I,JIDSCO-1+J)
   43       CONTINUE
   44     CONTINUE
        ENDIF
C
C Set flag in /IDRPCC/ to use spline
C
        IFLGID = 1
C
      ENDIF
C
C-----------------------------------------------------------------------
C Check for validity of IRES bank.
C
      IRETR = AGETDB('IRES',IRUNP)
C
C If first call and no bank for this run then fill /IRESCC/ and /IRESOL/
C with default values.
      IF(FIRST.AND.IRETR.EQ.0) THEN
        DO 50 I=1,JLAYRE
          RESCOF(1,I) = SIGR(I) + SDIF
          RESCOF(2,I) = -4.0*SDIF
          RESCOF(3,I) = 4.0*SDIF
C Note resolution averaged over cell.
          SIGRIR(I) = RESCOF(1,I) + RESCOF(2,I)/2.0 + RESCOF(3,I)/3.0
C Note the maximum r-phi resolution in the cell.
          SMAXIR(I) = RESCOF(1,I)
          REND = RESCOF(1,I) + RESCOF(2,I) + RESCOF(3,I)
          IF (REND.GT.SMAXIR(I)) SMAXIR(I) = REND
          IF (RESCOF(3,I).LT.0.0) THEN
            FMID = -0.5*RESCOF(2,I)/RESCOF(3,I)
            IF (FMID.GT.0.0.AND.FMID.LT.1.0) SMAXIR(I) = RESCOF(1,I) +
     +      RESCOF(2,I)*FMID + RESCOF(3,I)*FMID**2
          END IF
   50   CONTINUE
      ENDIF
C
C Get values from new IRES bank.  Fill /IRESCC/ and /IRESOL/.
C
      IF((FIRST.AND.IRETR.GT.0).OR.(IRETR.LT.0)) THEN
        KIRES = IW(NAMIND('IRES'))
        DO 70 I=1,JLAYRE
          DO 60 J=1,JCOFIR
            RESCOF(J,I) = RTABL(KIRES,I,JIRECO-1+J)
   60     CONTINUE
C Note resolution averaged over cell.
          SIGRIR(I) = RESCOF(1,I) + RESCOF(2,I)/2.0 + RESCOF(3,I)/3.0
C Note the maximum r-phi resolution in the cell.
          SMAXIR(I) = RESCOF(1,I)
          REND = RESCOF(1,I) + RESCOF(2,I) + RESCOF(3,I)
          IF (REND.GT.SMAXIR(I)) SMAXIR(I) = REND
          IF (RESCOF(3,I).LT.0.0) THEN
            FMID = -0.5*RESCOF(2,I)/RESCOF(3,I)
            IF (FMID.GT.0.0.AND.FMID.LT.1.0) SMAXIR(I) = RESCOF(1,I) +
     +      RESCOF(2,I)*FMID + RESCOF(3,I)*FMID**2
          END IF
   70   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C Check for validity of IZRS bank.
C
      IRETZ = AGETDB('IZRS',IRUNP)
C
C If first call and no bank for this run then fill /IZRSCC/ and /IRESOL/
C with default values.
C
      IF(FIRST.AND.IRETZ.EQ.0) THEN
        DO 80 I=1,JLYRIZ
          ZRTRIZ(I)   = SIGZ(I)
          ZRESIZ(1,I) = SIGZ(I)
          ZRESIZ(2,I) = 0.0
          SIGZIR(I) = SIGZ(I)
   80   CONTINUE
      ENDIF
C
C Get values from new IZRS bank.  Fill /IZRSCC/ and /IRESOL/.
C
      IF((FIRST.AND.IRETZ.GT.0).OR.(IRETZ.LT.0)) THEN
        KIZRS = IW(NAMIND('IZRS'))
        DO 100 I=1,JLYRIZ
          ZRTRIZ(I) = RTABL(KIZRS,I,JIZRTR)
          DO 90 J=1,JCOFIZ
            ZRESIZ(J,I) = RTABL(KIZRS,I,JIZRCO-1+J)
   90     CONTINUE
          SIGZIR(I) = ZRESIZ(1,I)
  100   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C Check for validity of IEDD, IET0 and IRRF banks
C
      IRETE = AGETDB('IEDDIET0',IRUNP)
      IRETF = AGETDB('IRRF',IRUNP)
C
C IRRF is not in ADBS8990 ....
      IF (IRETF.EQ.0 .AND. IRUN.LE.10000) CALL ALTELL(
     +  'IINRES: IRRF bank not in ADBS8990 - defaults taken',
     +  0,'RETURN')
C
C Set return flag
C
      IRET = -1
      IF(IRETD.GT.0.AND.IRETR.GT.0.AND.IRETZ.GT.0
     +             .AND.IRETE.GT.0.AND.IRETF.GT.0) IRET = 1
      IF(IRETD.EQ.0 .OR.IRETR.EQ.0 .OR.IRETZ.EQ.0
     +              .OR.IRETE.EQ.0 .OR.IRETF.EQ.0) IRET = 0
C
      FIRST = .FALSE.
C
      END
#endif
