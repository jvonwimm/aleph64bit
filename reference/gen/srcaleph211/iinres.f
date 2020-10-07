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
C!    commons:    /IDTCCC/  Drift-time corrections - old parametrisation
C!                /IDRPCC/  Drift-time corrections - new parametrisation
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
C?        default values. Note that /IDTCCC/ is kept for backwards compa
C?   If (first and existing IDTC bank still valid) or (new IDTC bank) th
C?      get values from IDTC bank. Fill /IDTCCC/ and /IDRPCC/
C?   Endif
C? Else (Monte Carlo or 1993 onwards;  run number > 20000 or < 2000)
C?   Check validity of IDRP bank - AGETDB
C?   If (first and no IDRP bank) then fill /IDTCCC/ and /IDRPCC/ with
C?                                default values
C?   If (first and existing IDRP bank still valid) or (new IDRP bank) th
C?      get values from IDRP bank. Fill /IDTCCC/ and /IDRPCC/
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
      SAVE
C commons etc.
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JIDTID=1,JIDTVR=2,JIDTLN=4,JIDTTO=5,JIDTCO=6,LIDTCA=8)
      PARAMETER(JIDRID=1,JIDRVR=2,JIDRTO=4,JIDRCO=5,LIDRPA=9)
      PARAMETER(JIREID=1,JIREVR=2,JIRELN=4,JIRECO=5,LIRESA=7)
      PARAMETER(JIZRID=1,JIZRVR=2,JIZRLN=4,JIZRCO=5,JIZRTR=7,LIZRSA=7)
      INTEGER JLAYID,JCOFID
      REAL TOFFID,DCOFID
      PARAMETER (JLAYID=8,JCOFID=3)
      COMMON/IDTCCC/TOFFID(JLAYID),DCOFID(JCOFID,JLAYID)
      INTEGER MLAYID,MCOFID
      REAL TTOFID,DTCOID
      PARAMETER (MLAYID=8,MCOFID=5)
      COMMON/IDRPCC/TTOFID(MLAYID),DTCOID(MCOFID,MLAYID)
      INTEGER JCOFIR,JLAYRE
      REAL RESCOF
      PARAMETER (JCOFIR=3,JLAYRE=8)
      COMMON/IRESCC/RESCOF(JCOFIR,JLAYRE)
      INTEGER JCOFIZ,JLYRIZ
      REAL ZRTRIZ,ZRESIZ
      PARAMETER (JCOFIZ=2,JLYRIZ=8)
      COMMON/IZRSCC/ZRTRIZ(JLYRIZ),ZRESIZ(JCOFIZ,JLYRIZ)
      INTEGER JRESIR
      REAL SMAXIR,SIGRIR,SIGZIR
      PARAMETER (JRESIR=8)
      COMMON/IRESOL/SMAXIR(JRESIR),SIGRIR(JRESIR),SIGZIR(JRESIR)
C-----------------------------------------------------------------------
      EXTERNAL AGETDB,NAMIND,GTSTUP
      INTEGER AGETDB,NAMIND,GTSTUP
      LOGICAL FIRST
      INTEGER IRET
      REAL VDRF,SDIF,SIGR(8),SIGZ(8)
      DATA VDRF/0.0050/
      DATA SDIF/0.0080/,SIGR/8*0.0100/,SIGZ/8*3.0/
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
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
C If 1989-1992 data use IDTC bank
C
      IF (IRUN.GT.2000 .AND. IRUN.LE.20000) THEN
C
C Check for validity of IDTC bank.
C
        IRETD = AGETDB('IDTC',IRUNP)
C
C If first call and no bank for this run then fill /IDTCCC/ and /IDRPCC/
C                                             with default values.
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
C Get values from new IDTC bank.  Fill /IDTCCC/ and /IDRPCC/
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
      ELSE
C
C Else for Monte Carlo and 1993 data onwards use IDRP bank
C
C Check for validity of IDRP bank.
C
        IRETD = AGETDB('IDRP',IRUNP)
C
C If first call and no bank for this run then fill /IDTCCC/ and /IDRPCC/
C                                                 with default values.
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
C Get values from new IDTC bank.  Fill /IDTCCC/ and /IDRPCC/
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
