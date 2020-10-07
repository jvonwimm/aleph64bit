      SUBROUTINE IGETDB(NAME,IRUN,IDAF,IRET,IER)
C-----------------------------------------------------------------------
C! Get ITC Dbase banks - for banks on run header and/or Dbase
C!
CKEY ITCDES ITC / INTERNAL
C!   Author      :-  J. Sedgbeer  91/01/07
C!   Modified    :-  J. Sedgbeer  91/10/21 Comment added.
C!
C!   Input:
C!    NAME    /A : Bank name
C!    IRUN    /I : Current run number
C!    IDAF    /I : Hierarchy flag:
C!                  IDAF.eq.0 : cards, Dbase, Run Header
C!                  IDAF.ne.0 : cards, Run header, Dbase
C!    commons    :  /BCS/   for banks
C!
C!   Output:
C!    IRET    /I : Return flag: (as for AGETDB)
C!                   IRET > 0  Existing values still valid
C!                   IRET = 0  Bank missing for this run
C!                   IRET < 0  bank reloaded
C!    IER     /I : Error flag  O.K. if IER = 0
C!
C!   calls     : JUNIDB - ALEPHLIB
C!   libraries : BOS
C!
C!   Description:
C! ONLY to be used for banks that can be on run header and or Dbase.
C! For ITC at present this applies only to IRFE, IZFE, IWST.
C! Get bank 'NAME' according to hierarchy specified by input argument
C! IDAF. Note that this routine assumes that
C!    data cards have bank number  = -1
C!    run header banks have number =  0
C!    data base banks have number  >  0
C! ** N.B.** This routine does not get the database or check that the
C!           database that is currently open is the correct one for the
C!           given run number. It is assumed that the correct database
C!           is already open.
C!
C? Get list of all banks 'NAME' currently in BCS
C? If bank number = -1 exists (data card)  then set FCARD flag
C? If bank number =  0 exists (run header) then set FHEAD flag
C? If FCARD then
C?   drop all but cards bank, set IRET=1 and return
C? endif
C? If IDAF.ne.0 and run header bank exists (FHEAD) then
C?   drop all but header bank, set IRET=-1 and return
C?   (Note: IRET set to -1 as this forces an update as you can't tell
C?    if a new bank number 0 was read in and overwrote the previous one.)
C? endif
C? Get Dbase bank number, NKEEP, for this run (use NDANR)
C? If no Dbase bank then
C?   keep header bank, NKEEP
C?   If no header bank then
C?     set IRET=0 (error) and return
C?   endif
C? endif
C? Drop all banks except bank number NKEEP (if it is already in /BCS/)
C? Set IRET=1 (existing bank still valid)
C? If bank NKEEP not already in BCS then
C?   get it from Dbase (use MDARD), set IRET = -1
C?   if problem loading it then set IRET=0
C? endif
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons etc.
#include "bcs.h"
      INTEGER NBMX
      PARAMETER (NBMX=10)
      INTEGER NBNK(NBMX),NNB
      LOGICAL FCARD,FHEAD,FGET
      CHARACTER*(*) NAME*4
      INTEGER IRUN,IDAF,IRET,IER,IND,I,NKEEP,LBASE
      INTEGER NAMIND,NDROP,MDARD,NDANR,JUNIDB
      EXTERNAL NAMIND,NDROP,MDARD,NDANR,JUNIDB
      INTEGER NCARD,NHEAD
      DATA NCARD/-1/,NHEAD/0/
C-----------------------------------------------------------------------
C
      LBASE = JUNIDB(0)
      IER = 0
      IRET = 0
      FCARD = .FALSE.
      FHEAD = .FALSE.
C
C Get all banks of NAME. Set flags if cards or run header bank found
C
      NNB = 0
      IND = NAMIND(NAME) + 1
   10 IND = IW(IND-1)
      IF(IND.NE.0) THEN
        IF(NNB.GE.NBMX) THEN
          IER = 1
          GOTO 999
        ENDIF
        NNB = NNB + 1
        NBNK(NNB) = IW(IND-2)
        IF(IW(IND-2).EQ.NCARD) FCARD = .TRUE.
        IF(IW(IND-2).EQ.NHEAD) FHEAD = .TRUE.
        GOTO 10
      ENDIF
C
C If cards bank exists then drop all others and return
C
      IF(FCARD) THEN
        DO 20 I=1,NNB
          IF(NBNK(I).NE.NCARD) IND = NDROP(NAME,NBNK(I))
   20   CONTINUE
        IRET = 1
        GOTO 999
      ENDIF
C
C If input IDAF.ne.0 then get run header bank (if it exists) and
C drop rest. (If no header bank then default to Dbase.)
C Note: return flag is set to -1 for run header banks. This forces
C an update as you can't tell if a new bank number 0 was read in and
C overwrote the previous one.
C
      IF(IDAF.NE.0.AND.FHEAD) THEN
        DO 30 I=1,NNB
          IF(NBNK(I).NE.NHEAD) IND = NDROP(NAME,NBNK(I))
   30   CONTINUE
        IRET = -1
        GOTO 999
      ENDIF
C
C Dbase bank number for this run: If no Dbase bank then keep header bank.
C                                 If no Dbase and no header => error.
C
      NKEEP = NDANR(LBASE,NAME,'LE',IRUN)
      IF(NKEEP.EQ.0) THEN
        NKEEP = NHEAD
        IF(.NOT.FHEAD) THEN
          IRET = 0
          GOTO 999
        ENDIF
      ENDIF
C
C Drop all banks except bank number NKEEP (if it is already in /BCS/)
C
      FGET = .TRUE.
      DO 40 I=1,NNB
        IF(NBNK(I).NE.NKEEP) THEN
          IND = NDROP(NAME,NBNK(I))
        ELSE
          FGET = .FALSE.
        ENDIF
   40 CONTINUE
C
C If Dbase bank not loaded then get it:
C
      IRET = 1
      IF(FGET) THEN
        IRET = -1
        IND = MDARD(IW,LBASE,NAME,NKEEP)
        IF(IND.LE.0) IRET = 0
      ENDIF
C
  999 CONTINUE
      END
#endif
