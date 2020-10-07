      SUBROUTINE IFECON(IRUN,IRET)
C-----------------------------------------------------------------------
C! Set up ITC Front-End electronics constants.
C!
CKEY ITCDES ITC / INTERNAL
C!   Author          :-  J. Sedgbeer  89/03/03
C!   Modified        :-  J. Sedgbeer  89/10/11 Fix for IRFE bank format
C!                                             error.
C!   Modified        :-  J. Sedgbeer  90/01/04 Get ISFE DB bank
C!   Modified        :-  J. Sedgbeer  91/01/07 Ensure IRFE and IZFE
C!                    banks are got from correct place - can be on cards
C!                    run header or Dbase.
C!   Modified        :-  J. Sedgbeer  92/01/30 Check r-phi TDC bin width
C!                    from IRFE bank. Remove default setting. Remove
C!                    obsolete code (for old MC)
C!   Modified        :-  J. Sedgbeer  92/02/04 Implement run-period
C!                    scheme for some dbase banks.
C!   Modified        :-  J. Sedgbeer  92/02/07 Get IZNL from Dbase. Fill
C!                    /IZNLCC/ from IZNL (or from IZFE if no IZNL)
C!   Modified: 95/04/02 J.Sedgbeer. Remove check on TDC binwidth from IRFE
C!
C!   Input:
C!    IRUN    /I : Current run number
C!    params.:    IRFEJJ  for ITDC bank
C!                IZFEJJ  for IZFE bank
C!                IZNLJJ  for IZNL bank
C!                ISFEJJ  for ISFE bank
C!    commons:    /BCS/   for banks  IRFE,IZFE,IZNL,ISFE from 'DB'
C!   Output:
C!    IRET    /I : Error flag: (as for AGETDB)
C!                   IRET > 0  Existing values still valid
C!                   IRET = 0  Error. One or more banks missing for this
C!                             run. FATAL.
C!                   IRET < 0  1 or more banks reloaded
C!    common   /IRFECC/  R-phi front end parameters
C!             /IZFECC/  Z front end parameters
C!             /IZNLCC/  Z non linearity parameters
C!
C!   calls     : AGETDB (Alephlib)
C!               IGETDB (Alephlib)
C!               GTSTUP (Alephlib)
C!   libraries:  BOS
C!
C!   Description:
C! Set up ITC front end constants.
C! Get data from a direct access file ( filled from D.B.), or input
C! data cards or via banks read in with data.
C!
C? If data (run number > 2000) then
C?   run period = run number
C? else (MC)
C?   get run period from function GTSTUP
C?   if no set-up number found set run period = run number
C? endif
C?
C? Get IRFE bank - IGETDB
C? If (first and existing IRFE bank still valid) or (new IRFE bank) then
C?    get values from IRFE bank. Fill /IRFECC/
C? Endif
C?
C? Get IZFE bank - IGETDB
C? If (first and existing IZFE bank still valid) or (new IZFE bank) then
C?    get values from IRFE bank. Fill /IZFECC/
C? Endif
C?
C? Get IZNL bank - AGETDB
C? If (first and existing IZNL bank still valid) or (new IZNL bank) then
C?    get values from IZNL bank. Fill /IZNLCC/
C? elseif( IZNL missing) then
C?    fill /IZNLCC/ from IZFE
C? Endif
C?
C? Check validity of ISFE bank - AGETDB.
C? Set Z flag in /IZFECC/
C? Set return flag
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons etc.
#include "bcs.h"
#include "isfejj.h"
#include "iznljj.h"
#include "irfejj.h"
#include "izfejj.h"
#include "irfecc.h"
#include "izfecc.h"
#include "iznlcc.h"
C-----------------------------------------------------------------------
      EXTERNAL AGETDB,NAMIND,GTSTUP
      INTEGER AGETDB,NAMIND,GTSTUP
      LOGICAL FIRST
      INTEGER IRET,IRETR,IRETZ,IRUN,IRUNP,KIRFE,KIZFE
      INTEGER IDAF,IER
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
C Set IDAF according to UDAF card
      IDAF = IW(NAMIND('UDAF'))
C
C Get IRFE bank.
C
      CALL IGETDB('IRFE',IRUNP,IDAF,IRETR,IER)
C
C Get values from new IRFE bank.  Fill /IRFECC/
C
      IF((FIRST.AND.IRETR.GT.0).OR.(IRETR.LT.0)) THEN
        KIRFE = IW(NAMIND('IRFE'))
C Set bin width and check. If bad set IRETR=0, i.e.FATAL error.
        BWIDIR = RTABL(KIRFE,1,JIRFBW)
        IBN0IR = ITABL(KIRFE,1,JIRFZB)
        DO 20 I=1,JLAYIR
          ITLOIR(I) = ITABL(KIRFE,1,JIRFCL+I-1)
          ITHIIR(I) = ITABL(KIRFE,1,JIRFCH+I-1)
   20   CONTINUE
      ENDIF
C
C Get IZFE bank.
C
      CALL IGETDB('IZFE',IRUNP,IDAF,IRETZ,IER)
C
C Get values from new IZFE bank.  Fill /IZFECC/
C
      IF((FIRST.AND.IRETZ.GT.0).OR.(IRETZ.LT.0)) THEN
        KIZFE = IW(NAMIND('IZFE'))
        BWIDIZ = RTABL(KIZFE,1,JIZFBW)
        IBN0IZ = ITABL(KIZFE,1,JIZFZB)
        EXP8IZ = RTABL(KIZFE,1,JIZFEX)
        SBNDIZ(1) = RTABL(KIZFE,1,JIZFS1)
        SBNDIZ(2) = RTABL(KIZFE,1,JIZFS2)
        SBNDIZ(3) = RTABL(KIZFE,1,JIZFS3)
        IBNDIZ    = ITABL(KIZFE,1,JIZFOO)
        DO 40 I=1,JLAYIR
          ITLOIZ(I) = ITABL(KIZFE,1,JIZFCL+I-1)
          ITHIIZ(I) = ITABL(KIZFE,1,JIZFCH+I-1)
   40   CONTINUE
      ENDIF
C
C Get IZNL bank.
C
      IREZ = AGETDB('IZNL',IRUNP)
C
C Get values from new IZNL bank.  Fill /IZNLCC/
C
      IF((FIRST.AND.IREZ.GT.0).OR.(IREZ.LT.0)) THEN
        KIZNL = IW(NAMIND('IZNL'))
        DO 50 I=1,JOFSLN
          OFSLIZ(I) = RW(KIZNL+LMHLEN+JIZNLO-1+I)
   50   CONTINUE
        CZNLIZ(1) = RW(KIZNL+LMHLEN+JIZNS1)
        CZNLIZ(2) = RW(KIZNL+LMHLEN+JIZNS2)
        CZNLIZ(3) = RW(KIZNL+LMHLEN+JIZNS3)
C
C If no IZNL bank then fill /IZNLCC/ from IZFE
      ELSEIF(IREZ.EQ.0) THEN
        IF(IRETZ.NE.0) THEN
          KIZFE = IW(NAMIND('IZFE'))
          DO 60 I=1,JOFSLN
            OFSLIZ(I) = RW(KIZFE+LMHLEN+JIZFS1)
   60     CONTINUE
          CZNLIZ(1) = 1.0
          CZNLIZ(2) = RW(KIZFE+LMHLEN+JIZFS2)
          CZNLIZ(3) = RW(KIZFE+LMHLEN+JIZFS3)
        ENDIF
      ENDIF
C
C Check for validity of ISFE bank.
C
      IRETS = AGETDB('ISFE',IRUNP)
      IF(FIRST) FZCOIZ = .TRUE.
      IF((FIRST.AND.IRETS.GT.0).OR.(IRETS.LT.0)) THEN
        KISFE = IW(NAMIND('ISFE'))
        FZCOIZ = IW(KISFE+LMHLEN+JISFZT).EQ.1
      ENDIF
C
C Set return flag. Don't consider IZNL.
C
      IRET = -1
      IF(IRETR.GT.0.AND.IRETZ.GT.0.AND.IRETS.GT.0) IRET = 1
      IF(IRETR.EQ.0 .OR.IRETZ.EQ.0 .OR.IRETS.EQ.0) IRET = 0
C
      FIRST = .FALSE.
C
      END
#endif
