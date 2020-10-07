      SUBROUTINE IGEOMW(IRUN,IRET)
C-----------------------------------------------------------------------
C! Set up ITC geometry constants for Wires.
C!
CKEY ITCDES ITC GEOM /INTERNAL
C!   Author          :-  J. Sedgbeer  89/03/03
C!   Modified        :-  J. Sedgbeer  89/10/11 Wire Position corrections
C!   Modified        :-  J. Sedgbeer  90/01/04 Get IWST and ICAE banks
C!   Modified        :-  J. Sedgbeer  91/01/07 Ensure IWST bank got from
C!                       correct place - can be cards, tape, dbase.
C!   Modified        :-  J.Sedgbeer   91/10/21 Get IWST from 1) cards,
C!                                    2) run header, 3) database
C!   Modified        :-  J. Sedgbeer  92/02/04 Implement run-period
C!                    scheme for some dbase banks.
C!   Input:
C!    IRUN    /I : Current run number
C!    params.:    ALCONS
C!                ILYRJJ  for ILYR bank
C!                ITCCJJ  for ITCC bank
C!                IEWPJJ  for IEWP bank
C!    commons:    /BCS/   for banks  ILYR,ITCC,IEWP,IWST,ICAE from 'DB'
C!
C!   Output:
C!    IRET    /I : Error flag: (as for AGETDB)
C!                   IRET > 0  Existing values still valid
C!                   IRET = 0  Error. One or more banks missing for this
C!                             run - leave values unchanged or, if first
C!                             time use default values.
C!                   IRET < 0  1 or more banks reloaded
C!    commons:    /ITWICC/  ITC wire geometry common
C!                /ISWPHI/  Sense wire phi values in ITC frame.
C!
C!   calls     : AGETDB (Alephlib)
C!               IGETDB (Alephlib)
C!               GTSTUP (Alephlib)
C!   libraries:  none
C!
C!   Description:
C! Set up ITC wire geometry in common /ITWICC/ and sense wire coords.
C! in common /ISWPHI/.
C! Get data from a direct access file ( filled from D.B.) or from
C! data cards or via bank input with data.
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
C? Check validity of ITCC bank - AGETDB
C? If (first and no ITCC bank) then fill /ITWICC/ with default values
C? If (first and existing ITCC bank still valid) or (new ITCC bank) then
C?    get values from ITCC bank. Fill part of /ITWICC/
C? Endif
C? Check validity of ILYR bank - AGETDB
C? If (first and no ILYR bank) then fill /ITWICC/ and /ISWPHI/ with
C?                                     default values
C? If (first and existing ILYR bank still valid) or (new ILYR bank) then
C?    get values from ILYR bank. Fill part of /ITWICC/
C?    hence fill /ISWPHI/
C? Endif
C? Check validity of IEWP bank - AGETDB
C? If (first and existing IEWP bank still valid) or (new IEWP bank) then
C?    correct wire positions in /ISWPHI/
C? Endif
C? Get IWST bank - IGETDB
C? Check validity of ICAE bank - AGETDB
C? Set return flag
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C commons etc.
#include "alcons.h"
#include "bcs.h"
#include "ilyrjj.h"
#include "itccjj.h"
#include "itwicc.h"
#include "iewpjj.h"
#include "iswphi.h"
C-----------------------------------------------------------------------
      EXTERNAL AGETDB,NAMIND,GTSTUP
      INTEGER AGETDB,NAMIND,GTSTUP
      LOGICAL FIRST
      REAL WZMX,SGMX,RWIR(8),PHWR(8)
      INTEGER NWIR(8),IWIR(8)
      DATA WZMX/100./,SGMX/0.010/
      DATA NWIR/4*96,4*144/,IWIR/0,96,192,288,384,528,672,816/
      DATA RWIR/16.11,17.20,18.70,19.79,21.68,23.01,24.69,26.02/
      DATA PHWR/0.,1.,0.,1.,0.,1.,0.,1./
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
C Check for validity of ITCC bank.
C
      IRET1 = AGETDB('ITCC',IRUNP)
C
C If first call and no bank for this run then fill /ITWICC/ with default
C                                                        values.
      IF(FIRST.AND.IRET1.EQ.0) THEN
        WZMXIT = WZMX
        SGMXIT = SGMX
      ENDIF
C
C Get values from new ITCC bank.  Fill /ITWICC/
C
      IF((FIRST.AND.IRET1.GT.0).OR.(IRET1.LT.0)) THEN
        KITCC  = IW(NAMIND('ITCC'))
        WZMXIT = RTABL(KITCC,1,JITCWL)
        SGMXIT = RTABL(KITCC,1,JITCWS)
      ENDIF
C
C Check for validity of ILYR bank.
C
      IRET2 = AGETDB('ILYR',IRUNP)
C
C If first call and no bank for this run then fill /ITWICC/ and
C             /ISWPHI/ with default values.
C
      IF(FIRST.AND.IRET2.EQ.0) THEN
        DO 100 I=1,8
          IWIRIT(I) = IWIR(I)
          NWIRIT(I) = NWIR(I)
          RWIRIT(I) = RWIR(I)
          CELWIT(I) = TWOPI*RWIR(I)/FLOAT(NWIR(I))
          PHWRIT(I) = PI*PHWR(I)/FLOAT(NWIR(I))
  100   CONTINUE
C
        DO 210 IL = 1,8
          NW = NWIRIT(IL)
          DO 200 J = 1,NW
            IND = IWIRIT(IL) + J
            PHSWIS(IND) = FLOAT(J-1)*TWOPI/FLOAT(NW) + PHWRIT(IL)
  200     CONTINUE
  210   CONTINUE
      ENDIF
C
C Get values from new ILYR bank.  Fill /ITWICC/ and hence /ISWPHI/
C
      IF((FIRST.AND.IRET2.GT.0).OR.(IRET2.LT.0)) THEN
        KILYR = IW(NAMIND('ILYR'))
        NROW  = LROWS(KILYR)
        IWTOT = 0
        DO 300 I=1,NROW
          LAY         = ITABL(KILYR,I,JILYLN)
          IWIRIT(LAY) = IWTOT
          NWIRIT(LAY) = ITABL(KILYR,I,JILYNW)
          IWTOT       = IWTOT+NWIRIT(LAY)
          RWIRIT(LAY) = RTABL(KILYR,I,JILYRW)
          CELWIT(LAY) = TWOPI*RWIRIT(LAY)/FLOAT(NWIRIT(LAY))
          PHOFF       = FLOAT(ITABL(KILYR,I,JILYPO))
          PHWRIT(LAY) = PI*PHOFF/FLOAT(NWIRIT(LAY))
  300   CONTINUE
C
        DO 410 IL = 1,8
          NW = NWIRIT(IL)
          DO 400 J = 1,NW
            IND = IWIRIT(IL) + J
            PHSWIS(IND) = FLOAT(J-1)*TWOPI/FLOAT(NW) + PHWRIT(IL)
  400     CONTINUE
  410   CONTINUE
      ENDIF
C
C Check for validity of IEWP bank.
C
      IRET3 = AGETDB('IEWP',IRUNP)
C
C Get values from new IEWP bank.  Correct /ISWPHI/
C
      IF((FIRST.AND.IRET3.GT.0).OR.(IRET3.LT.0)) THEN
        KIEWP = IW(NAMIND('IEWP'))
        DO 510 IL = 1,8
          KK = KROW(KIEWP,IL)
          NW = NWIRIT(IL)
          DO 500 J = 1,NW
            IND = IWIRIT(IL) + J
            PHSWIS(IND) = PHSWIS(IND) - RW(KK+JIEWDP-1+J)
  500     CONTINUE
  510   CONTINUE
      ENDIF
C
C Set IDAF = 1 to get hierarchy cards/run header/database
      IDAF = 1
C
C Get IWST bank
      CALL IGETDB('IWST',IRUNP,IDAF,IRET4,IER)
C
C Get ICAE bank (may be there or not)
      IRET5 = AGETDB('ICAE',IRUNP)
C
C Set return flag (IRET5, i.e. ICAE bank, not important)
C
      IRET = -1
      IF(IRET1.GT.0.AND.IRET2.GT.0.AND.IRET3.GT.0
     +                            .AND.IRET4.GT.0) IRET = 1
      IF(IRET1.EQ.0 .OR.IRET2.EQ.0 .OR.IRET3.EQ.0
     +                             .OR.IRET4.EQ.0) IRET = 0
C
      FIRST = .FALSE.
C
      END
#endif
