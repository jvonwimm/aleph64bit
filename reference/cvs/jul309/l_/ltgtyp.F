      SUBROUTINE LTGTYP(IRUN,MASK,XLTRG)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :-  J.Wear               21-MAR-1990
C!
C!   Inputs:  IRUN -- run number
C!        -   MASK -- fired trigger mask (either L1,L2,L2*enable mask)
C!
C!   Outputs: XLTRG(5) -- logical array containing which type of Lumi
C!                        triggers have fired. (either wire or tower)
C!            ITYPE=1     : single low threshold
C!                 =2     : single high threshold
C!                 =3     : single very high threshold
C!                 =4     : low-threshold coincidence
C!                 =5     : high-threshold coincidence
C!
C!   Libraries required:
C!
C!   Description  This routine begs to have more readable code.  Will
C!   ===========  be upgraded when Trigger code is available in ALEPHLIB
C!
C?
C!======================================================================
#ifndef DOC
#include "rcurnt.h"
      PARAMETER (NLTYP=5,NLBIT=5)
      LOGICAL XLTRG(NLTYP)
      LOGICAL BTEST
      DIMENSION LBITS(NLBIT),LTORW(NLBIT)
      CHARACTER*8 TLIST(NLTYP)
      DATA TLIST /'LO_SING','HI_SING','VH_SING','LO_COIN','HI_COIN'/
C
      DO 10 I=1,NLTYP
  10    XLTRG(I) = .FALSE.
C
C Set Bhabha trigger bit for all Monte-Carlo
      IF(FMCRUN) THEN
        XLTRG(5) = .TRUE.
        GOTO 999
      ENDIF
C
C Get bit assignment of the trigger types and check them
C
      DO 11 ITTYP = 1,NLTYP
        CALL LTGBIT(IRUN,TLIST(ITTYP),LBITS,LTORW,NTBIT,IFAIL)
        IF (IFAIL.GT.1) GOTO 999
        DO 12 ILBIT = 1,NLBIT
          IF ( LBITS(ILBIT).GT.0 )
     &    XLTRG(ITTYP) = XLTRG(ITTYP).OR.BTEST(MASK,LBITS(ILBIT))
 12     CONTINUE
 11   CONTINUE
  999 RETURN
      END
#endif
