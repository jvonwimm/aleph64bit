      SUBROUTINE SRUPAT(IER)
C----------------------------------------------------------------------
C! Find parameters of patches
C! where no tracks are found
C!
C!    Author:     H. Meinhard       29-Mar-1988
C!    Modified:   H. Meinhard       19-Apr-1988
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    In case that at any side no track was found, get mean parameters
C!    of patches:
C!    - On each side, look whether a track was found; if not, determine
C!      parameters of patches (theta, phi) together with their errors;
C!    - Look whether distance of two patches is below a minimum
C!      distance. If yes, patches are too close together, find mean
C!      value of parameters
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "sanamc.h"
#include "srecpc.h"
#include "sstpjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C look whether bank SUPA exists; if yes, return
      IF (IW(NASUPA) .NE. 0)                                GOTO 999
C
C if tracks on both sides, no action
      KSSTP = IW(NASSTP)
      IF (KSSTP .EQ. 0) THEN
        NTRKA = 0
        NTRKB = 0
      ELSE
        NTRKA = ITABL(KSSTP,1,JSSTPE) - ITABL(KSSTP,1,JSSTPB) + 1
        NTRKB = ITABL(KSSTP,2,JSSTPE) - ITABL(KSSTP,2,JSSTPB) + 1
      ENDIF
      IF (NTRKA.GT.0 .AND. NTRKB.GT.0)                      GOTO 999
C
C find parameters of patches ...
      CALL SRUPAR(IER)
      IF (IER .EQ. 1)                                       GOTO 999
      IF (IER .NE. 0)                                       GOTO 902
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Patches on sides without tracks found:'
        CALL SUPAPR(LOUTRL)
      ENDIF
C
C get mean value of patches being close together ...
      CALL SRSELO('SUPA','SUCP','SSUP',ACPASR,IER)
      IF (IER .EQ. 1)                                       GOTO 999
      IF (IER .NE. 0)                                       GOTO 903
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Patches on sides without tracks selected. Patches found:'
        CALL SUPAPR(LOUTRL)
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  902 IER = 2
      GOTO 999
  903 IER = 3
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
