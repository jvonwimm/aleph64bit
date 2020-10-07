      SUBROUTINE ECTPC(IER)
C***********************************************************************
C!CREATE TPC TRACKS                                                    *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  861128                                    *
C!  MODIFIED : DENIS COURVOISIER  890324                               *
C!                                                                     *
C!  CREATE FAKED TPC TRACKS FROM THE TRUE MONTE CARLO DATA    ,        *
C!    FOR EXTRAPOLATION IN ECAL                                        *
C!                                                                     *
C!   STEERING ,  CALLS :                                               *
C!             EFICV GETS PARTICLE AT ITS CREATION VERTEX              *
C!             EPNEU  PROPAGATES NEUTRALS AT TPC EXIT                  *
C!             EPCHG PROPAGATES CHARGED                                *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!    INPUT  : NONE                                                    *
C!                                                                     *
C!    OUTPUT : NONE                                                    *
C!                                                                     *
C!                                                                     *
C!       IER = return code ( 0 if normal)                              *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
#include "ecnamc.h"
      IER=0
C
C?   If etp1 bank exists, use it
C?
      IF(IW(NAETP1).NE.0)THEN
         IF (JDBDRF(4) .GE. 1) THEN
            CALL RERROR(' ECTPC',1,' ETP1 BANK PRESENT, WILL BE USED')
         ENDIF
C
C?   IF TPC RECONSTRUCTION ASKED, USE IT
C
      ELSEIF(FDETRF(JULTP))THEN
         IF (JDBDRF(4) .GE. 1) THEN
            CALL RERROR(' ECTPC',2,' USING TGFT BANK  ')
         ENDIF
         CALL EPRTPC (IER)
         IF(IER.NE.0)GO TO 998
      ELSE
C         if data card EOPT exists, replace etp1 for FKIN or KINE
C                 (Monte-Carlo banks)
        KEOPT= NLINK ('EOPT',0)
        IF (KEOPT.NE.0) THEN
         IF (JDBDRF(4) .GE. 1) THEN
           WRITE(LDEBRL,*)' ECTPC --> TPC RECONSTRUCTION NOT REQUESTED'
           WRITE(LDEBRL,*)' ECTPC --> USING TPTE BANK(TRUE MONTE-CARLO'
         ENDIF
         CALL EFICV(IER)
         IF(IER.NE.0)GO TO 998
         CALL EPCHG
         CALL EPNEU
         CALL EPCALO
        ELSE
          CALL EPRTPC(IER)
          IF (IER.NE.0) GO TO 998
        ENDIF
      ENDIF
      RETURN
  998 CONTINUE
      CALL RERROR(' ECTPC',4,' exit')
      RETURN
      END
#endif
