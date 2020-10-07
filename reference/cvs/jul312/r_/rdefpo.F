      SUBROUTINE RDEFPO
C----------------------------------------------------------------------
C!  - make POT banks
C!   Created by Juergen Knobloch       8-JUL-1988
C!   Modified by F.Ranjard            17-FEB-1995
C!               call LUPACR when LCAL .and. SATR are required.
C!               if not name-indices are not all set.
C!
C!   Purpose   : STEERING ROUTINE TO CREATE POT BANKS
C!   Inputs    :
C!   Outputs   :
C!
C!   Called by : RECONS
C!   Calls     : CPOTCR,TJTOP,FITWRI,HPRPOT
C!
C!   Modified  : B.Bloch, P.Comas     7-FEB-1995
C!              First LCAL has to be processed and after SICAL: even-
C!              tually, PECO is created by LCAL and used by SICAL
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "rparac.h"
      LOGICAL FLAGS(3)
C
C?   MVD
C
      IF (FDETRF(JULVD)) CALL VPRPOT
C
C?   ITC
C
      IF (FDETRF(JULIT)) CALL IREOPD(IER)
C
C?   TPC
C
      FLAGS(1)=.TRUE.
      FLAGS(2)=.TRUE.
      FLAGS(3)=.TRUE.
      IF (FDETRF(JULTP)) CALL TJTOP(FLAGS,IER)
C
C?   FIT
C
      IF (FDETRF(JULTP)) CALL FITWRI
C
C?   ECAL and general Calorimeter banks
C
      IF (FDETRF(JULEC).OR.FDETRF(JULHC))CALL CPOTCR(IERR)
      IF (FDETRF(JULEC).OR.FDETRF(JULHC)) CALL CPOTEH(IER)
      IF (FDETRF(JULEC)) CALL CPADWR (IER)
C
C?   HCAL
C
      IF (FDETRF(JULHC)) THEN
        CALL PHCHIC(IER)
        CALL HPRPOT
        CALL HDGPOT
      ENDIF
C
C?   LCAL
C
      IF (FDETRF(JULLC)) CALL LCRPOT
C
C?   SATR
C
      IF (FDETRF(JULSA)) CALL SPRPOT(IER)
C
C?   Lumi
C
      IF (FDETRF(JULLC).AND.FDETRF(JULSA)) CALL LUPACR(IER)
C
C?    B.Bloch and P.Comas (7-FEB-1995)
C?    First LCAL and after SICAL: PECO is created by LCAL and
C?    used by SICAL
C
C?   IF LCAL OR reprocessing with LCAL object saved in LPSV bank
C
      IF (FDETRF(JULLC).OR. IW(NAMIND('LPSV')).NE.0) CALL CPOTLC
C
C?   SICAL
C
      IF (FDETRF(JULSI)) CALL SICRPOT
C
C?   Build relation between M.C. true  and reconstructed Calo. Objects
C
      IF (FDETRF(JULEC).OR.FDETRF(JULLC)) CALL PECHIC(IER)
  999 RETURN
      END
#endif
