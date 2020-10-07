      SUBROUTINE IDELEC(ICH,IDIP,IDIF,RT,RL,LIDT)
C-----------------------------------------------------------------
C! Return electron identication information for
C  the ENFLW (CHARGED!) track ich.
C
C  Input:  -- ich:   The ENFLW track
C
C  Output  -- idip: 1 if electron (EIDT flag)
C                   0 if not electron
C                  -1 if no EIDT or p<800MeV/c
C          -- idif: Quality flag  (EIDT flag)
C          -- rt  : Transverse estimator
C          -- rl  : Transverse estimator
C          -- lidt: local identification flag
C
C  Patrick Janot - 27 June 1991
C-----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "qcde.h"
#include "qmacro.h"
C
      jch = ichefw(ich)
C
    2 IF ( jch .GE. kfrtot(3) .OR. jch .LT. kfrtot(1) ) THEN
        idip = 0
        idif = 0
        rt   = 0.
        rl   = 0.
        lidt = 5
      ELSE
        jch = jch - kfrtot(1) + 1
        idif = leflg(jch)/3
        idip = leflg(jch) - idif*3 - 1
        rt   = elrtr(jch)
        rl   = elrlg(jch)
        lidt = idtflg(jch)
      ENDIF
C
      RETURN
      END
#endif
