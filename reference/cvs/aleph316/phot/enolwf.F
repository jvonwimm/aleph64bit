       SUBROUTINE ENOLWF(XIN , ECLU , XPARM , ESX)
C=============================================================
CKEY PHOTONS
C!     Calculate normalised Xin using Xparm parametrisation
C!     Author                             M.N Minard 10/02/92
C!     Input      XIN  : Estimator to normalise
C!                ECLU : Energy of object
C!          XPARM(3,2) : (1,1) TO (3,1) parametrisation of xin
C!                       (1,2) to (3,2) error parametrisation
C!     Output    ESX   : Normalised estimator
C!=============================================================
#ifndef DOC
      SAVE
      DIMENSION XPARM(3,2)
C
C-    Calculate expected value
C
      ESX = -99.
      IF ( XPARM (1,1) .GT.-99.) THEN
      EXP = XPARM(1,1)+(1./(XPARM(2,1)+XPARM(3,1)*ECLU))
      SEXP= XPARM(1,2)+(1./(XPARM(2,2)+XPARM(3,2)*ECLU))
      ESX = ( XIN-EXP) / SEXP
      ENDIF
      RETURN
      END
#endif
