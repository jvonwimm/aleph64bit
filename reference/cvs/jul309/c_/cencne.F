      SUBROUTINE CENCNE(IECOB,ICHYP,ERES,IRES,IER)
C***********************************************************************
C! Compute residual energy in charged ecob                             *
C!                                                                     *
C!  AUTHOR   :  J BADIER     881216                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ECOB , EHYP , EPAR                                     *
C!    OUTPUT  : CPAR (via cparad)                                      *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Iecob  = Ecobject number (input)                                  *
C!   Ichyp  = CalHyp number   (input)                                  *
C!   Eres(4)= Residual energy (output)                                 *
C!   Ires   = 0 : there was no residual energy; # 0 else               *
C!   Ier    = Return code (0=OK)                                       *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
      DIMENSION ERES(*)
#include "ecnamc.h"
#include "bcs.h"
#include "ecobjj.h"
#include "ehypjj.h"
#include "eparjj.h"
#include "bmacro.h"
      IER=0
      IRES = 0
C   ECOB bank.
      KECOB=IW(NAECOB)
      IF (KECOB.EQ.0) GO TO 97
C   EHYP bank.
      KEHYP = IW(NAEHYP)
      IF (KEHYP.EQ.0) GO TO 97
C
C   Look the hypothesis
C
      IHYP1 = ITABL( KECOB , IECOB , JECOEH )
      IF( IHYP1 .EQ. 0 ) GO TO 97
C ----- First hypothesis type.
      ITYP1 = ITABL( KEHYP , IHYP1 , JEHYTY )
C ----- Other hypothesis ?
      NEXT = ITABL( KEHYP , IHYP1 , JEHYEH )
      IF( NEXT .NE. 0 ) THEN
C ----- Second hypothesis type.
        JTYP = ITABL( KEHYP , NEXT , JEHYTY )
C        Search the main hypothesis.
        IF( JTYP .LT. ITYP1 ) THEN
          ITYP2 = ITYP1
          ITYP1 = JTYP
          IHYP2 = IHYP1
          IHYP1 = NEXT
        ELSE
          IHYP2 = NEXT
          ITYP2 = JTYP
        ENDIF
      ENDIF
C   Main hypothesis
C   Corrected energy
      ERES(4) = RTABL( KEHYP , IHYP1 , JEHYEN)
C   Theta,phi.
      TH = RTABL( KEHYP , IHYP1 , JEHYTH )
      FI = RTABL( KEHYP , IHYP1 , JEHYPH )
C   Cartesian system
      W = COS( TH )
      S = SIN( TH )
      U = S * COS( FI )
      V = S * SIN( FI )
C   Impulsion
      ERES(1) = ERES(4) * U
      ERES(2) = ERES(4) * V
      ERES(3) = ERES(4) * W
C ----- Particles of the main hypothesis.
C        First particle.
C   EPAR bank.
      KEPAR = IW(NAEPAR)
      IF (KEPAR.EQ.0) GO TO 96
      IPAR = ITABL( KEHYP , IHYP1 , JEHYEP )
    1 CONTINUE
      IF( IPAR .EQ. 0 ) GO TO 96
C
C?  Update pointers to banks, in case of garbage collection
C
      KECOB=IW(NAECOB)
      KEHYP=IW(NAEHYP)
      KEPAR=IW(NAEPAR)
C ----- Particle type.
      KTYP = ITABL( KEPAR , IPAR , JEPATY )
C   Theta,phi.
      TH = RTABL( KEPAR , IPAR , JEPATH )
      FI = RTABL( KEPAR , IPAR , JEPAPH )
C   Cartesian system
      W = COS( TH )
      S = SIN( TH )
      U = S * COS( FI )
      V = S * SIN( FI )
C   Identificators.
      R1 = RTABL( KEPAR , IPAR , JEPAIT )
      R2 = RTABL( KEPAR , IPAR , JEPAIT + 1 )
C   Energy.
      E = RTABL( KEPAR , IPAR , JEPAEN )
C   Impulsion assuming zero mass.
      EX = E * U
      EY = E * V
      EZ = E * W
C   Residual energy.
      ERES(1) = ERES(1) - EX
      ERES(2) = ERES(2) - EY
      ERES(3) = ERES(3) - EZ
      ERES(4) = ERES(4) - E
C   Create PCPA
      CALL CPARAD(ICHYP,IHYP1,0,0,KTYP,E,TH,FI,R1,R2,IER)
      IF( IER .NE. 0 ) GO TO 97
C ----- Other particle ?
      KEPAR = IW(NAEPAR)
      IPAR = ITABL( KEPAR , IPAR , JEPAEP )
      GO TO 1
C   ----------------------------------------------------------
   97 CONTINUE
      IER = 1
   96 CONTINUE
C
C? At present (Julia 233), there is never any residual energy
C?  for neutral Ecobs : Ires should always be 0, as assigned
C?  at the beginning
C
C      IF( ABS( ERES(4) )  .GT. .001 ) IRES = 1
C
      RETURN
      END
#endif
