      SUBROUTINE CENCCH(IECOB,ICHYP,ERES,IRES,IER)
C***********************************************************************
C! Compute residual energy in charged ecob                             *
C!                                                                     *
C!  AUTHOR   : D. PALLIN     881221                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ECLU                                                   *
C!    OUTPUT  : CPAR (via cparad)                                      *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Iecob   = Ecobject number (input)                                 *
C!   Ichyp   = CalHyp number   (input)                                 *
C!   IRES    = 0 IF NO RESIDUAL ENERGY (OUTPUT)                        *
C!   Eres(4) = Residual energy (output)                                *
C!   Ier     = Return code (0=OK)                                      *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "ecnamc.h"
#include "ehypjj.h"
#include "eparjj.h"
#include "ecobjj.h"
#include "bcs.h"
      DIMENSION ERES(4)
#include "bmacro.h"
      IER=0
C  THERE IS ALWAYS A NEUTRAL RESIDUAL ENERGY  FOR A CHARGED CLUSTER
      IRES=1
C
C?
C
      KECOB=IW(NAECOB)
      KEHYP=IW(NAEHYP)
      KEPAR=IW(NAEPAR)
C
C? WHICH HYP FOR THIS ECOB?
C
      IHYP=ITABL(KECOB,IECOB,JECOEH)
      EC=RTABL(KEHYP,IHYP,JEHYEN)
      THC=RTABL(KEHYP,IHYP,JEHYTH)
      PHC=RTABL(KEHYP,IHYP,JEHYPH)
C
C? GET ASSOCIATED EPAR (IF ONE)
C
      IPAR=ITABL(KEHYP,IHYP,JEHYEP)
      PHG=0.
      THG=0.
      EG=0
      IDUM=0
      DUM=0.
      IF(IPAR.NE.0)THEN
      NATUR=ITABL(KEPAR,IPAR,JEPATY)
      EG=RTABL(KEPAR,IPAR,JEPAEN)
      THG=RTABL(KEPAR,IPAR,JEPATH)
      PHG=RTABL(KEPAR,IPAR,JEPAPH)
C  FILL PCPA FOR THIS EPAR
      CALL CPARAD(ICHYP,IHYP,IDUM,IDUM,NATUR,EG,THG,PHG,DUM,DUM,IER)
      ENDIF
C  FIND CHARGED RESIDUAL ENERGY
      ERES(1)=EC*COS(PHC)*SIN(THC)-EG*COS(PHG)*SIN(THG)
      ERES(2)=EC*SIN(PHC)*SIN(THC)-EG*SIN(PHG)*SIN(THG)
      ERES(3)=EC*COS(THC)-EG*COS(THG)
      ERES(4)=EC-EG
      RETURN
      END
#endif
