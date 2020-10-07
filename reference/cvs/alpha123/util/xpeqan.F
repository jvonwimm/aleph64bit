      FUNCTION XPEQAN(ITK,CPAR,IANTI)
C----------------------------------------------------------------------
C! Test if particle ITK is of type CPAR ( if IANTI = 0 )
C     or its antiparticle ( if IANTI .NE. 0 )
C     J. Boucrot  10-Nov-1993           adapted from I.Tomalin   1/2/93
C----------------------------------------------------------------------
#ifndef DOC
#include "parna.h"
      LOGICAL XPEQAN
      CHARACTER*(*) CPAR
#include "qcdesh.h"
      KTPCOD(KI)=IW(KOQVEC+KI*KCQVEC+JQVEPA)
C----------------------------------------------------------------------
      ICODE = KTPCOD(ITK)
      IF (IANTI.EQ.0) THEN
        XPEQAN = FLISPA(ICODE).EQ.CPAR
      ELSE
        IANT = KCANTI(ICODE,1)
        XPEQAN = FLISPA(IANT).EQ.CPAR
      ENDIF
      END
#endif
