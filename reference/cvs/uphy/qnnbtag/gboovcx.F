      SUBROUTINE GBOOVCX (JVEC4,NBV,GAMMA,  BVEC4)

C----------------------------------------------------------------------
C!
C!   Changed by Stefan Walther                                29.7.1991
C
C!   Purpose   : Boost jet vectors using a BETA derived from GAMMA
C!   Inputs    : JVEC4, NBV, gamma
C!   Outputs   : BVEC4
C!
C!   Called by : Gbjvecx
C!   Calls     : None
C!
C----------------------------------------------------------------------

C - - - - - - - - - - - - - - DECLARATIONS - - - - - - - - - - - - - -
#include "qcde.h"
      Integer   nbv, ivec
      Real      jvec4( 4,* ), bvec4( 4,* ), gamma, beta
#include "qmacro.h"
C - - - - - - - - - - - - - - C O D E - - - - - - - - - - - - - - - - -
C      gamma = sqrt( 1.0/( ( 1.0 - beta )*( 1.0 + beta ) ) )
C      beta  = sqrt( 1.0 - 1/(gamma*gamma) )
C
C                             Apply Lorentz transformation

C COR 2/3/94
      IF(GAMMA.LE.1.) THEN
      BETA = 0.
      ELSE IF (GAMMA.GT.1000.) THEN
      BETA = 1.
      ELSE
      beta  = sqrt( 1.0 - 1.0/(gamma*gamma) )
      ENDIF

      Do  100  ivec = 1,nbv

         bvec4(1,ivec) = gamma*( jvec4(1,ivec) - beta*jvec4(4,ivec) )
         bvec4(2,ivec) =         jvec4(2,ivec)
         bvec4(3,ivec) =         jvec4(3,ivec)
         bvec4(4,ivec) = gamma*( jvec4(4,ivec) - beta*jvec4(1,ivec) )

  100 Continue

      Return
      End

C***********************************************************************
