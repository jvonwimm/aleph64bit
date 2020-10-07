C --------------------------------------------------------
      SUBROUTINE JETFOR (ICLASS,JET,GAMMANAT,ENERGY,MASS,IFLAG,
     &                           XJFM)
C --------------------------------------------------------
C
C Written by Stefan Walther
C University of Mainz                           21.6.1991
C
#include "qcde.h"
      Integer   i, iclass, jet, lenvec, ivec, nbv, ncv, ifalg
      Real      m_B0, gamma, gammaNat, g_b,
     &          energy, mass, E
      Parameter ( lenvec = 200, m_B0 = 5.2794 )    ! GeV/c**2

      Real      eval(3), evec(3,3)
      Real      jvec4(4,lenvec), bvec4(4,lenvec), x, y, z

      Real      XJFM
#include "qmacro.h"
C - - - - - - - - - - - - - - C O D E - - - - - - - - - - - - - - - - -
C
C                             Get vectors for jet JET
C                             with basis vectors given by jet direction
      XJFM = -1.
      Call gjtvec ( iclass,jet,lenvec,  jvec4,nbv)

      gammaNat = energy / mass      ! energy and mass of the jet

C                             Check number in this jet is ok

      If ( nbv .le. lenvec ) Then

         iflag = 1

C  calculate with natural gamma boosted vector
         Call gboovcx (jvec4,nbv,gammaNat,  bvec4)

C  Fragmentation function gives 0.697 for <x>
            x = 0.
            y = 0.
            z = 0.
            Do ivec = 1,nbv
               x = x + jvec4(1,ivec)
               y = y + jvec4(2,ivec)
               z = z + jvec4(3,ivec)
            Enddo
            g_b = Sqrt( (x**2+y**2+z**2)*0.697 / m_B0**2 +1. ) *0.697

         E = g_b *m_B0

C
C  calculate with g_b boosted vector
         Call gboovcx (jvec4,nbv,g_b,  bvec4)

         x = 0.
         y = 0.
         z = 0.
         Do ivec = 1,nbv
C  now take only tracks which are now in bvec4(g_b) forward direction
C  in vector cvec4
            If (bvec4(1,ivec).ge.0.) then
               x = x + bvec4(1,ivec)
               y = y + bvec4(2,ivec)
               z = z + bvec4(3,ivec)
            Endif
         Enddo

C  Impulserhaltung
         XJFM = Sqrt( x**2 +y**2 +z**2 )

      Else

         IFLAG = 0

      Endif

      Return
      End

C----------------------------------------------------------------------
*AF
