      Subroutine TMATUT(a1_in,a2_in,isect,ic,aout)
C-----------------------------------------------------------------------
C!
C!    Compute the matrix : aout = (a1)*(T_ic)*(a2**T)
C!    Input : a1_in(3,3,ltsect)
C!            a2_in(3,3,ltsect)
C!            isect           = Sector number
C!            ic              = coordinate number (x=1, y=2, z=3)
C!    Output: aout(3,3)
C!
C-----------------------------------------------------------------------
#ifndef DOC

#include "tpgpar.h"
C
      Dimension a1_in(3,3,ltsect),a2_in(3,3,ltsect),aout(3,3),t(3,3,3)
C
C++   Setup Matrices T
C
      Do k=1,3
         Do i=1,3
            Do j=1,3
               t(i,j,k) = 0.
            Enddo
         Enddo
      Enddo
C.... T_x
      t(2,3,1) = -1.
      t(3,2,1) =  1.
C.... T_y
      t(1,3,2) =  1.
      t(3,1,2) = -1.
C.... T_z
      t(1,2,3) = -1.
      t(2,1,3) =  1.
C
C++   Multiply matrices together
C
      Do i=1,3
         Do j=1,3
            aout(i,j) = 0.
            Do k=1,3
            Do l=1,3
               aout(i,j)=aout(i,j)+
     +                   a1_in(i,k,isect)*t(k,l,ic)*a2_in(j,l,isect)
            Enddo
            Enddo
         Enddo
      Enddo
C
      Return
      End
#endif
