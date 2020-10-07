************************************************************************
      SUBROUTINE SMSSSS(CPL, LT, AV)
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      complex(16) CPL
      complex(16) AV(0:LASIZE)
*     complex(16) AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate scalar-scalar-scalar-scalar vertex.
*
*           ! 4
*           V
*      -->--+--<---
*        1  ^  3
*           !2
*
*     CPL      : input  : coupling constant.
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*-----------------------------------------------------------------------
      LT(0) = 4
      LT(1) = 1
      LT(2) = 1
      LT(3) = 1
      LT(4) = 1
      AV(0) = CPL
*     CALL CTIME('SMSSSS')
      RETURN
      END
