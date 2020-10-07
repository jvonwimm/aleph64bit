      Real Function YKNEM2(row, parentM, dauM)
C-------------------------------------------------------------------
C
C!  Find the mass^2 of a kink's neutral daughter
C
C     Given a row in the YKNK bank, and a mass for the parent and charged
C     daughter, compute the mass of the neutral and the uncertainty.
C
C     Author:  Paul Rensing     08/04/97
C
C     Inputs:   YKNK bank
C               row         row of YKNK to use
C               parentM     mass of particle which is decaying
C               dauM        mass of charged decay product
C     Returns:  YKNEM2      mass^2 of unobserved neutral decay product
C
C--------------------------------------------------------------------
#ifndef DOC

      implicit none

      integer row
      real parentM, dauM

#include "bcs.h"

#include "yknkjj.h"
#include "bmacrod.h"

      integer NAMIND
      external NAMIND

      integer NamYKNK
      save NamYKNK
      integer IndYKNK

      double precision pp2, pd2, pn2, ep, ed, en

      data NamYKNK/0/

#include "bmacro.h"
      
      if (NamYKNK .eq. 0) NamYKNK = NAMIND('YKNK')

      IndYKNK = IW(NamYKNK)
      if (IndYKNK .le. 0) then
         YKNEM2 = 1000.0
         return
      end if

      pp2 = DBLE(RTABL(indYKNK, row, JYKNPK))**2
      pd2 = DBLE(RTABL(indYKNK, row, JYKNPL))**2 +
     $     DBLE(RTABL(indYKNK, row, JYKNPT))**2
      pn2 = (DBLE(RTABL(indYKNK, row, JYKNPL)) -
     $     DBLE(RTABL(indYKNK, row, JYKNPK)) )**2 +
     $     DBLE(RTABL(indYKNK, row, JYKNPT))**2
      ep = sqrt(pp2 + parentM**2)
      ed = sqrt(pd2 + dauM**2)
      en = ep - ed

      YKNEM2 = en**2 - pn2
      return
      end
#endif
