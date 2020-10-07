      SUBROUTINE HADINT(itk,start,xlen)
C--------------------------------------------------------------
C  Hadronic interactions
C  input:  itk = MC track
C  output: start = # of interaction length where shower started
C          xlen  = # depth of shower in int. length
C--------------------------------------------------------------
#include "ftrig.h"
#include "qcde.h"
C
#include "qmacro.h"
C
      xnucl = -ALOG(RNDM(xnucl))           ! 1st inter. at XNUCL Int. length
      if ( xnucl .gt. LambdaECAL ) then    !  do no interact in ECAL
        start = xnucl - LambdaECAL
        remain = 1.
      else
        start = 0.
        remain = (1.-xnucl/LambdaECAL)     ! fraction left in ECAL
      endif
      call rannor(gauss1,gauss2)
      xlen = 0.2*alog(max(qe(itk)*remain,1.))+0.7 ! position of maximum
     &     + max(0.055*qe(itk)+.3*gauss1,0.)      ! phenomenological ....

      return
      end
