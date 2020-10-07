*DK hcinit
      SUBROUTINE HCINIT
C------------------------------------------------------
C
C! Initializes the variables used in local and counter commons
C!       at the beginning of the run
C!
C!                                 Author: G.Catanesi 87/10/07
C!
C!      Called by : HCIRUN
C!      Calls     : none
C!
C --------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "hccoun.h"
#include "hcloc.h"
C -----------------------------------------------------------------
C  Reset to zero the common HCCOUN
C
      NHCC01 = 0
      NHCC02 = 0
      NHCC03 = 0
      NHCC04 = 0
      NHCC05 = 0
      HCEAVE = 0.
      HCANST = 0.
C
C  Reset the common HCLOC
C
      IHCPOR = 0
      IHCMOD = 0
      IHCIPL = 0
C
      HCPDIP = 0.
      HCPPHI = 0.
      HCPACX = 0.
      HCPACY = 0.
      HCPACZ = 0.
      HCPAX0 = 0.
      HCPAY0 = 0.
      HCPAZ0 = 0.
      FHADRC = .FALSE.
      FHCPRJ = .FALSE.
C
      RETURN
      END
#endif
