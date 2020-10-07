      SUBROUTINE TLCTDP(IUNIT)
C
C-----------------------------------------------------------------------
C! Dump TPC cluster cuts
C!
C!    Author:  R. Johnson    3-5-87
C!
C!    Input:  IUNIT    /I     Fortran logical unit number for output
C!
C!    Called by TRNCON
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "rcurnt.h"
#include "tcluct.h"
C
      WRITE(IUNIT,10) IRUNRC
   10 FORMAT(/' Dump of TPC cluster cuts (common TCLUCT) at run ',I5/
     &        ' Definitions of variables are in DDL for bank TLCT.')
      WRITE(IUNIT,19) ITLCPT,ITLCPC,RTLCDM
      WRITE(IUNIT,20) ITLCML,RTLCTL,ITLCTA,ITLCCA,FTLCOO,
     &                FTLCAF,ITLCNP,ITLCXP,ITLCIB,ITLCAB
      WRITE(IUNIT,21) RTLCZW,ITLCPM,ITLCRM,ITLCOC,
     &                ITLCOP,ITLCFA,FTLCQK,RTLCFT,ITLCMS
      RETURN
   19 FORMAT(/' PulseThresh=',T20,I3/
     &        ' PeakCut=',T20,I3/
     &        ' DefMax=',T20,3(F8.2,1X))
   20 FORMAT( ' MinimumLen=',T20,I3/
     &        ' TanLdivide=',T20,F8.2/
     &        ' TimeAlg=',T20,I3,1X,I3/
     &        ' ChargeAlg=',T20,I3,1X,I3/
     &        ' OverlapOpt=',T20,L3,1X,L3/
     &        ' AlgForce=',T20,L3/
     &        ' miNPulses=',T20,I3/
     &        ' maXPulses=',T20,I3/
     &        ' mInBuckets=',T20,I3/
     &        ' mAxBuckets=',T20,I3)
   21 FORMAT( ' ZWindow=',T20,2(F8.2,1X)/
     &        ' PeakMin=',T20,I3/
     &        ' RphiMxdef=',T20,I3/
     &        ' OverCut=',T20,I3/
     &        ' OverParm=',T20,I3/
     &        ' FullAnalysis=',T20,I3/
     &        ' QuicKanalysis=',T20,L3/
     &        ' FractThresh=',T20,F8.2/
     &        ' MaxSaturate=',T20,I3)
      END
#endif
