      SUBROUTINE TFILTJ(IER)
C
C---------------------------------------------------------------------
C! Fill bank PTUN with constants for packing and unpacking
C! of TPC data to and from the POT.
C!
C!    Author:   R. Johnson   15-06-88
C!    Modified: W. Wiedenmann 3-10-91
C!              This is a modified version of TFILTU on
C!              ALEPHLIB for JULIA to write out the
C!              packing constants (PTUN) with the run header
C!              
C!              D. Casper 26-03-97
C!              Allow errors to be packed into two bytes instead of
C!              one, to allow accurate refitting.  Scale factors
C!              changed accordingly.
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C!
C!    Called by TINIRU
C!
C---------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "ptunjj.h"
C
      CALL AUBOS('PTUN',0,LPTUNA+LMHLEN,KPTUN,IER)
      IF (IER.EQ.2) RETURN
      IW(KPTUN+LMHCOL)=LPTUNA
      IW(KPTUN+LMHROW)=1
      IW(KPTUN+LMHLEN+JPTUID)= 1
      IW(KPTUN+LMHLEN+JPTUVR)= 1
      IW(KPTUN+LMHLEN+JPTUVR+1)=999
      RW(KPTUN+LMHLEN+JPTUPS)= 10.0E-6
      RW(KPTUN+LMHLEN+JPTUZS)= 1.0E-4
      RW(KPTUN+LMHLEN+JPTURS)= 40.0E-4
      RW(KPTUN+LMHLEN+JPTUSR)= 1.0E-4
      RW(KPTUN+LMHLEN+JPTUSZ)= 1.0E-4
      RW(KPTUN+LMHLEN+JPTUPB)= 40.0E-4
      RW(KPTUN+LMHLEN+JPTUZB)= 80.0E-4
      RW(KPTUN+LMHLEN+JPTUTM)= 0.001
      RW(KPTUN+LMHLEN+JPTUTL)= 0.5
      RW(KPTUN+LMHLEN+JPTUAD)= 1.0
      RW(KPTUN+LMHLEN+JPTURP)= 16.0E-6
C
  999 CONTINUE
      RETURN
      END
#endif
