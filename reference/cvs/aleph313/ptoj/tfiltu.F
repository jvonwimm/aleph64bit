      SUBROUTINE TFILTU(IER)
C
C---------------------------------------------------------------------
C! Fill bank PTUN with constants for packing and unpacking
C! of TPC data to and from the POT.
C!
C!    Author:   R. Johnson   15-06-88
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C!
C!    Called by TJTOP and TPTOJ
C!
C!    Eventually these constants will be taken from the data base.
C!    For now they are hard wired into this routine.
C!
C---------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "ptunjj.h"
C
      CALL AUBOS('PTUN',0,LPTUNA+LMHLEN,KPTUN,IER)
      IF (IER.EQ.2) RETURN
      IW(KPTUN+LMHCOL)=13
      IW(KPTUN+LMHROW)=1
      IW(KPTUN+LMHLEN+JPTUID)= 1
      IW(KPTUN+LMHLEN+JPTUVR)= 1
      IW(KPTUN+LMHLEN+JPTUVR+1)=999
      RW(KPTUN+LMHLEN+JPTUPS)= 10.0E-6
      RW(KPTUN+LMHLEN+JPTUZS)= 40.0E-4
      RW(KPTUN+LMHLEN+JPTURS)= 40.0E-4
      RW(KPTUN+LMHLEN+JPTUSR)= 40.0E-4
      RW(KPTUN+LMHLEN+JPTUSZ)= 80.0E-4
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
