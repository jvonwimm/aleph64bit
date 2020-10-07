      SUBROUTINE TPAKBC(IER)
C
C-----------------------------------------------------------------------
C! Transfer the TBCO bank to the PTBC bank
C!
C!    Author:   R. Johnson  16-06-88
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C!
C!    Called by TJTOP
C!
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tbcojj.h"
#include "ptbcjj.h"
#include "ptunjj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTBCO=NAMIND('TBCO')
        NPTUN=NAMIND('PTUN')
        NPTBC=NAMIND('PTBC')
      ENDIF
      KTBCO=IW(NTBCO)
      IF (KTBCO.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
      IF (IW(NPTBC).NE.0) CALL BDROP(IW,'PTBC')
      LEN=LROWS(KTBCO)*LPTBCA+LMHLEN
      IW(1)=1
      CALL AUBOS('PTBC',0,LEN,KPTBC,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=3
        GO TO 999
      ENDIF
      IF (IGARB.EQ.1) THEN
        KPTUN=IW(NPTUN)+LMHLEN
        KTBCO=IW(NTBCO)
      ENDIF
      IW(KPTBC+LMHCOL)=LPTBCA
      IW(KPTBC+LMHROW)=LROWS(KTBCO)
C
      KPTUN=KPTUN+LMHLEN
      DO 500 IC=1,LROWS(KTBCO)
        ID=ITABL(KTBCO,IC,JTBCIN)
        IROW=ID/100000
        ISLOT=(ID-IROW*100000)/1000
        ISTYP=ITPTYP(ISLOT)
        IF (ISTYP.NE.1) THEN
          IROWS=IROW-NTPDRW(1)
        ELSE
          IROWS=IROW
        ENDIF
        R=RTABL(KTBCO,IC,JTBCRV)
        PHI=RTABL(KTBCO,IC,JTBCPH)
        Z=RTABL(KTBCO,IC,JTBCZV)
        CALL TCTSEC(ISLOT,R,PHI,Z,RS,RPHIS,ZS)
        PHIS=RPHIS/RS
        IW(KROW(KPTBC,IC)+JPTBSL)=ISLOT
        IW(KROW(KPTBC,IC)+JPTBSR)=IROWS
        IW(KROW(KPTBC,IC)+JPTBPS)=INT(PHIS/RW(KPTUN+JPTUPB))
        IW(KROW(KPTBC,IC)+JPTBZV)=INT(ZS/RW(KPTUN+JPTUZB))
        IW(KROW(KPTBC,IC)+JPTBNP)=ITABL(KTBCO,IC,JTBCRW)
        IW(KROW(KPTBC,IC)+JPTBNB)=ITABL(KTBCO,IC,JTBCZW)
  500 CONTINUE
      IER=0
C
  999 CONTINUE
      RETURN
      END
#endif
