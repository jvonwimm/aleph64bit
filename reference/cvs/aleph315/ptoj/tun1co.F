      SUBROUTINE TUN1CO(IC,ITPCO,RTPCO,IER)
C
C---------------------------------------------------------------------
C! Unpack single coordinate from PTCO
C!
C!    Author:  R. Johnson    15-06-88
C!
C!    Input:  IC       /I         Coordinate number to unpack
C!    Output: ITPCO(6) /I         Integer attributes of bank TPCO
C!            RTPCO(6) /R         Real attributes of bank TPCO
C!            IER      /I         Error return= nonzero if coordinate
C!                                cannot be found.
C!     Called by PTPCOJ
C!
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "bcs.h"
#include "ptcojj.h"
#include "tpcojj.h"
#include "tpgpar.h"
#include "tpgeop.h"
#include "tpgeom.h"
#include "ptunjj.h"
C
      DIMENSION ITPCO(*),RTPCO(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTCO=NAMIND('PTCO')
        NPTUN=NAMIND('PTUN')
      ENDIF
      KPTCO=IW(NPTCO)
      IF (KPTCO.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
      KPTUN=KPTUN+LMHLEN
      IF (IC.LT.1 .OR. IC.GT.LROWS(KPTCO)) THEN
        IER=3
        GO TO 999
      ENDIF
      IROW=ITABL(KPTCO,IC,JPTCSR)
      ISLOT=ITABL(KPTCO,IC,JPTCSL)
      ISTYP=ITPTYP(ISLOT)
      IF (ISTYP.NE.1) THEN
        IROWG=IROW+NTPDRW(1)
      ELSE
        IROWG=IROW
      ENDIF
C
C++   The following IF statement allows one to read MC events
C++   generated before JPTCDR was added to the end of the PTCO bank.
C
      IF (JPTCDR.LE.LCOLS(KPTCO)) THEN
        DR= FLOAT(ITABL(KPTCO,IC,JPTCDR))*RW(KPTUN+JPTURS)
      ELSE
        DR= 0.
      ENDIF
      RG= TPPROW(IROWG)+DR
      PHI=FLOAT(ITABL(KPTCO,IC,JPTCPS))*RW(KPTUN+JPTUPS)
      DPHI=PHI+TPDPHW(IROW,ISTYP)
      PAD= (RG*DPHI)/TPDSEP(ISTYP)
      IPAD=MIN(NTPDPR(IROW,ISTYP)+2,INT(PAD+1.5))
      IPAD=NTPDPR(IROW,ISTYP)+2 - IPAD
      ITPCO(JTPCIN)=IROWG*100000 + ISLOT*1000 + IPAD
      ZS= FLOAT(ITABL(KPTCO,IC,JPTCZV))*RW(KPTUN+JPTUZS)
C
C++   Convert to the global reference frame
C
      IF (IENDTP(ISLOT).EQ.2) THEN
        PHIG= PHI + TPPHI0(ISLOT)
        ZG= ZS - ZTPCMX
      ELSE
        PHIG= TPPHI0(ISLOT) - PHI
        ZG=  ZTPCMX - ZS
      ENDIF
      IF (PHIG.GT.TWOPI) THEN
        PHIG=PHIG-TWOPI
      ELSEIF (PHIG.LT.0.) THEN
        PHIG=PHIG+TWOPI
      ENDIF
C
      RTPCO(JTPCRV)=RG
      RTPCO(JTPCPH)=PHIG
      RTPCO(JTPCZV)=ZG
      RTPCO(JTPCSR)=(FLOAT(ITABL(KPTCO,IC,JPTCSP))*RW(KPTUN+JPTUSR))**2
      RTPCO(JTPCSZ)=(FLOAT(ITABL(KPTCO,IC,JPTCSZ))*RW(KPTUN+JPTUSZ))**2
      ITPCO(JTPCOF)=0
      ITPCO(JTPCTN)=0
      ITPCO(JTPCCN)=0
      ITPCO(JTPCIT)=0
      IER=0
C
  999 CONTINUE
      RETURN
      END
#endif
