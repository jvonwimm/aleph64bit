      SUBROUTINE TUN1NC(IC,ITK,ITPCO,RTPCO,IER)
C
C---------------------------------------------------------------------
C! Unpack single coordinate from PTNC
C!
C!    Author:  R. Johnson    17-06-90
C!
C!    Input:  IC       /I         Coordinate number to unpack
C!            ITK      /I         Track number in FRFT
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
#include "ptncjj.h"
#include "tpcojj.h"
#include "tpgpar.h"
#include "tpgeop.h"
#include "tpgeom.h"
#include "ptunjj.h"
C
      DIMENSION ITPCO(*),RTPCO(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./,MODE/1/
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTNC=NAMIND('PTNC')
        NPTUN=NAMIND('PTUN')
      ENDIF
      KPTNC=IW(NPTNC)
      IF (KPTNC.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
      KPTUN=KPTUN+LMHLEN
      IF (IC.LT.1 .OR. IC.GT.LROWS(KPTNC)) THEN
        IER=3
        GO TO 999
      ENDIF
      IROW=ITABL(KPTNC,IC,JPTNSR)
      ISLOT=ITABL(KPTNC,IC,JPTNSL)
      ISTYP=ITPTYP(ISLOT)
      IF (ISTYP.NE.1) THEN
        IROWG=IROW+NTPDRW(1)
      ELSE
        IROWG=IROW
      ENDIF
C
C++   The new procedure is to store raw sector coordinates on the
C++   POT, from which TPCO can be derived by applying all alignment
C++   and field corrections.
C
      RPHIS= FLOAT(ITABL(KPTNC,IC,JPTNRP))*RW(KPTUN+JPTURP)
      ZS=    FLOAT(ITABL(KPTNC,IC,JPTNZV))*RW(KPTUN+JPTUZS)
      RS=    TPPROW(IROWG)
C
C++   Transform the z coordinate according changes in drift velocity
C++   and t0
C
      CALL TCRZVD(IENDTP(ISLOT),ZS,ZCR)
      ZS = ZCR
C
C++   Transform the coordinate to the ALEPH frame of reference,
C++   including all necessary alignment corrections.
C
      CALL TCTGLB(ISLOT,RS,RPHIS,ZS,R,RPHI,Z)
C
C++   Correct coordinate z for time-of-flight
C
      CALL TCRTOF(ALFIEL(IROW),R,Z,ITK,'FRFT',ZCR)
      Z=ZCR
C
C++   Correct for drift field distortions
C
      PHI= RPHI/R
      CALL TLACOR(R,PHI,Z,RCR,PHICR,ZCR,MODE)
      R=RCR
      PHI=PHICR
      ZG=ZCR
      CALL TCRTRA(IENDTP(ISLOT),R,PHI,ZG,RG,PHIG)
C
C++   Correct for z distortions measured by VDET
C
      CALL TZCRVD(IENDTP(ISLOT),RG,ZG,ZCR)
      ZG=ZCR
C
C++   Correct for Rz sector alignment measured by VDET
C
      CALL TZCSVD(ISLOT,RG,PHIG,ZG,ZCR)
      ZG=ZCR
C
C++   Correct for Residual field distortions
C
      CALL TCORES(IENDTP(ISLOT),IROWG,RG,PHIG,ZG,RCR,PHICR,ZCR)
      RG  =RCR
      PHIG=PHICR
      ZG  =ZCR
      CALL TFICOR(IENDTP(ISLOT),IROWG,RG,PHIG,ZG,RCR,PHICR,ZCR)
      RG  =RCR
      PHIG=PHICR
      ZG  =ZCR
C
      ITPCO(JTPCIN)=100000*IROWG+1000*ISLOT
      RTPCO(JTPCRV)=RG
      RTPCO(JTPCPH)=PHIG
      RTPCO(JTPCZV)=ZG
      RTPCO(JTPCSR)=(FLOAT(ITABL(KPTNC,IC,JPTNSP))*RW(KPTUN+JPTUSR))**2
      RTPCO(JTPCSZ)=(FLOAT(ITABL(KPTNC,IC,JPTNSZ))*RW(KPTUN+JPTUSZ))**2
      ITPCO(JTPCOF)=0
      ITPCO(JTPCTN)=0
      ITPCO(JTPCCN)=0
      ITPCO(JTPCIT)=0
      RTPCO(JTPCRR)=RPHIS
      RTPCO(JTPCRZ)=ZS
      IER=0
C
  999 CONTINUE
      RETURN
      END
#endif
