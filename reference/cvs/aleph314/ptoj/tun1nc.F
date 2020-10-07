      SUBROUTINE TUN1NC(IC,ITK,ITPCO,RTPCO,IER)
C---------------------------------------------------------------------
C! Unpack single coordinate from PTNC
C!
C!    Author:  R. Johnson    17-06-90
C!    Modified:D. Casper     07-06-96   Average in wire coordinates, unless
C!                                      a TNWZ card is present.  Also,
C!                                      automatically compute coordinate
C!                                      errors, and load in TPCO.
C!             D. Casper     23-04-97   Ignore the wires if a TOAL card
C!                                      is present, since they won't be
C!                                      right.
C!
C!    Input:  IC       /I         Coordinate number to unpack
C!            ITK      /I         Track number in FRFT
C!    Output: ITPCO(12)/I         Integer attributes of bank TPCO
C!            RTPCO(12)/R         Real attributes of bank TPCO
C!            IER      /I         Error return= nonzero if coordinate
C!                                cannot be found.
C!     Called by PTPCOJ
C!
C      changes:
C      - print statement in first call
C      - wire flag stored in jtpcit
C      - modify call to TCRTRA->TCRTRN
C      - correct bug 100000*IROWG+1000*ISLOT
C                               | was irow
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
#include "frftjj.h"
#include "ptunjj.h"
#include "pcoijj.h"
#include "twzzjj.h"
C
      DIMENSION ITPCO(*),RTPCO(*)
      DIMENSION S(2),PHIP(2),ZP(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./,MODE/1/
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTNC=NAMIND('PTNC')
        NPTUN=NAMIND('PTUN')
        NTWZZ=NAMIND('TWZZ')
        NPCOI=NAMIND('PCOI')
        NFRFT=NAMIND('FRFT')
        KTNWZ=IW(NAMIND('TNWZ'))
        KTOAL=IW(NAMIND('TOAL'))
        IF (KTNWZ.EQ.0 .AND. KTOAL.EQ.0) THEN
          CALL ALTELL(
     &     'TUN1NC - TPC wire coordinates will be used (if present)',
     &     0,'RETURN')
        ELSE
          CALL ALTELL(
     &      'TUN1NC - TPC wire coordinates disabled by user',
     &      0,'RETURN')
        ENDIF
      ENDIF
C
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
      CALL TCRTOF(ALFIEL(DUM),R,Z,ITK,'FRFT',ZCR)
      Z=ZCR
C
C++   Correct for drift field distortions
C
      PHI= RPHI/R
      CALL TLACOR(R,PHI,Z,RCR,PHICR,ZCR,MODE)
      R=RCR
      PHI=PHICR
      Z=ZCR
      CALL TCRTRN(ISLOT,R,PHI,Z,RG,PHIG,ZG)
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
C++   Load errors packed in PTNC
C
      SIGRPHI2 = (FLOAT(ITABL(KPTNC,IC,JPTNSP))*RW(KPTUN+JPTUSR))**2
      SIGZ2    = (FLOAT(ITABL(KPTNC,IC,JPTNSZ))*RW(KPTUN+JPTUSZ))**2
      ICOR     = 0
      iwflg    = 0
C
      KPCOI=IW(NPCOI)
      IF (ITK.LE.0 .OR. KPCOI.EQ.0) GOTO 998
C
C++   Check whether a wire coordinate is associated with this guy.
C
      ICOR = ITABL(KPCOI,IC,JPCOTF)/10
      IWCO = ICOR/100
      KTWZZ = IW(NTWZZ)
      IF (IWCO .GT. 0 .AND. KTWZZ.GT.0 .AND. KTNWZ.EQ.0
     &      .AND. KTOAL.EQ.0)THEN
        IF(IWCO.LE.LROWS(KTWZZ))THEN
C
C++   set wire flag
C
            iwflg = 1
C
            ITWZZ = KROW(KTWZZ,IWCO)
            SZW2 = RW(ITWZZ+JTWZSZ)
C
C++   Average the wire and pad coordinates
C
            WTP = 1./SIGZ2
            WTW = 1./SZW2
            ZSP = FLOAT(ITABL(KPTNC,IC,JPTNZV))*RW(KPTUN+JPTUZS)
            ZS = (WTP*ZSP + WTW*(ZSP+RW(ITWZZ+JTWZDZ)))/(WTP+WTW)
            SIGZ2 = 1./(WTW+WTP)
C
C++   Transform the combined z coordinate according changes in drift velocity
C++   and t0
C
            CALL TCRZVD(IENDTP(ISLOT),ZS,ZCR)
            ZS = ZCR
C
C++   Transform the combined coordinate to the ALEPH frame of reference,
C++   including all necessary alignment corrections.
C
            CALL TCTGLB(ISLOT,RS,RPHIS,ZS,R,RPHI,Z)
C
C++   Correct combined coordinate z for time-of-flight
C
            CALL TCRTOF(ALFIEL(DUM),R,Z,ITK,'FRFT',ZCR)
            Z=ZCR
C
C++   Correct for drift field distortions
C
            PHI= RPHI/R
            CALL TLACOR(R,PHI,Z,RCR,PHICR,ZCR,MODE)
            R=RCR
            PHI=PHICR
            Z=ZCR
            CALL TCRTRN(ISLOT,R,PHI,Z,RG,PHIG,ZG)
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
        ENDIF
      ENDIF
C
 998  CONTINUE
      ITPCO(JTPCIN)=100000*IROWG+1000*ISLOT
      RTPCO(JTPCRV)=RG
      RTPCO(JTPCPH)=PHIG
      RTPCO(JTPCZV)=ZG
      RTPCO(JTPCSR)=SIGRPHI2
      RTPCO(JTPCSZ)=SIGZ2
      ITPCO(JTPCOF)=ICOR*10
      ITPCO(JTPCTN)=0
      ITPCO(JTPCCN)=0
      ITPCO(JTPCIT)=iwflg
      RTPCO(JTPCRR)=RPHIS
      RTPCO(JTPCRZ)=ZS
      IER=0
C
  999 CONTINUE
      RETURN
      END
#endif
