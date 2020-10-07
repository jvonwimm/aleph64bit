      SUBROUTINE HPRANP
C-------------------------------------------------------------------
C! prepare Hcal analog data for POT reprocessing
C
C      R.Tenchini           date : 890720
C
C!      fill HSDA bank and call HSTOFI
C!
C!
C!      called by HPREDA
C?
C!======================================================================
#ifndef DOC
      EXTERNAL HINTST,HNREG
      LOGICAL TEST,HINTST
      INTEGER HNREG
#include "bcs.h"
#include "hcnamc.h"
#include "hstadd.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "hcjosu.h"
#include "hsdajj.h"
#include "hstojj.h"
#include "phstjj.h"
#include "bmacro.h"
      DATA       NERR/0/
      JPHST=NLINK('PHST',0)
      IF (JPHST.EQ.0) GO TO 99
      NPHST=LROWS(JPHST)
C
C?             create HSDA bank
C
      LNHSD=LMHLEN+NPHST*LHSDAA
      CALL AUBOS('HSDA',0,LNHSD,JHSDA,IGARB)
      IF (IGARB.EQ.2) THEN
        CALL RERROR('HPRANP',1,'no space for HSDA bank')
        RETURN
      ELSE IF (IGARB.EQ.1) THEN
        JPHST=NLINK('PHST',0)
      ENDIF
C
C?             create HSTO bank
C
      LNHST=LMHLEN+NPHST*LHSTOA
      CALL AUBOS('HSTO',0,LNHST,JHSTO,IGARB)
      IF (IGARB.EQ.2) THEN
        CALL RERROR('HPRANP',2,'no space for HSTO bank')
        RETURN
      ELSE IF(IGARB.EQ.1) THEN
        JPHST=NLINK('PHST',0)
        JHSDA=NLINK('HSDA',0)
      ENDIF
      KHSTO=JHSTO+LMHLEN
      IW(JHSDA+LMHCOL)=LHSDAA
      IW(JHSDA+LMHROW)=NPHST
      IW(JHSTO+LMHCOL)=LHSTOA
      IW(JHSTO+LMHROW)=NPHST
C
C?             loop on fired towers
C
      DO 10 N=1,NPHST
        IADDHS=0
        IPHIHS=ITABL(JPHST,N,JPHSPI)
        ITHETA=ITABL(JPHST,N,JPHSTI)
        IF(ITHETA.GT.128) THEN
          ISTKHS=2
          ITETHS=ITHETA-128
        ELSE
          ISTKHS=1
          ITETHS=ITHETA
        ENDIF
        IREGHS=HNREG(ITETHS)
C?             check indices within expected range
        TEST=HINTST(ITETHS,IPHIHS,1)
        IF(.NOT.TEST)THEN
          NERR=NERR+1
          IF (NERR.GT.10) GO TO 10
          WRITE(LDEBRL,*)
     +      ' routine HPRAND:Hcal analog address out of range - ',
     +      'RUN,EV = ', IRUNRC,IEVTRC
          WRITE(LDEBRL,*)' nregio,indphi,inteta =',IREGHS,IPHIHS,
     +      ITETHS
          GO TO 10
        ENDIF
C?           get module #, subcomp #
        CALL HFNDMD(ITETHS,IPHIHS,ISTKHS,ISUBHS,IMODHS,IOVRHS)
        EGEV=RTABL(JPHST,N,JPHSCE)
        NSTOHC(ISTKHS)=NSTOHC(ISTKHS)+1
        IW(KROW(JHSDA,N)+JHSDTI)=ITETHS
        IW(KROW(JHSDA,N)+JHSDPI)=IPHIHS
        IW(KROW(JHSDA,N)+JHSDSN)=ISTKHS
        RW(KROW(JHSDA,N)+JHSDDE)=EGEV
        ETOTHC(ISTKHS)=ETOTHC(ISTKHS)+EGEV
        IW(KROW(JHSDA,N)+JHSDSC)=ISUBHS
        IW(KROW(JHSDA,N)+JHSDMN)=IMODHS
        IW(KROW(JHSDA,N)+JHSDRN)=IREGHS
        IW(KROW(JHSDA,N)+JHSDCN)=0
        IW(KROW(JHSDA,N)+JHSDNS)=0
        CALL HSTOFI(KHSTO)
   10 CONTINUE
      IPOTG=1
      CALL HCALIB(IPOTG)
*/----------------------------------------------------------
   99 RETURN
      END
#endif
