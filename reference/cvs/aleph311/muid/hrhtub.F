      SUBROUTINE HRHTUB(IER)
C----------------------------------------------------------------------
C
CKEY MUONID HCAL EFFICIENCY / INTERNAL
C
C!  - Remake the HTUB bank based on HCAL tube efficiencies observed
C!    in the data
C!
C!   Author   :- D.Cinabro             20-MAY-1990
C!               G.Taylor              10-APR-1992
C!               Changed to have map for all monte carlo samples
C!               to allow easy incorpoation of MC changes
C!               Change grouping of HCAL layers
C!               G.Taylor              10-APR-1992
C!               map efficiency using HDTE c/w HTEF
C!               ie use modules c/w supermodules
C!
C!   Inputs: HTUB bank
C!   Outputs: Remade HTUB bank
C!            IER /I Error flag
C!                    = 0 ok
C!                    = 1 old galeph version
C!                    = 2 no ASIM bank
C!                    = 3 year not in range 1990-1994
C!                    = 6 no HTUB bank
C!                    = 7 bos garbage collection
C!
C!======================================================================
#ifndef DOC
#include "htubjj.h"
#include "rhahjj.h"
#include "bcs.h"
      INTEGER ALGTDB,JUNIDB
      CHARACTER CHAINT*4
      DATA IERROR/0/
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C Check on GALEPH version used in production
C
      IER=0
      KRHAH = IW(NAMIND('RHAH'))
      KAJOB = IW(NAMIND('AJOB'))
      IVER = 0
      JVER =-1
      DO 3 IST = 1,LROWS(KRHAH)
        IF (CHAINT(ITABL(KRHAH,IST,JRHAPN)).EQ.'GALE')
     &      IVER = ITABL(KRHAH,IST,JRHAPV)
C Write the Julia version in JVER: if Galeph file JVER=-1
        IF (CHAINT(ITABL(KRHAH,IST,JRHAPN)).EQ.'JULI')
     &      JVER = ITABL(KRHAH,IST,JRHAPV)
C
    3 CONTINUE
      IF (IVER.EQ.0.AND.KAJOB.GT.0) IVER = ITABL(KAJOB,1,9)
      IF (IVER.LT.237) THEN
        IF (IW(6).GT.0)
     &  WRITE(IW(6),*) ' *** HRHTUB - Galeph versions < 237 are not ',
     &    ' supported by MUIDO '
        IER=1
        RETURN
      ELSEIF (IVER.LT.241) THEN
C used for 1990 production
        IMCEF=1
      ELSE
C used for 1991 production
        IMCEF=2
      ENDIF
C now find out which year to simulate
      KASIM = IW(NAMIND('ASIM'))
      IF (KASIM.LE.0) THEN
        IF (IW(6).GT.0)
     &  WRITE(IW(6),*) ' ERROR : HRHTUB - NO ASIM BANK ON THIS MC '
        IER=2
        RETURN
      ELSE
        IDEF=(ITABL(KASIM,1,1)/100)
        IF (IDEF.LT.90.OR.IDEF.GT.99) THEN
          IF(IW(6).GT.0)
     &    WRITE(IW(6),*) ' ERROR : HRHTUB - FUNNY YEAR IN ASIM '
          IER=3
          RETURN
        ENDIF
      ENDIF
C
      KHDTEM=MDARD(IW,JUNIDB(0),'HDTE',IMCEF)
      IF(KHDTEM.LE.0) THEN
        IF (IW(6).GT.0.AND.IERROR.EQ.0) THEN
         WRITE(IW(6),*) ' ###################################### '
         WRITE(IW(6),*) ' HRHTUB : ERROR - HDTE BANK NUMBER ',IMCEF,
     &      ' NOT ON THE DATABASE'
         WRITE(IW(6),*) ' HRHTUB : IN this case the HCAL efficiency'
         WRITE(IW(6),*) ' HRHTUB : treatment will not be applied, you'
         WRITE(IW(6),*) ' HRHTUB : will get fully efficient monte carlo'
         WRITE(IW(6),*) ' HRHTUB : Proceed with care........ '
         WRITE(IW(6),*) ' ###################################### '
         IERROR=IERROR+1
        ENDIF
        IER=4
        RETURN
      ENDIF
      KHDTED=MDARD(IW,JUNIDB(0),'HDTE',IDEF)
      IF(KHDTED.LE.0) THEN
        IF (IW(6).GT.0.AND.IERROR.EQ.0) THEN
         WRITE(IW(6),*) ' ###################################### '
         WRITE(IW(6),*) ' HRHTUB : ERROR - HDTE BANK NUMBER ',IDEF,
     &      ' NOT ON THE DATABASE'
         WRITE(IW(6),*) ' HRHTUB : IN this case the HCAL efficiency '
         WRITE(IW(6),*) ' HRHTUB : treatment will not be applied, you'
         WRITE(IW(6),*) ' HRHTUB : will get fully efficient monte carlo'
         WRITE(IW(6),*) ' HRHTUB : Proceed with care... '
         WRITE(IW(6),*) ' ###################################### '
         IERROR=IERROR+1
        ENDIF
        IER=5
        RETURN
      ENDIF
C
C Want to have full efficiency in HTUB 1 and no HTUB 0
C
      KHTUB = NLINK('HTUB',1)
      IF (KHTUB.LE.0) THEN
        KHTUB=NSWAP('HTUB',0,'HTUB',1)
        IF (KHTUB.LE.0) THEN
C         WRITE(6,*) ' ERROR : HRHTUB - No HTUB bank on input file'
          IER=6
          RETURN
        ENDIF
      ELSE
        IND=NDROP('HTUB',0)
      ENDIF
C
      CALL AUBOS('HTUB',0,IW(KHTUB),KHTUB0,IGARB)
      IF(IGARB.EQ.2) THEN
        IDUM=NSWAP('HTUB',0,'HTUB',1)
        IER=7
        RETURN
      ELSE IF(IGARB.EQ.1) THEN
        KHTUB=NLINK('HTUB',1)
        KHDTEM=NLINK('HDTE',IMCEF)
        KHDTED=NLINK('HDTE',IDEF)
      ENDIF
      IW(KHTUB0+1)=IW(KHTUB+1)
      IW(KHTUB0+2)=IW(KHTUB+2)
C
C Look for HLTU banks
C
      KHLTU=NLINK('HLTU',0)
      IF(KHLTU.NE.0) THEN
       NUMRND=LROWS(KHLTU)+LROWS(KHTUB)
      ELSE
       NUMRND=LROWS(KHTUB)
      ENDIF
C
C Look for HZEF bank in database otherwise  usual mapping
C
      KHZEF=MDARD(IW,JUNIDB(0),'HZEF',IDEF)
      IF(KHZEF.LE.0) KHLTU=0
C
C Protection against corrupted HLTU bank (ALPHA job AND Julia < 271)
C
      JALPV=NLINK('ALPV',0)
      IF(JALPV.GT.0.AND.JVER.GT.0.AND.JVER.LE.270) THEN
        IF(IW(6).NE.0.AND.IERROR.EQ.0) THEN
          WRITE(IW(6),*) 'HRHTUB: You are trying to redo extrapolation'
          WRITE(IW(6),*) 'HRHTUB: inside an ALPHA job using MC data '
          WRITE(IW(6),*) 'HRHTUB: processed with JULIA < 271:'
          WRITE(IW(6),*) 'HRHTUB: NO Z-dep. mapping to avoid crash'
          IERROR=IERROR+1
        ENDIF
C
        KHLTU=0
      ENDIF

C
C Loop over the rows of KHTUB
C
      NGOOD = 0
      DO 20 ITUB = 1,LROWS(KHTUB)
        ISUB = ITABL(KHTUB,ITUB,JHTUSN)
        IMOD = ITABL(KHTUB,ITUB,JHTUMN)
        ILAY = ITABL(KHTUB,ITUB,JHTULN)
        IF (ISUB.NE.2) ILAY=ILAY+1
        IF (ISUB.EQ.1) THEN
          IMOD = IMOD
        ELSEIF (ISUB.EQ.2) THEN
          JMOD=IMOD
          IMOD = 6 + IMOD
        ELSE
          IMOD = 30 + IMOD
        ENDIF
        ILAY = (ILAY+2)/2
C Check if we are in the barrel and if HLTU exists
        IF(ISUB.NE.2.OR.KHLTU.LE.0) GOTO 200
C Check if we are in a special module
        IHZRO=0
        DO 140 J=1,LROWS(KHZEF)
         IF(JMOD.EQ.ITABL(KHZEF,J,1).AND.ILAY.LE.ITABL(KHZEF,J,2)) THEN
           IHZRO=J
           GOTO 145
         ENDIF
 140    CONTINUE
        GOTO 200
 145    CONTINUE
C Special module mapping
        DO 150 J=1,LROWS(KHLTU)
         IF(ITABL(KHLTU,J,2).NE.ITUB) GOTO 150
         ZPOS=RTABL(KHLTU,J,1)
C Take into account z sign convention in Galeph
         IF(MOD(JMOD,2).NE.0) ZPOS=-ZPOS
C Compute Z-dependent efficiency
         TEFF=RTABL(KHZEF,IHZRO,3)+
     +        RTABL(KHZEF,IHZRO,4)*ZPOS+
     +        RTABL(KHZEF,IHZRO,5)*ZPOS**2
        CALL ALPROB(1,NUMRND,PROB)
        IF(PROB.LT.TEFF*
     &         RTABL(KHDTED,IMOD,ILAY)/RTABL(KHDTEM,IMOD,ILAY)) THEN

C If at least one is good then the whole cluster is good
          NGOOD = NGOOD + 1
          IPNTO = KROW(KHTUB,ITUB)+1
          IPNTN = KROW(KHTUB0,NGOOD)+1
          CALL UCOPY(IW(IPNTO),IW(IPNTN),LHTUBA)
          GOTO 20
        ENDIF
 150   CONTINUE
C If all the tubes are deleted then the cluster is deleted
       GOTO 20
C
C Usual mapping: End-cap, no supported period, no HLTU , no special mod
C
 200   CONTINUE
C Should it be removed
        CALL ALPROB(1,NUMRND,PROB)
        IF (RTABL(KHDTEM,IMOD,ILAY).LT..001) THEN
          IF (IW(6).GT.0)
     &    WRITE(IW(6),*)' ERROR : QRHTUB - THIS HIT SHOULD NOT BE HERE'
        ELSEIF (PROB/1.01.LT.
     &         RTABL(KHDTED,IMOD,ILAY)/RTABL(KHDTEM,IMOD,ILAY)) THEN
C If not mark it as so in IGOOD array and count in NGOOD
          NGOOD = NGOOD + 1
          IPNTO = KROW(KHTUB,ITUB)+1
          IPNTN = KROW(KHTUB0,NGOOD)+1
          CALL UCOPY(IW(IPNTO),IW(IPNTN),LHTUBA)
        ENDIF
   20 CONTINUE
   21 CONTINUE
C
      IW(KHTUB0+LMHROW) = NGOOD
      IDUM= NBANK('HTUB',0,NGOOD*LHTUBA+LMHLEN)
  999 RETURN
      END
#endif
