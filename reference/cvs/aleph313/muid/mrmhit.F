      SUBROUTINE MRMHIT(IER)
C-----------------------------------------------------------------------
C
CKEY MUONID MUON EFFICIENCY / INTERNAL
C
C!    mask MonteCarlo bank MHIT to take into account
C!    muon chambers inefficiencies
C!
C!    authors: G.Capon and F.Cerutti    10-JUN-1990
C!    revised: G.Taylor                 20-APR-1992
C!             G.Taylor                 21-OCT-1992
C!              get the monte carlo eff from
C!              the database
C!
C!    output : IER/I = Error flag
C!                    = 0 ok
C!                    = 1 no ASIM bank
C!                    = 2 year not in range 1990-1994
C!                    = 3 corresponding MCPE bank not on database
C!                    = 4 no MHIT bank
C!                    = 5 bos garbage collection
C!
C=======================================================================
#ifndef DOC
#include "rhahjj.h"
#include "mhitjj.h"
#include "bcs.h"
      LOGICAL BADEVT
      INTEGER IOFFS(3)
      DATA IOFFS/0,16,54/
      INTEGER ALGTDB,JUNIDB
      DATA IERROR/0/
#include "bmacro.h"
C-----------------------------------------------------------------------
      IER=0
C
C now find out which year to simulate
C
      KASIM = IW(NAMIND('ASIM'))
      IF (KASIM.LE.0) THEN
        IF(IW(6).GT.0)
     &  WRITE(6,*) ' ERROR : MRMHIT - NO ASIM BANK ON THIS MC '
        IER=1
        RETURN
      ELSE
        IDEF=(ITABL(KASIM,1,1)/100)
        IF (IDEF.LT.90.OR.IDEF.GT.99) THEN
          IF(IW(6).GT.0)
     &    WRITE(IW(6),*) ' ERROR : MRMHIT - FUNNY YEAR IN ASIM '
          IER=2
          RETURN
        ENDIF
      ENDIF
      IMCEF=IDEF-89
C
      KMCPED=MDARD(IW,JUNIDB(0),'MCPE',IDEF)
      IF(KMCPED.LE.0) THEN
        IF (IW(6).GT.0.AND.IERROR.EQ.0) THEN
         WRITE(IW(6),*) ' ###################################### '
         WRITE(IW(6),*) ' MRMHIT : ERROR - MCPE BANK NUMBER ',IDEF,
     &      ' NOT ON THE DATABASE'
         WRITE(IW(6),*) ' MRMHIT : IN this case the Muon Chamber '
         WRITE(IW(6),*) ' MRMHIT : efficiency '
         WRITE(IW(6),*) ' MRMHIT : treatment will not be applied, you'
         WRITE(IW(6),*) ' MRMHIT : will get fully efficient monte carlo'
         WRITE(IW(6),*) ' MRMHIT : Proceed with care........ '
         WRITE(IW(6),*) ' ###################################### '
         IERROR=IERROR+1
        ENDIF
        IER=4
        RETURN
      ENDIF
C
      KMCPEM=MDARD(IW,JUNIDB(0),'MCPE',IMCEF)
      IF(KMCPEM.LE.0) THEN
       KMCPEM=MDARD(IW,JUNIDB(0),'MCPE',1)
       IF(KMCPEM.LE.0) THEN
        IF (IW(6).GT.0.AND.IERROR.EQ.0) THEN
         WRITE(IW(6),*) ' ====================================== '
         WRITE(IW(6),*) ' MRMHIT : ERROR - MCPE BANK NUMBER ',IMCEF,
     &      ' NOT ON THE DATABASE'
         WRITE(IW(6),*) ' MRMHIT : IN this case the Muon Chamber '
         WRITE(IW(6),*) ' MRMHIT : efficiency '
         WRITE(IW(6),*) ' MRMHIT : treatment will not be applied, you'
         WRITE(IW(6),*) ' MRMHIT : will get fully efficient monte carlo'
         WRITE(IW(6),*) ' MRMHIT : Proceed with care........ '
         WRITE(IW(6),*) ' ====================================== '
         IERROR=IERROR+1
        ENDIF
        IER=4
        RETURN
       ENDIF
      ENDIF
C
C
C if 93 mc then delete hits in 4c and 4d for .256 of the events
      BADEVT=.FALSE.
      IF(IDEF.EQ.93) THEN
       CALL ALPROB(3,1,PROBEV)
       IF(PROBEV.LT..256) BADEVT=.TRUE.
       IND=NDROP('D4CD',0)
      CALL AUBOS('D4CD',0,LMHLEN+1,JD4CD,IGARB)
      IF(IGARB.EQ.2) THEN
        IDUM=NSWAP('MHIT',0,'MHIT',1)
        IER=6
        IF (IW(6).GT.0.AND.IERROR.EQ.0) THEN
         WRITE(IW(6),*) ' ====================================== '
         WRITE(IW(6),*) ' MRMHIT : UNABLE TO CREATE D4CD BANK'
         WRITE(IW(6),*) ' ====================================== '
         IERROR=1
        ENDIF
        RETURN
      ELSE IF(IGARB.EQ.1) THEN
        KMHIT=NLINK('MHIT',1)
        KMHIT0=NLINK('MHIT',0)
        KMCPED=NLINK('MCPE',IDEF)
      ENDIF
       IW(JD4CD+LMHCOL)=1
       IW(JD4CD+LMHLEN)=1
       IW(JD4CD+LMHLEN+1)=1
       IF(BADEVT) IW(JD4CD+LMHLEN+1)=-1
      ENDIF
C
C Want to have full efficiency in MHIT 1 and no MHIT 0
C
      KMHIT = NLINK('MHIT',1)
      IF (KMHIT.LE.0) THEN
        KMHIT=NSWAP('MHIT',0,'MHIT',1)
        IF (KMHIT.LE.0) THEN
          IER=4
          RETURN
        ENDIF
      ELSE
        IND=NDROP('MHIT',0)
      ENDIF
C
      CALL AUBOS('MHIT',0,IW(KMHIT),KMHIT0,IGARB)
      IF(IGARB.EQ.2) THEN
        IDUM=NSWAP('MHIT',0,'MHIT',1)
        IER=5
        RETURN
      ELSE IF(IGARB.EQ.1) THEN
        KMHIT=NLINK('MHIT',1)
        KMCPED=NLINK('MCPE',IDEF)
      ENDIF
      IW(KMHIT0+1)=IW(KMHIT+1)
      IW(KMHIT0+2)=IW(KMHIT+2)
C
C drop rows to be deleted in MHIT bank
C
      NGOOD=0
      DO 10 IHIT=1,LROWS(KMHIT)
        NSUB= ITABL(KMHIT,IHIT,JMHISC)
        NSLOT=ITABL(KMHIT,IHIT,JMHISN)
        IPLAN=ITABL(KMHIT,IHIT,JMHILN)
        KSLOT=NSLOT+IOFFS(NSUB)
C           generate a vector of random numbers
        CALL ALPROB(2,LROWS(KMHIT),PROB)
        IF (PROB.GT.RTABL(KMCPED,KSLOT,IPLAN)/
     &               RTABL(KMCPEM,KSLOT,IPLAN)) GO TO 10
C drop hits in 4c and 4d if this is a bad event
        IF(BADEVT.AND.NSUB.EQ.3.AND.
     +  (NSLOT.EQ.4.OR.NSLOT.EQ.5.OR.
     +   NSLOT.EQ.21.OR.NSLOT.EQ.22)) GOTO 10
        NGOOD=NGOOD+1
        IPNTO = KROW(KMHIT,IHIT)+1
        IPNTN = KROW(KMHIT0,NGOOD)+1
        CALL UCOPY(IW(IPNTO),IW(IPNTN),LMHITA)
   10 CONTINUE
C
      IW(KMHIT0+LMHROW) = NGOOD
      IDUM= NBANK('MHIT',0,NGOOD*LMHITA+LMHLEN)
  999 RETURN
      END
#endif
