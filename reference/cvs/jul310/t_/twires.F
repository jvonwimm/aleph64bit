      SUBROUTINE TWIRES(IER)
C
C---------------------------------------------------------------------
C! Create TPC wire-pulses.
C!
C!   Author:   R. Johnson   20-07-89
C!   Modified: R. Johnson   21-01-91 new reduction algorithm
C!   Modified: W. Wiedenmann 3-02-92 new banks TSIR,TSDI
C!   Modified: R. Johnson   24-08-92 handle pulse length banks
C!   Modified: R. Johnson    4-09-93 split double peaked pulses
C!                                   and add reference bank TWRF
C!                                   Obsolete TWPU bank removed.
C!
C!   Output: IER       /I    Error code:  1 = we ran out of space
C!                                        2 = input banks missing
C!                                        0 = space is OK
C!   Input banks:
C!           TSIR,TSDI,TSLE  Wire hits and digitizations
C!   Output banks:
C!           TWRF       Wire pulses and pointers (optional)
C!           TWRR,TWLE       Reduced TRIR bank and pulse length bank
C!
C!   Called from TPREDA
C!
C!   Description
C!   ===========
C!     Banks TWRR and TWLE are created and filled for sector ISLOT.
C!     TWRF contains pointers from TWRR back to TSIR and TSDI.
C!     It is optional (with the TOPT 'TWPU' card) and is not
C!     used further by JULIA.  It is useful for debugging.
C!
C!   NOTE: This routine represents a module within JULIA.
C!--------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tpgpar.h"
#include "bcs.h"
#include "tpgeom.h"
#include "tpgeow.h"
#include "tpccon.h"
#include "twirct.h"
#include "tcluct.h"
#include "tzcorr.h"
#include "tflags.h"
C
C----- Local variables -------------------------------------------------
C
      PARAMETER (MXSMP=512,MXHITS=2)
      DIMENSION ISPH(MXSMP),IQ(MXHITS),TIME(MXHITS),LENGTH(MXHITS)
      DIMENSION IQUAL(MXHITS),IFLG(MXHITS)
      CHARACTER TEXT*60
      LOGICAL FIRST
      LOGICAL FSMART
      CHARACTER NAM1*4,NAM2*4,NAM3*4
      DIMENSION IBTOR(0:7)
      SAVE FIRST,NTSIR,NTRIR,NTSDI,NTRDI,NTWRR,NTWLE,NTWRF
      DATA FIRST/.TRUE./
      DATA IBTOR/24,28,16,20,8,12,0,4/
      DATA IAVGPL/7/          !Average pulse length of pulses reduced
                              !in the TPP.
C
C----- Function definitions --------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTSIR=NAMIND('TSIR')
        NTRIR=NAMIND('TRIR')
        NTSDI=NAMIND('TSDI')
        NTRDI=NAMIND('TRDI')
        NTWRR=NAMIND('TWRR')
        NTWLE=NAMIND('TWLE')
        NTWRF=NAMIND('TWRF')
      ENDIF
C
C++   Check that the wire hit bank is present
C
      IF ((IW(NTRIR).EQ.0).AND.(IW(NTSIR).EQ.0)) THEN
        IER=2
        GO TO 999
      ENDIF
C
C++   Drop any output banks still lingering around
C
      IF (IW(NTWRR).NE.0) CALL BDROP(IW,'TWRR')
      IF (IW(NTWLE).NE.0) CALL BDROP(IW,'TWLE')
      IF (IW(NTWRF).NE.0) CALL BDROP(IW,'TWRF')
C
C++   Put the output banks on the temporary list (not DST banks)
C
      IF (FWIRPU) THEN
        CALL BLIST(IW,'T+','TWRF')
      ENDIF
      CALL BLIST(IW,'T+','TWRRTWLE')
C
C++   Loop over the sectors
C
      KTSIR=IW(NTSIR)
      IF (KTSIR.NE.0) THEN
         FSMART=.TRUE.
         NAM1='TSIR'
         NAM2='TSDI'
         NAM3='TSLE'
         KTRIR=KTSIR
      ELSE
         FSMART=.FALSE.
         NAM1='TRIR'
         NAM2='TRDI'
         NAM3='TRLE'
         KTRIR=IW(NTRIR)
      ENDIF
  310 IF (KTRIR.EQ.0) GO TO 410
        ISLOT=IW(KTRIR-2)
        IF (ISLOT.LE.0 .OR. ISLOT.GT.LTSECT) THEN
          CALL RERROR('TWIRES',3,'Sector number out of range in '//NAM1)
          KTRIR=IW(KTRIR-1)
          GO TO 310
        ENDIF
        ISTYP=ITPTYP(ISLOT)
C
        ITHRS= JTHRTW+JPEDST(ISLOT,2)
        NWHIT=IW(KTRIR)
        IF (NWHIT.LE.0) GO TO 400
C
C++     Create wire pulse banks for this sector
C
        IF (FWIRPU) THEN
          IW(1)=1
          CALL AUBOS('TWRF',ISLOT,2*NWHIT+LMHLEN,KTWRF,IGARB)
          IF (IGARB.EQ.2) GO TO 998
          IF (IGARB.NE.0) THEN
            KTRIR=NLINK(NAM1,ISLOT)
          ENDIF
          IW(KTWRF+LMHCOL)=1
          IW(KTWRF+LMHROW)=0
        ENDIF
        IW(1)=1
        CALL AUBOS('TWRR',ISLOT,2*NWHIT+LMHLEN,KTWRR,IGARB)
        IF (IGARB.EQ.2) GO TO 998
        IF (IGARB.NE.0) THEN
          KTRIR=NLINK(NAM1,ISLOT)
          KTWRF=NLINK('TWRF',ISLOT)
        ENDIF
        IW(KTWRR+LMHCOL)=1
        IW(KTWRR+LMHROW)=0
C
C++     Link to the bank with pulse lengths (exists only starting 1991)
C
        LEN=(2*NWHIT-1)/4+LMHLEN+1
        CALL AUBOS('TWLE',ISLOT,LEN,KTWLE,IGARB)
        IF (IGARB.EQ.2) GO TO 998
        IF (IGARB.NE.0) THEN
          KTRIR=NLINK(NAM1,ISLOT)
          KTWRR=NLINK('TWRR',ISLOT)
          KTWRF=NLINK('TWRF',ISLOT)
        ENDIF
        IW(KTWLE+LMHCOL)=1
        KTRLE=NLINK(NAM3,ISLOT)
C
C++     Loop over wire hits.  NOTE to anyone modifying this
C++     routine:  please keep the TWRR, TWLE, and TWRF banks parallel.
C++     ALL hits in TSIR should appear in TWRR, perhaps with a
C++     flag in bit 13 labeling them as bad.   Those hits in
C++     in TSIR for which the digitizations were thrown away
C++     are flagged as bad and given zero charge in TWRR.
C
        KTRDI=NLINK(NAM2,ISLOT)
        IOFS=0
        DO 10 IH=1,NWHIT
          IWHTL=IW(KTRIR+IH)
C
C++       Check whether the pulse has been reduced already (as in TPP)
C
          IRFLG=IBITS(IWHTL,13,1)
          IF (IRFLG.EQ.1) THEN
            ITIM=IBITS(IWHTL,0,13)
C
C++         Correct the time for fact that the algorithm puts the
C++         time at the pulse center.  Therefore we need to shift by
C++         the shaping time of the amplifier.
C
            ZBUCK=FLOAT(ITIM)/TPACKW + TZOFFS(9)
            IWIR=IBITS(IWHTL,24,8)
            IF (FWIRPU) THEN
              IW(KTWRF+LMHROW)=LROWS(KTWRF)+1
              IWORD=KTWRF+LMHLEN+LROWS(KTWRF)
              CALL MVBITS(IH,0,16,IW(IWORD),0)
              CALL MVBITS(IOFS,0,16,IW(IWORD),16)
            ENDIF
            IW(KTWRR+LMHROW)=LROWS(KTWRR)+1
            IFLG(1)=1
            CALL MVBITS(IFLG(1),0,1,IWHTL,13)
            IBUCK= MIN(8191,INT(ZBUCK*TPACKW))
            CALL MVBITS(IBUCK,0,13,IWHTL,0)
            IW(KTWRR+LMHLEN+LROWS(KTWRR))=IWHTL
C
C++         Copy the pulse lengths into TWLE
C
            IWRD1=KTWLE+LMHLEN+1+(LROWS(KTWRR)-1)/4
            IBIT1=24-8*MOD(LROWS(KTWRR)-1,4)
            IF (KTRLE.NE.0) THEN
              IWORD=KTRLE+1+(IH-1)/8
              IBIT0=IBTOR(MOD(IH-1,8))
              CALL MVBITS(IW(IWORD),IBIT0,4,IW(IWRD1),IBIT1)
            ELSE
              CALL MVBITS(IAVGPL,0,8,IW(IWRD1),IBIT1)
            ENDIF
          ELSE
C
C++         Reduce the wire pulse.
C
            IF (KTRDI.EQ.0) THEN
              WRITE(TEXT,937) ISLOT,NAM2
  937         FORMAT('Sector ',I2,': non-reduced wire pulse found,',
     &               ' but no ',A4,' bank.')
              CALL RERROR('TWIRES',2,TEXT(1:60))
            ENDIF
C
            IT0=IBITS(IWHTL,0,9)
            IWIR=IBITS(IWHTL,24,8)
C
C++         Unreduced pulses with no entries in TSDI
C
            IF (KTRDI.EQ.0 .OR. (NAM1.EQ.'TSIR' .AND.
     &           ((IBITS(IWHTL,14,1).NE.0).OR.
     &              (IBITS(IWHTL,15,1).NE.0)))) THEN
              NWSMP=0
              NHITS=1
              LENGTH(NHITS)=IBITS(IWHTL,16,8)
              TIME(NHITS)=FLOAT(2*IT0+LENGTH(NHITS))/2.
              IQ(NHITS)=0
              IFLG(NHITS)=0
            ELSE
              NWSMP=IBITS(IWHTL,16,8)
C
C++           Loop over samples to collect the pulse heights
C
              NSAT=0
              LENP=0
              IWORD=KTRDI+1+IOFS/4
              IBIT0=24-8*MOD(IOFS,4)
              DO 5 IS=1,NWSMP
                IPH=IBITS(IW(IWORD),IBIT0,8)
                IF (IPH.GE.ITHRS) THEN
                  LENP=LENP+1
                  IF (IPH.EQ.255) NSAT=NSAT+1
                ENDIF
                ISPH(IS)=IPH-JPEDST(ISLOT,2)
                IBIT0=IBIT0-8
                IF (IBIT0.LT.0) THEN
                  IWORD=IWORD+1
                  IBIT0=24
                ENDIF
    5         CONTINUE
C
C++           Analyze the pulse.  Currently, the program can
C++           make one or two hits from each pulse.
C
              IF (LENP.GE.1) THEN
                IF (IALGTW.EQ.1) THEN
C
C++               This sum-of-highest-3 algorithm generally is not used
C
                  NHITS=1
                  CALL TRDWP1(IT0,NWSMP,ISPH,IQ,TIME,IQUAL)
                  LENGTH(1)=3
                ELSE
C
C++               Sum of all algorithm.  This one should be used.
C
                  IF (SPLITW) THEN
                    CALL TRDWP3(NSAT,IT0,NWSMP,ISPH,NHITS,
     &                                     IQ,TIME,LENGTH,IQUAL)
                  ELSE
                    NHITS=1
                    CALL TRDWP2(IT0,NWSMP,ISPH,IQ,TIME,LENGTH,IQUAL)
                  ENDIF
C
C++               The charge is divided by 2 to pack into 10 bits
C
                  DO 6721 K=1,NHITS
                    IQ(K)=IQ(K)/JCPAKW
                    IF (IQ(K).GT.1023) IQ(K)=1023
 6721             CONTINUE
                ENDIF
              ELSE
                NHITS=1
                IQ(NHITS)=0
                TIME(NHITS)=FLOAT(IT0)
                LENGTH(NHITS)=1
                IQUAL(NHITS)=1
              ENDIF
              DO 687 K=1,NHITS
                IF (IQUAL(K).NE.0) THEN
                  IFLG(K)=0
                ELSE
                  IFLG(K)=1
                ENDIF
C
C++             Serious saturation is flagged by setting charge to max.
C
                IF (NSAT.GT.MXSATW) THEN
                  IQ(K)=1023
                ENDIF
C
C++             Flag as bad those pulses which are too long
C
                IF (LENGTH(K).GT.AVPLTW) THEN
                  IFLG(K)=0
                ENDIF
  687         CONTINUE
            ENDIF
C
            DO 688 K=1,NHITS
C
C++           Correct z position for offset due to shaping time.  First,
C++           however, truncate in the same way as done in the TPP, so
C++           that times of pulses reduced by the TPP are the same as
C++           those reduced here.
C
              ZBUCK=FLOAT(MIN(8191,INT(TIME(K)*TPACKW)))/TPACKW
     &                                                   +TZOFFS(9)
C
C++           The z position is multiplied by 16 before packing
C
              IBUCK= MIN(8191,INT(ZBUCK*TPACKW))

              CALL MVBITS(IBUCK,0,13,IWHTL,0)
              CALL MVBITS(IFLG(K),0,1,IWHTL,13)
              CALL MVBITS(IQ(K),0,10,IWHTL,14)
              CALL MVBITS(IWIR,0,8,IWHTL,24)
              IW(KTWRR+LMHROW)=LROWS(KTWRR)+1
              IW(KTWRR+LMHLEN+LROWS(KTWRR))=IWHTL
              IWRD1=KTWLE+LMHLEN+1+(LROWS(KTWRR)-1)/4
              IBIT1=24-8*MOD(LROWS(KTWRR)-1,4)
              CALL MVBITS(LENGTH(K),0,8,IW(IWRD1),IBIT1)
              IF (FWIRPU) THEN
                IW(KTWRF+LMHROW)=LROWS(KTWRF)+1
                IWORD=KTWRF+LMHLEN+LROWS(KTWRF)
                CALL MVBITS(IH,0,16,IW(IWORD),0)
                CALL MVBITS(IOFS,0,16,IW(IWORD),16)
              ENDIF
  688       CONTINUE
            IOFS=IOFS+NWSMP
          ENDIF
   10   CONTINUE
C
C++     Set the lengths of the output banks
C
        IF (FWIRPU) THEN
          IF (LROWS(KTWRF).EQ.0) THEN
            KTWRF=NDROP('TWRF',ISLOT)
          ELSE
            LEN=LROWS(KTWRF)+LMHLEN
            IF (LEN.LT.IW(KTWRF)-4) THEN
              CALL AUBOS('TWRF',ISLOT,LEN,KTWRF,IGARB)
            ENDIF
          ENDIF
        ENDIF
        IF (LROWS(KTWRR).EQ.0) THEN
          KTWRR=NDROP('TWRR',ISLOT)
          KTWLE=NDROP('TWLE',ISLOT)
        ELSE
          LEN=LROWS(KTWRR)+LMHLEN
          IF (LEN.LT.IW(KTWRR)-4) THEN
            CALL AUBOS('TWRR',ISLOT,LEN,KTWRR,IGARB)
          ENDIF
          IW(KTWLE+LMHROW)=LROWS(KTWRR)
          LEN=(LROWS(KTWRR)-1)/4+LMHLEN+1
          IF (LEN.LT.IW(KTWLE)-4) THEN
            CALL AUBOS('TWLE',ISLOT,LEN,KTWLE,IGARB)
          ENDIF
        ENDIF
C
C++     Update the pointers for the next sector
C
  400   CONTINUE
        KTRIR=IW(KTRIR-1)
        GO TO 310
  410 CONTINUE
C
      IER=0
      GO TO 999
C
  998 CONTINUE
      CALL RERROR('TWIRES',1,'No space for named banks')
      IER=1
  999 CONTINUE
      RETURN
      END
#endif
