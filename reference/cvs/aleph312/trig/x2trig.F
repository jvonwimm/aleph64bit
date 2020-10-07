      SUBROUTINE X2TRIG (IRET)
C
C -----------------------------------------------------------
C! Main Level 2 trigger routine
C
C   Author : T.Medcalf   10/9/87
C   Modified T.M.        21/10/88
C   Modified T.M.        2/10/89
C                        New definition of output banks.
CKEY TRIGGER LEVEL2 / USER
C
C? Takes the TTHT bank as its input, and emulates the level2
C? track-finding electronics to produce a bos bank of hits
C? that passed through the electronics, a bank of tracks
C? found, and a bank containing a MSKLN word mask of the tracks
C? found in segments of phi and theta.
C
C - Output  : IRET / I      = return code
C                             0 means OK
C                             1 not enough space for BOS banks
C -----------------------------------------------------------
C   the BOS bank X2DF contains the hits and tracks found by LEVEL 2.
C    1 :  words / row  ( = 1 )
C    2 :  # of rows
C   For 24 sectors
C   word(1)   : processor number
C   word(2)   : number of time-bins with hits in them
C   word(3)   : number of tracks
C   word(i+3) : hit-pattern for i-th non-empty time-bin
C   word(word(2)+3)   : last hit-pattern
C   word(word(2)+i+3) : number of i-th non-empty time-bin
C   word(2*word(2)+3) : number of last non-empty time-bin
C   word(2*word(2)+i+3)       : theta-bin of i-th track
C   word(2*word(2)+word(3)+3) : theta-bin of last track
C   The next word is the next processor-number, and the above repeats.
C  -----------------------------------------------------------
C   The BOS bank X2MS contains the Trigger mask from level 2.
C   1   :   words / mask segment ( = 1 )
C   2   :   # of mask segments ( = 60 )
C   WORD(I)  : 1 if track found in Ith segment
C            : 0 if no track found in Ith segment.
C  -----------------------------------------------------------
C   The 24 work-banks ( 1 for each of 24 logical Phi-sectors ),
C  contain 3 words per hit , FORMAT INTEGER.
C   Each bank has input to it the hits from TTHT, which lie in its
C  sector.
C   So bank structure is
C   1   :   Words per hit
C   2   :   Number of hits in sector
C   Word(I)   = Row number of Ith hit
C   Word(I+1) = Pad number of Ith hit
C   Word(I+2) = Time-bin number of Ith hit
C  ---------------------------------------------------------------------
C   The work-banks for tracks in each zone, before and after sorting int
C  track order, for the counting process, contain 1 word / hit, which is
C  track-number from the TPC emulation.
C   1  :  Words per hit ( = 1 )
C   2  :  Number of hits
C   Word(I) = Track-number of Ith hit
C  ---------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "xtrcom.h"
#include "x2namc.h"
#include "x2cons.h"
#include "tthtjj.h"
#include "x2dfjj.h"
#include "x2msjj.h"
C
C    Using 2 words per row, but starting at zero, number and last are
      PARAMETER ( NWRTB = 2 * NPUSE, NWDLST = NWRTB - 1 )
C    And the first is
      PARAMETER ( NWDFST = 0 )
C    The theta-bins extend to
      PARAMETER ( NBMXP = NBMAX + 1 )
C    There are 4 pads in a phi-sector, and 24 phi-sectors -
C   12 on each end-cap and 6 each in the inner and outer zones.
      PARAMETER ( NPADS = 4, NPHSC = 24, NECSC = 12, NZSC = 6 )
C    The last real time-bin possible is
      PARAMETER ( NTMXM = NTMAX - 1 )
C
      INTEGER JTIME( 0:NTMAX, NPUSE, NPADS )
C 256th timebin is only there for zeroing dead hits algorithm.
C
      INTEGER JHTRW(NPUSE)
      INTEGER IQH(NWDFST:NWDLST),IQT(NWDFST:NWDLST),IQN(NWDFST:NWDLST)
      INTEGER ITHB(IOS),ITHS(IOS)
      INTEGER ITH(NWDFST:NBMXP,NPUSE),INTH(NWDFST:NBMXP)
      INTEGER IOTH(NWDFST:NBMXP,NPUSE)
      INTEGER IPHI(2*IOS),NMTF(IOS),ITMC(IOS)
      LOGICAL ADFLAG
      INTEGER IRET
C
      DATA NATTHT /0/
C Bmacro contains function statement defns
#include "bmacro.h"
C  After SORT of work-bank, position of track-word
      ISTRWD(IS,IC) =  JX2TRK(IS)+LMHLEN + IW( JX2SOR(IS)+LMHLEN+IC )
C  Number of bit to be set in hit-pattern
      NSETB(IR,IP) = 4*IR+IP-4
C  Phi-sector # deduced from hardware processor #
      IXSCP(IP) = NZSC+NECSC*(IP/NZSC)-IP
C   MOD(I,J) has range -J - (J-1), this changes it to 1 - J.
      MODPS(I,J) = 1 + MOD(I-1+J,J)
C -------------------------------------------------------------------
C
      IRET = 0
C
C Initialise Bos bank pointer, RETURN if TTHT is absent.
      IF (NATTHT.EQ.0) NATTHT = NAMIND('TTHT')
      ITTHT = IW(NATTHT)
      IF (ITTHT .EQ. 0) RETURN
      NHITS = LROWS (ITTHT)
      IF (NHITS .EQ. 0) RETURN
C
C Initialise work bank.
C
      DO 100 ISEC = 1, NPHSC
        CALL WBANK(IW,JWORKB(ISEC),LWBNK,*900)
        JSEC = JWORKB(ISEC)
        IW(JSEC-3) = INTCHA ('X2WK')
        IW(JSEC+1) = JWPHT
        IW(JSEC+2) = 0
  100 CONTINUE
C
C Initialise work bank for track counting.
C
      DO 101 ISEC = 1, IOS
        CALL WBANK(IW,JX2TRK(ISEC),NHITS+LMHLEN,*900)
        CALL WBANK(IW,JX2SOR(ISEC),NHITS+LMHLEN,*900)
        JTRK = JX2TRK(ISEC)
        JSOR = JX2SOR(ISEC)
        IW(JTRK-3) = INTCHA ('X2TK')
        IW(JSOR-3) = INTCHA ('X2SO')
        IW(JTRK+1) = 1
        IW(JSOR+1) = 1
        IW(JTRK+2) = 0
        IW(JSOR+2) = 0
  101 CONTINUE
C
      DO 1000 IHT = 1,NHITS
C Get track number
        ITRKN = ITABL(ITTHT,IHT,JTTHKT)
C
        ICNUM = ITABL(ITTHT,IHT,JTTHCI)
        PHI = RTABL(ITTHT,IHT,JTTHPH)
        Z = RTABL(ITTHT,IHT,JTTHZV)
C
C Extract pad,row,slot from ICNUM
C
        IROW = ICNUM/100000
        ISLOT = (ICNUM-(IROW*100000))/1000
        IPAD = (ICNUM-(IROW*100000)-(ISLOT*1000))
C Now add track number to workbank
        IOSEC = 1
        IF(IROW.GT.NPUSE) IOSEC = 2
        JTRK = JX2TRK(IOSEC)
        IW( KNEXT(JTRK)+1 ) = ITRKN
        IW(JTRK+LMHROW) = IW(JTRK+LMHROW) + 1
C
C
C Make pad numbers relative to x2 trigger sector edges.
C Also 'join' the split pads on the outer 5 rows.
C
C
        IF (IROW.LT.ISMX) THEN
C   Inner sector
          IXSEC = ISLOT
          IF (IXSEC.GE.19) IXSEC = IXSEC-6
C  Change the pad-numbering to be anti-clockwise
          IPAD = 5-IPAD
        ELSE
C   Outer sector
C   There are 2 Pads / slot and 2 slots / sector, in the outer zone
C  except where the Pads are split.
C   There is an offset between the start of a slot and the start of
C  a sector, equal to .5 slot = 1 Pad.
C
C    Renumber 2 as 1 again, and 3 and 4 both as 2 .
          IF (IROW.GE.15) IPAD = (IPAD+1)/2
          IFNEG = 0
C    There are 18 slots on each endcap = 6 inner + 12 outer.
          IF (ISLOT.GT.18) THEN
            ISLOT = ISLOT-18
            IFNEG = 1
          END IF
C    Slot-numbers convert to sector-numbers at half-rate, above 6.
          IF ( MOD( ISLOT, 2 ) .EQ. 1 ) THEN
            IXSEC = 7+(ISLOT-7)/2
C    The offset means 1 and 2 become 3 and 4, in the odd slots...
            IPAD = 4-IPAD
          ELSE
            IXSEC = 7+(ISLOT-8)/2
C   ...and 4 and 1, in the even slots.
            IF (IPAD.EQ.1) THEN
              IXSEC = IXSEC+1
              IF (IXSEC.EQ.13) IXSEC = 7
              IPAD = 1
            ELSE
              IPAD = 4
            END IF
          END IF
C   Add the number of sectors / endcap for other endcap.
          IF (IFNEG.EQ.1) IXSEC = IXSEC+NECSC
C   And renumber the outer-zone rows to start at 1
          IROW = IROW-NPUSE
          IROW = IPDCON(IROW)
        END IF
C
C Now we have:
C  IXSEC : L2trig sector number
C  IROW  : Padrow number out of existing padrows
C  IPAD  : Pad number (1-4)
C
C Now check that padrow is being used
C
        IF (IROW.EQ.-1) GOTO 1000
C
C Calculate timebin. Clock speed is 5.0 MHz (in X2CONS)
C
        ITMBN = INT((ZTPCMX-ABS(Z))*CLOCKR*10./TDVELO)
        IF ((ITMBN.LT.0).OR.(ITMBN.GE.NTMAX)) GOTO 1000
C
C Now add the hit to the appropriate sector work bank.
C Expand the bank if neccessary, and update the hit count.
C
        IWPTR = JWORKB(IXSEC)
        IF( LFRROW(IWPTR) .EQ. 0 ) THEN
          CALL WBANK(IW,JWORKB(IXSEC),IW(IWPTR)+LWBNK/2,*900)
        END IF
C Now add hit to workbank.
        IWPTR = JWORKB(IXSEC)
        IWPTRU = KNEXT(IWPTR)
        IW(IWPTRU+1) = IROW
        IW(IWPTRU+2) = IPAD
        IW(IWPTRU+3) = ITMBN
        IW(IWPTR+2) = IW(IWPTR+2)+1
C
 1000 CONTINUE
C  Calculate the number of tracks in the TPC
      DO 1511 ISEC = 1, IOS
        IHIO = IW(JX2TRK(ISEC)+2)
        IF(IHIO.EQ.0) THEN
          ITMC(ISEC) = 1
        ELSE
          CALL SORTZV(IW(JX2TRK(ISEC)+LMHLEN+1),
     +                IW(JX2SOR(ISEC)+LMHLEN+1),IHIO,-1,0,0,0,0)
C  Now the hits are ordered by track-number
          ITMC(ISEC) = 1
          ITLST = IW( ISTRWD(ISEC,1) )
          DO 1510 IHT = 2,IHIO
            ITCUR = IW( ISTRWD(ISEC,IHT) )
C  If the hit is on a different track, count 1 more track
            IF (ITCUR.NE.ITLST) THEN
              ITMC(ISEC) = ITMC(ISEC)+1
              ITLST = ITCUR
            END IF
 1510     CONTINUE
        END IF
 1511 CONTINUE
C
C Now add IDIGNZ noise hits to work banks
C
      DO 1500 INZ = 1,IDIGNZ
        IXSEC = INT(1.+RNDM(1)*NPHSC)
        IROW = INT(1.+RNDM(1)*NPUSE)
        IPAD = INT(1.+RNDM(1)*NPADS)
        Z = RNDM(1)*ZTPCMX
        ITMBN = INT((ZTPCMX-Z)*CLOCKR*10./TDVELO)
C
        IWPTR = JWORKB(IXSEC)
        IF( LFRROW(IWPTR) .LE. 0 ) THEN
          CALL WBANK(IW,JWORKB(IXSEC),IW(IWPTR)+LWBNK/2,*900)
        END IF
C Now add noise to workbank.
        IWPTR = JWORKB(IXSEC)
        IWPTRU = KNEXT(IWPTR)
        IW(IWPTRU+1) = IROW
        IW(IWPTRU+2) = IPAD
        IW(IWPTRU+3) = ITMBN
        IW(IWPTR+LMHROW) = IW(IWPTR+LMHROW)+1
 1500 CONTINUE
C
C Allocate Bos banks for X2 mask, X2 data (hits found, tracks found)
C
      IX2MS = IW(NAX2MS)
      IF (IX2MS.NE.0) CALL BDROP(IW,'X2MSX2DF')
      CALL ALBOS('X2MS',0,MSKLN+LMHLEN,IX2MS,IGARB)
      CALL BLIST(IW,'E+','X2MS')
      IW(IX2MS+LMHCOL) = LX2MSA
      IW(IX2MS+LMHROW) = MSKLN
C (Mask bank is 60 words fixed length)
C
      CALL ALBOS('X2DF',0,IDFLEN,IX2DF,IGARB)
      CALL BLIST(IW,'E+','X2DF')
      IW(IX2DF+LMHCOL) = LX2DFA
      IW(IX2DF+LMHROW) = 0
C Set up to count lost hits and total number of fifo hits
      IDEAD = 0
      ITNFH = 0
      IFLST = 0
C Set up to count distinct tracks found
      LSBF = 0
      LSSF = 0
      NMTF(1) = 0
      NMTF(2) = 0
C
C Zero level 2 mask (60 bits)
C
      CALL VZERO(IX2MSK,MSKLN)
C
C
C +*+*+   N o w    l o o p    o v e r    p r o c e s s o r s   +*+*+
C
C  NPHSC (= 24) processors are numbered 0-23...
C
      DO 2000 IPROC = 0, NPHSC-1
C
C  but not in the same order...
        IXSEC = IXSCP(IPROC)
C
C  Begin X2DF subsection with processor # and 2 counters
        IF(LFRWRD(IX2DF).LT.3) THEN
           NDATA = IW(IX2DF)+ IDFLEN/2
           CALL AUBOS('X2DF',0,NDATA,IX2DF,IGARB)
           IF (IX2DF.EQ.0) GOTO 900
        END IF
        IW(KNEXT(IX2DF)+1) = IPROC
        IW(IX2DF+LMHROW) =  IW(IX2DF+LMHROW)+1
C  Hits counter
        IADHT = KNEXT(IX2DF)+1
        IW(IADHT) = 0
        IW(IX2DF+LMHROW) =  IW(IX2DF+LMHROW)+1
C  Tracks counter
        IADTR = KNEXT(IX2DF)+1
        IW(IADTR) = 0
        IW(IX2DF+LMHROW) =  IW(IX2DF+LMHROW)+1
C
        IOSEC = 1+((IXSEC-1)/NZSC)-((IXSEC-1)/NECSC)*2
C   IOSEC is 1 for inner sectors (1-6,13-18), 2 for outers.
        ICSEC = MOD(IXSEC-1,NZSC)+1
C   ICSEC is sector number regardless of which endcap, or inner/outer
        CALL VZERO(JTIME(0,1,1),(NTMAX+1)*NPUSE*NPADS)
C Fill JTIME array with hit pattern for sector IXSEC
        JWPTR = JWORKB(IXSEC)
        DO 2010 IHN = 1,LROWS(JWPTR)
          IROW = ITABL(JWPTR,IHN,1)
          IPAD = ITABL(JWPTR,IHN,2)
          ITMBN = ITABL(JWPTR,IHN,3)
          JTIME(ITMBN,IROW,IPAD) = 1
 2010   CONTINUE
C
C Now we are finished with the hits in the workbank.
C Use JTIME array to generate hit-patterns and drift-times for X2DF.
C
        DO 2014 ITMBN = 0, NTMXM
          ITBHP = 0
          DO 2016 IROW = 1, NPUSE
             DO 2018 IPAD = 1, NPADS
                IF (JTIME(ITMBN,IROW,IPAD).EQ.1) THEN
                   NTSET = NSETB(IROW,IPAD)
C  Build the hit into the pattern
                   CALL SBIT1(ITBHP,NTSET)
                ENDIF
 2018        CONTINUE
 2016     CONTINUE
C
C    Add non-empty hit-pattern to X2DF.
C
          IF (ITBHP.NE.0) THEN
             IF(LFRWRD(IX2DF).LT.1) THEN
                NDATA = IW(IX2DF)+ IDFLEN/2
                CALL AUBOS('X2DF',0,NDATA,IX2DF,IGARB)
                IF (IX2DF.EQ.0) GOTO 900
             END IF
             IW(KNEXT(IX2DF)+1) = ITBHP
             IW(IX2DF+LMHROW) = IW(IX2DF+LMHROW)+1
             IW(IADHT) = IW(IADHT)+1
          ENDIF
 2014   CONTINUE
C
C      Add the drift-times to X2DF
C
       DO 2114 ITMBN = 0, NTMXM
          ADFLAG = .FALSE.
          DO 2116 IROW = 1, NPUSE
             DO 2118 IPAD = 1, NPADS
                IF (JTIME(ITMBN,IROW,IPAD).EQ.1) THEN
                   ADFLAG = .TRUE.
                   GOTO 2116
                ENDIF
 2118        CONTINUE
 2116     CONTINUE
          IF (ADFLAG) THEN
C  Add drift-time to X2DF
             IF(LFRWRD(IX2DF).LT.1) THEN
                NDATA = IW(IX2DF)+ IDFLEN/2
                CALL AUBOS('X2DF',0,NDATA,IX2DF,IGARB)
                IF (IX2DF.EQ.0) GOTO 900
             END IF
             IW(KNEXT(IX2DF)+1) = ITMBN
             IW(IX2DF+LMHROW) = IW(IX2DF+LMHROW)+1
          ENDIF
 2114  CONTINUE
C
        INFH = 0
        IDDSC = 0
        DO 2020 ITM = 0, NTMXM
          CALL VZERO(JHTRW,NPUSE)
          IHFL = 0
          DO 2030 IROW = 1,NPUSE
            DO 2040 IPAD = 1,NPADS
              IF (JTIME(ITM,IROW,IPAD).EQ.1) THEN
C Set 'bit' in row store, and flag that this timebin
C    has been hit.
                JHTRW(IROW) = 1
                IHFL = 1
              END IF
 2040       CONTINUE
 2030     CONTINUE
          IF (IHFL.EQ.1) THEN
            INFH = INFH+1
            IRTOT = 0
            IFCLR = 0
            DO 2300 IROW = 1,NPUSE
              IRTOT = IRTOT+JHTRW(IROW)*2**(IROW-1)
              JTIME(ITM,IROW,1) = JHTRW(IROW)
              IF (JHTRW(IROW).NE.0) THEN
                IFCLR = IFCLR+1
                IF (ITHOVR(IOSEC,ITM,IROW).EQ.1) IFCLR = IFCLR+1
              END IF
 2300       CONTINUE
            JTIME(ITM,1,2) = IRTOT
            JTIME(ITM,1,3) = IFCLR
          END IF
 2020   CONTINUE
        IDEAD = IDEAD+IDDSC
C
C   Now the level 3 bank hits and drift-times have been written
C                                                for this sector,
C    And the fifo hits have been stored on the work bank
C
      ITNFH = ITNFH+INFH
C
C Now the hit rows are in the JTIME array ready for fifo processing.
C
C Initialise fifo head ptr, tailptr, full flag, rows lost in sector.
C , also hits  (overlapped and not) lost in fifo.
      IHEAD = 0
      ITAIL = 0
      IFULL = 0
      IFOVL = 0
      IFNVL = 0
      IFNVH = 0
      IFOVH = 0
      IFLSC = 0
      IFPSC = 0
      ILSEN = 0
      ITMAX = INT((ZTPCMX*CLOCKR*10.)/ADVELO)
      CALL VZERO(ITH(0,1),2*NTMAX)
      CALL VZERO(INTH(0),NBMXP+1)
      CALL VZERO(IOTH(0,1),2*NTMAX)
C
C Now loop over each time slot
C
      DO 8000 ITBN = 0, NTMXM
        IF((ITBN.LT.ITMAX).AND.(JTIME(ITBN,1,2).NE.0)) THEN
          IF (IFULL.EQ.1) THEN
            IFLSC = IFLSC+1
            IFLST = IFLST+1
            DO 8511 IROW = 1, NPUSE
              IFNVL = IFNVL+(JTIME(ITBN,IROW,1)*(1-
     +          ITHOVR(IOSEC,ITBN,IROW)))
              IFOVL = IFOVL+(JTIME(ITBN,IROW,1)*
     +          ITHOVR(IOSEC,ITBN,IROW))
 8511       CONTINUE
          ELSE
            IT16 = MOD(ITAIL,NWRTB)
            IQH(IT16) = JTIME(ITBN,1,3)
            IQT(IT16) = 0
            IQN(IT16) = ITBN
            ITAIL = ITAIL+1
            IF (ITAIL.EQ.IHEAD+NWRTB) IFULL = 1
          END IF
        END IF
        IF (ITAIL.NE.IHEAD) THEN
          IH16 = MOD(IHEAD,NWRTB)
          IQT(IH16) = IQT(IH16)+1
          IF (IQT(IH16).EQ.IQH(IH16)) THEN
            IFPSC = IFPSC+1
            IHEAD = IHEAD+1
            IFULL = 0
            ITM = IQN(IH16)
            DO 8010 IROW = 1, NPUSE
              IF (JTIME(ITM,IROW,1).NE.0) THEN
                IFNVH = IFNVH+1-ITHOVR(IOSEC,ITM,IROW)
                IFOVH = IFOVH+ITHOVR(IOSEC,ITM,IROW)
                ITHB(1) = ITHETA(IOSEC,ITM,IROW)
                ITHS(1) = ITHSUB(IOSEC,ITM,IROW)
                ITHB(2) = ITHOVR(IOSEC,ITM,IROW)+ITHB(1)
                ITHS(2) = ITHOSB(IOSEC,ITM,IROW)
C
                IF (ITH(ITHB(1),IROW).EQ.0) THEN
                  ITH(ITHB(1),IROW) = ITHS(1)
                  INTH(ITHB(1)) = INTH(ITHB(1))+1
                  IOTH(ITHB(1),IROW) = ITHOVR(IOSEC,ITM,IROW)
                END IF
                IF(ITHB(2).NE.ITHB(1)) THEN
                  IF (ITH(ITHB(2),IROW).EQ.0) THEN
                    ITH(ITHB(2),IROW) = ITHS(2)
                    INTH(ITHB(2)) = INTH(ITHB(2))+1
                    IOTH(ITHB(2),IROW) = 0
                  END IF
                END IF
C
C
              END IF
 8010       CONTINUE
          END IF
        END IF
 8000 CONTINUE
C
C Now see if any rows/hits are lost at the end of fifo
C processing.
C
      IF (ITAIL.NE.IHEAD) THEN
        IH16 = MOD(IHEAD,NWRTB)
        IT16 = MOD(ITAIL,NWRTB)
C First find losses in current fifo slot, which may not have
C been all processed. Row NPUSE is procd first, then NPUSE-1 etc down
C 1. Within a row, overlap hits are processed before
C non-ovelapped.
        NPROC = IQT(IH16)
        NSF = 0
        ITM = IQN(IH16)
        IROW = NPUSE
 7100   CONTINUE
        IF ((JTIME(ITM,IROW,1).EQ.1).AND.
     +    (ITHOVR(IOSEC,ITM,IROW).EQ.1)) THEN
          IBIN = ITHETA(IOSEC,ITM,IROW)+1
          ISUB = ITHOSB(IOSEC,ITM,IROW)
          IF (ITH(IBIN,IROW).EQ.0) THEN
            ITH(IBIN,IROW) = ISUB
            INTH(IBIN) = INTH(IBIN)+1
            IOTH(IBIN,IROW) = 0
          END IF
          NSF = NSF+1
          IF (NSF.EQ.NPROC) GOTO 7777
        END IF
        IF (JTIME(ITM,IROW,1).EQ.1) THEN
          IBIN = ITHETA(IOSEC,ITM,IROW)
          ISUB = ITHSUB(IOSEC,ITM,IROW)
          IF (ITH(IBIN,IROW).EQ.0) THEN
            ITH(IBIN,IROW) = ISUB
            INTH(IBIN) = INTH(IBIN)+1
            IOTH(IBIN,IROW) = ITHOVR(IOSEC,ITM,IROW)
          END IF
          NSF = NSF+1
          IF(NSF.EQ.NPROC) GOTO 7777
        END IF
        IROW = IROW-1
        IF(IROW.EQ.0) GOTO 7777
        GOTO 7100
 7777   CONTINUE
C Now the current fifo word has been processed. Now we may lose
C complete rows after the current one.
        ILSEN = ITAIL-IHEAD-1
        IF(ILSEN.LT.0) ILSEN = 0
      END IF
C
C Now use ITH,ITHN and IOTH arrays together with look up tables
C to do theta bin and z-road analysis.
C
      DO 4000 IBIN = 1,NTBINS(IOSEC)
        IMAXH = IHTMAX(IOSEC,IBIN)
        ITHRS = ITHRSH(IMAXH)
        IF (INTH(IBIN).GE.ITHRS) THEN
          DO 4010 IPCHX = 1, ICHX
            IL = IPADPR(1,IPCHX)
            IU = IPADPR(2,IPCHX)
            IF (ITH(IBIN,IL)*ITH(IBIN,IU).NE.0) GOTO 4020
 4010     CONTINUE
C No valid pad pairs found - do next theta bin
          GOTO 4000
C IPCHX is the chosen pad pair
 4020     IVOTE = 0
          IOVTE = 0
          ISU = ITH(IBIN,IU)
          ISL = ITH(IBIN,IL)
          DO 4030 IROW = 1, NPUSE
            ISN = IZRDLK(ISL,ISU,IROW,IPCHX,IOSEC)
            IVOTE = IVOTE+ITVOTE(ITH(IBIN,IROW),ISN)
            IOVTE = IOVTE+ITVOTE(ITH(IBIN,IROW),ISN)*IOTH(IBIN,IROW)
 4030     CONTINUE
          IZPRD = IZRDLK(ISL,ISU,ISMX,IPCHX,IOSEC)
          IF (IVOTE.GE.ITHRS) THEN
C Track found!!
            IBINF = IBIN
C Increment tracks found if this track is not the same as
C   the last one.
            IF((IBINF.NE.LSBF).OR.(IXSEC.NE.LSSF)) THEN
              NMTF(IOSEC) = NMTF(IOSEC)+1
              LSBF = IBINF
              LSSF = IXSEC
C And put it in X2DF
              IF(LFRWRD(IX2DF).LT.1) THEN
                NDATA = IW(IX2DF)+ IDFLEN/2
                CALL AUBOS('X2DF',0,NDATA,IX2DF,IGARB)
                IF (IX2DF.EQ.0) GOTO 900
              END IF
              LCTRK = KNEXT(IX2DF)+1
              IW(LCTRK) = IBINF
              IW(IX2DF+LMHROW) =  IW(IX2DF+LMHROW)+1
              IW(IADTR) = IW(IADTR)+1
            END IF
C
            IF(IOVTE.GE.ITHRS) THEN
C Track found in overlap region!
              IBINF = IBINF+1
C Change last entry
              IW(LCTRK) = IBINF
              LSBF = IBINF
C Add to X2DF
              IF(LFRWRD(IX2DF).LT.1) THEN
                 NDATA = IW(IX2DF)+ IDFLEN/2
                 CALL AUBOS('X2DF',0,NDATA,IX2DF,IGARB)
                 IF (IX2DF.EQ.0) GOTO 900
              END IF
              IW(KNEXT(IX2DF)+1) = IBINF
              IW(IX2DF+LMHROW) =  IW(IX2DF+LMHROW)+1
              IW(IADTR) = IW(IADTR)+1
            ENDIF
C
            IF( IX2PRL .NE. 0 ) WRITE(IW(6),4040) IXSEC, IBINF, IZPRD
C
C Add track to approp 'bits' in IX2MSK
C
            DO 4045 ITHSN = 1, IOS
              ITHSG = MASCON(IBINF,ITHSN,IOSEC)
              IF(IOSEC.EQ.1) THEN
                IF (ITHSG.GT.2) THEN
                  NPHSG   = 3
                  IPHI(1) = MODPS(ICSEC+1,NZSC)
                  IPHI(2) = ICSEC
                  IPHI(3) = MODPS(ICSEC-1,NZSC)
                ELSE
                  NPHSG   = 4
                  IPHI(1) = MODPS(ICSEC*2+1,NECSC)
                  IPHI(2) = ICSEC*2
                  IPHI(3) = MODPS(ICSEC*2-1,NECSC)
                  IPHI(4) = MODPS(ICSEC*2-2,NECSC)
                END IF
              ELSE
                IF(ITHSG.GT.2) THEN
                  NPHSG   = 2
                  IPHI(1) = ICSEC
                  IPHI(2) = MODPS(ICSEC-1,NZSC)
                ELSE
                  NPHSG   = 4
                  IPHI(1) = ICSEC*2
                  IPHI(2) = ICSEC*2-1
                  IPHI(3) = MODPS(ICSEC*2-2,NECSC)
                  IPHI(4) = MODPS(ICSEC*2-3,NECSC)
                END IF
              END IF
              IF(ITHSG.EQ.1) THEN
                IMBAS = 25
              ELSE IF (ITHSG.EQ.2) THEN
                IMBAS = 37
                IF(IXSEC.GT.12) IMBAS = 13
              ELSE IF (ITHSG.EQ.3) THEN
                IMBAS = 49
                IF(IXSEC.GT.12) IMBAS = 7
              ELSE
                IMBAS = 55
                IF(IXSEC.GT.12) IMBAS = 1
              END IF
C
C Now IPHI(1:nthsg) hold phi bins, IMBAS holds start of mask
C    sections so set bits in mask.
C
              DO 4047 IPH = 1,NPHSG
                 IX2MSK(IMBAS+IPHI(IPH)-1) = 1
 4047         CONTINUE
C
C loop for next theta segment.
 4045       CONTINUE
C
          END IF
        END IF
 4000 CONTINUE
C
C print sector stats if requested
      IF(IX2PRL.NE.0) THEN
        IF(IPROC.EQ.0) WRITE(IW(6),8716)
 8716 FORMAT(/' +++X2TRIG+++ Sector hit data summary',//,
     +       ' Sector  dead',2('  rows  hits  hits'),'  end',/,
     +       '       losses thru  singl overl',
     +       ' lost  singl overl losses')
        WRITE(IW(6),8707)IXSEC,IDDSC,IFPSC,IFNVH,IFOVH,
     +    IFLSC,IFNVL,IFOVL,ILSEN
 8707 FORMAT(1X,I3,5X,I3,3X,2(I3,3X,I3,3X,I3,3X,I3,2X),I3)
      END IF
C
 2000 CONTINUE
C (loop for next processor/sector)
C
C
C     * *       *      * * * *
C    E N D     O F    E V E N T
C     * *       *      * * * *
C
C Fill histograms
      IF (IX2HIS.EQ.1) THEN
C Total tracks found
        CALL HF1(1140,REAL(NMTF(1)+NMTF(2)),1.)
C percent tracks found in inner/outer
        EFFIN = MIN(100.,100.0*REAL(NMTF(1))/REAL(ITMC(1)))
        EFFOU = MIN(100.,100.0*REAL(NMTF(2))/REAL(ITMC(2)))
        CALL HF1(1141,EFFIN,1.)
        CALL HF1(1142,EFFOU,1.)
      END IF
C
C Print statistics for event.
      IF (IX2PRL.NE.0) THEN
        WRITE(IW(6),9410) NHITS, ITNFH, IDEAD, IFLST, NMTF(1), NMTF(2)
     +                    ,ITMC(1), ITMC(2)
C
      END IF
C
C Copy IX2MSK array into bank X2MS
C Extract yes/no decision
C
      NYES = 0
      IX2MS = IW(NAX2MS)
      DO 9200 IMN = 1, MSKLN
        IF (IX2MSK(IMN).EQ.1) NYES = 1
        IW(IX2MS+LMHLEN+IMN) = IX2MSK(IMN)
 9200 CONTINUE
      IF (NYES.EQ.1) NACCL2 = NACCL2+1
      IW(IX2MS+LMHROW) = MSKLN
C
C Now compress  Bos bank.
      CALL AUBPRS('X2DF')
C
      IRET = 0
      GOTO 999
C
C Error return to label 900 - unable to allocate workbank
C
 900  CONTINUE
      IRET = 1
      IW(1) = NPHSC
      CALL WDROP (IW,JWORKB)
      IW(1) = IOS
      CALL WDROP (IW,JX2TRK)
      IW(1) = IOS
      CALL WDROP (IW,JX2SOR)
      CALL ALTELL(' X2TRIG: not enough space for work bank',1,'NEXT')
C
 999  CONTINUE
      IW(1) = NPHSC
      CALL WDROP (IW,JWORKB)
      IW(1) = IOS
      CALL WDROP (IW,JX2TRK)
      IW(1) = IOS
      CALL WDROP (IW,JX2SOR)
C
      RETURN
C
 4040 FORMAT(1X,'+++X2TRIG+++  Track found:',
     +        ' Sector = ', I3,5X,
     +        ' Th - bin = ', I3,5X,
     +        ' Pred vertex = ', I3 )
C
 9410 FORMAT( ' +++X2TRIG+++ Event summary data', /,
     +  ' Hits read off TTHT bank        = ', I5, /,
     +  ' Rows written to pre-fifo array = ', I5, /,
     +  ' Hits lost in dead times        = ', I5, /,
     +  ' Rows lost in fifo congestion   = ', I5, /,
     +  ' Tracks found in inner sector   = ', I5, /,
     +  ' Tracks found in outer sector   = ', I5, /,
     +  ' Tracks generated in inner      = ', I5, /,
     +  ' Tracks generated in outer      = ', I5 )
C
      END
#endif
