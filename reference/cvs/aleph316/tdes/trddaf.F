      SUBROUTINE TRDDAF (LBASE,IRUN,IRET)
C ----------------------------------------------------------------------
C. - M.Mermikides - 860921             modified by F.Ranjard - 880420
C!  Read DAF and Fill commom blocks TPGEOM,TPGEOP,TPGEOW,TPGEOT
C.   with useful TPC geometric quantities derived from data base
C
C  - D.Casper - 970305  modify to read new alignment (TNLO, etc)
C                       if available.  If new-style alignment does not
C                       exist, old alignment is used.  TOAL card forces
C                       old alignment even if new banks exist
C               970416  check validity range of TNLO to avoid using
C                       invalid calibrations for a previous year if none
C                       exist for the current year.
C
C Modified: M.Cattaneo 980605 Print calibration bank style message once only
C
C - Input arguments:  LBASE   = DAF file logical unit #
C                     IRUN    = current run #
C
C - Output argument:  IRET    = return code ( = ALGTDB return value)
C                               = 0 means at least one bank is missing
C                               < 0 means at least one bank is new
C                               > 0 means no bank is new
#ifndef DOC
      SAVE
#include "alcons.h"
#include "bcs.h"
#include "tjjpar.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpgeow.h"
#include "tpgeot.h"
C
      LOGICAL FIRST
      EXTERNAL NAMIND, ALGTDB
      INTEGER ALGTDB
      DIMENSION ANFIR(LTSTYP),ANTPA(LTSTYP)
      DIMENSION ITPD1 (LTTPAD,LTSTYP)
      CHARACTER*12 COMBNK
      CHARACTER*12 OLDBNK
      CHARACTER*16 NEWBNK
C These banks are common to old and new alignment procedures
      DATA COMBNK /'TCGDTMTYTSGM'/
C These banks are specific to one procedure or the other, and
C only members of one set should be loaded at a given time
      DATA OLDBNK /'TPOSTSLOTSCO'/
      DATA NEWBNK /'TNOSTNLOTNCOTBDR'/
      DATA FIRST /.TRUE./
C
#include "bmacro.h"
C
C ---------------------------------------------------------------------
      IF (FIRST) THEN
        NTOAL = NAMIND('TOAL')
        NTDFV = NAMIND('TDFV')
        NTNFV = NAMIND('TNFV')
      ENDIF
      NTPROW = LTPDRO
      NTSECT = LTSECT
C
C - Determine which alignment to look for first.  TOAL forces old alignment
C - to be used (error if none exists).
C
      IF (IW(NTOAL).NE.0) THEN
        IRET = ALGTDB(LBASE,COMBNK//OLDBNK,IRUN)
C
C - If banks are missing (IRET=0) OR banks are still valid
C      and it is not the 1st entry (IRET>0 and .NOT.FIRST) RETURN
C
        IF ((IRET.EQ.0) .OR. (IRET.GT.0 .AND. .NOT.FIRST)) RETURN
C
C - We have read valid calibration banks for old alignment; drop any
C   new alignment banks to avoid confusion.
C
        CALL BDROP(IW,NEWBNK)
      ELSE
C
C - If no TOAL card, always try to load new alignment banks first
C
        IRET = ALGTDB (LBASE,COMBNK//NEWBNK,IRUN)
C
C - If banks are still valid and it is not the first entry, return
C
        IF (IRET.NE.0) THEN
            KTNLO = IW(NAMIND('TNLO'))
            IF (IRUN.GE.IW(KTNLO+LMHLEN+JTNLVR).AND.
     &          IRUN.LE.IW(KTNLO+LMHLEN+JTNLVR+1)) THEN
                IF (IRET.GT.0 .AND. .NOT.FIRST) RETURN
            ELSE
                IRET = 0
            ENDIF
        ENDIF
        IF (IRET.NE.0) THEN
C
C - We have read valid calibration banks for new alignment; drop any
C   old alignment banks to avoid confusion
C
            CALL BDROP(IW,OLDBNK)
        ELSE
C
C - If new alignment banks are not found, try the old set
C
            IRET = ALGTDB (LBASE,COMBNK//OLDBNK,IRUN)
C
C - If *these* banks are missing (IRET=0) OR banks are still valid
C      and it is not the 1st entry (IRET>0 and .NOT.FIRST) RETURN
C
            IF ((IRET.EQ.0)
     &          .OR. (IRET.GT.0 .AND. .NOT.FIRST)) RETURN
C
C - We have read valid calibration banks for old alignment; drop any
C   new alignment banks to avoid confusion
C
            CALL BDROP(IW,NEWBNK)
        ENDIF
      ENDIF
C
C - Tell user what kind of alignment we are using and double check for
C   clashes
C
      KTNLO = IW(NAMIND('TNLO'))
      KTSLO = IW(NAMIND('TSLO'))
      IF (KTNLO.GT.0 .AND. KTSLO.EQ.0) THEN
        IF (IW(6).GT.0 .AND. FIRST) WRITE(IW(6),1)
  1     FORMAT(/,' TRDDAF - 1997-style calibration banks read')
      ELSE IF (KTSLO.GT.0 .AND. KTNLO.EQ.0) THEN
        IF (IW(6).GT.0 .AND. FIRST) WRITE(IW(6),2)
  2     FORMAT(/,' TRDDAF - pre-1997-style calibration banks read')
      ELSE IF(KTSLO.GT.0 .AND. KTNLO.GT.0) THEN
        CALL ALTELL('TRDDAF - inconsistent banks - contact expert',
     &      -1,'END')
      ELSE
        CALL ALTELL('TRDDAF - missing banks - contact expert',
     &      -1,'END')
      ENDIF
      FIRST = .FALSE.
      IF (KTNLO.EQ.0 .AND. IW(NTDFV).GT.0 .AND. IW(NTNFV).GT.0)
     &  CALL ALTELL('TRDDAF: WARNING - TNFV present without TNLO; '//
     &  'v_d may be inconsistent!',0,'RETURN')
      IF (KTNLO.GT.0 .AND. IW(NTDFV).GT.0 .AND. IW(NTNFV).EQ.0)
     &  CALL ALTELL('TRDDAF: WARNING - TNLO present without TNFV; '//
     &  'v_d may be inconsistent!',0,'RETURN')
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I           'TSGM':             <<<  Sector geometry >>>           I
C  I           -------             ------------------------           I
C  I                                                                  I
C  I  Generate the constants describing                               I
C  I               the corners of the 3 sector - types                I
C  +------------------------------------------------------------------+
C
      KTSGM = IW(NAMIND('TSGM'))
C
      IF (KTSGM .GT. 0) THEN
C
         DO 40 IS = 1, LTSTYP
            NTPCRN (IS) = ITABL (KTSGM,IS,JTSGNC)
            DO 30 ICRN = 1,5
               TPCORN (1,ICRN,IS) = RTABL (KTSGM,IS,JTSGYC-1+ICRN)
               TPCORN (2,ICRN,IS) = RTABL (KTSGM,IS,JTSGXC-1+ICRN)
   30       CONTINUE
   40    CONTINUE
C
      ENDIF
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I           'TCGD':             <<<  General sector constants >>>  I
C  I           -------             ---------------------------------  I
C  I                                                                  I
C  I  Generate the constants describing                               I
C  I           the geometrical constants valid for all sector types   I
C  I                                                                  I
C  +------------------------------------------------------------------+
C
      KTCGD = IW(NAMIND('TCGD'))
C
      IF (KTCGD .GT. 0) THEN
C
C Angular spacing between sectors of the same type (nominally = pi/3)
         ANGSP = RTABL (KTCGD,1,JTCGAS)
C  Distance of edgde of first pad from sector border
         BORDS = RTABL (KTCGD,1,JTCGBD)
C
C++     Fill general constants for pads into   /TPGEOP/
C
         DO 220 IS = 1,LTSTYP
            TPDRST (IS) = RTABL (KTCGD,1,JTCGRS)
            TPDHGT (IS) = RTABL (KTCGD,1,JTCGPH)
            TPDSEP (IS) = RTABL (KTCGD,1,JTCGPS)
            TPDWID (IS) = RTABL (KTCGD,1,JTCGPW)
            TPDHWD (IS) = RTABL (KTCGD,1,JTCGEW)
C
C         General constants for trigger - pads into   /TPGEOT/
C
            TPTRST (IS) = RTABL (KTCGD,1,JTCGTS)
            TPTRHG (IS) = RTABL (KTCGD,1,JTCGTH)
C
C         General constants for wires into   /TPGEOW/
C
            TWSTEP (IS) = RTABL (KTCGD,1,JTCGWP)
C
  220    CONTINUE
C
C     Wall thickness of TPC
C
          DRTPMN      = RTABL (KTCGD,1,JTCGTT)
          DRTPMX      = RTABL (KTCGD,1,JTCGTT+1)
          DZTPMX      = RTABL (KTCGD,1,JTCGTT+2)
C
C     Compute inner dimensions (sensitive volume)
C
          RTPCMN = RTABL (KTCGD,1,JTCGTO) + DRTPMN
          RTPCMX = RTABL (KTCGD,1,JTCGTO+1) - DRTPMX
          ZTPCMX = RTABL (KTCGD,1,JTCGTO+2) - DZTPMX
C
C    Wheel rib - thickness, rib - width and equivalent thickness
C
          TPFRDZ      = RTABL (KTCGD,1,JTCGWT)
          TPFRDW      = RTABL (KTCGD,1,JTCGWW)
          TPAVDZ      = RTABL (KTCGD,1,JTCGWE)
C
C    Wheel rib offsets (describing the kinks in the ribs)
C
          TPFOF1      = RTABL (KTCGD,1,JTCGWK)
          TPFOF2      = RTABL (KTCGD,1,JTCGWK+1)
          TPFOF3      = RTABL (KTCGD,1,JTCGWK+2)
C
C    Thickness of frame on which wires are mounted
C
          TFRATH      = RTABL (KTCGD,1,JTCGFT)
       ENDIF
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I           'TMTY':             <<<  Sector - types >>>            I
C  I           -------             -----------------------            I
C  I                                                                  I
C  I  Generate the constants describing                               I
C  I       the geometry of the sensitive elements (pads, wires, tpads)I
C  I                                                                  I
C  +------------------------------------------------------------------+
C
      KTMTY = IW(NAMIND('TMTY'))
C
      IF (KTMTY .GT. 0) THEN
         DO 80 IS = 1, LTSTYP
            ANFIR (IS)      = RTABL (KTMTY,IS,JTMTPP)
C      radius of 1. pad row:
            TPDRBG (IS)     = RTABL (KTMTY,IS,JTMTRF)
C      number of padrows
            NTPDRW (IS)     = ITABL (KTMTY,IS,JTMTNP)
C      number of pads in each padrow
            DO  50 IROW = 1,12
               NTPDPR (IROW,IS) = ITABL (KTMTY,IS,JTMTPR-1+IROW)
   50       CONTINUE
C
C      radius of 1. tpad row:
            TPTRBG (IS)      = RTABL (KTMTY,IS,JTMTRT)
C      number of tpadrows
            NTPTRW (IS)      = ITABL (KTMTY,IS,JTMTNT)
C
C      number of tpads in each tpadrow
            DO 150 IROW = 1,11
               NTPTPR (IROW,IS) = ITABL (KTMTY,IS,JTMTTR-1+IROW)
  150       CONTINUE
C
            ANTPA(IS) = RTABL (KTMTY,IS,JTMTAT)
C  Trigger pad numbering
            ITPD1(1,IS) = ITABL (KTMTY,IS,JTMTTC)
            ITPD1(2,IS) = ITABL (KTMTY,IS,JTMTTC+1)
            ITPD1(3,IS) = ITABL (KTMTY,IS,JTMTTC+2)
            ITPD1(4,IS) = ITABL (KTMTY,IS,JTMTTC+3)
C      radius of center of 1. wire, # of wires:
            TWIRE1 (IS) = RTABL (KTMTY,IS,JTMTPW)
            NTWIRE (IS) = ITABL (KTMTY,IS,JTMTNW)
C      wire pointers to first and last el. channel
            ITLWIF (IS) = ITABL (KTMTY,IS,JTMTEF)
            ITLWIL (IS) = ITABL (KTMTY,IS,JTMTEL)
            DO 78 IREG = 1,4
   78       NTREG1 (IREG,IS) = ITABL (KTMTY,IS,JTMTWF+IREG-1)
C
   80    CONTINUE
C
C++     compute radii of padrows
C
         DO  70 IS = 1,9
            TPPROW (IS) = TPDRBG (1) + (IS- 1) * TPDRST (1)
   70    CONTINUE
         DO  75 IS = 10,21
            TPPROW (IS) = TPDRBG (2) + (IS-10) * TPDRST (2)
   75    CONTINUE
C
C++     Compute angular pad-width, PHI of sector-frame
C++     and leading pad edge
C
         DO 88 I=1,LTSROW*LTSTYP
            TPDPHS(I,1) = 0.
            TPDPHW(I,1) = 0.
            TPDPHF(I,1) = 0.
   88    CONTINUE
C
         DO 91 IS = 1,LTSTYP
            DO  90 IROW = 1, NTPDRW (IS)
               RAD = TPDRBG (IS) + (IROW-1) * TPDRST (IS)
               PHI = TPDSEP (IS) / RAD
               FPADS = FLOAT (NTPDPR (IROW,IS))
               TPDPHS (IROW,IS) = PHI
               TPDPHW (IROW,IS) = 0.5 * PHI * FPADS
               TPDPHF (IROW,IS) = 0.5 * PHI
     +                * (FPADS + 2.*(BORDS/TPDSEP(IS)))
   90       CONTINUE
   91    CONTINUE
C
C++     Compute PHI of edges, half-width and PHI
C++     of centers of Triggerpads
C
         DO 112 IS   = 1, LTSTYP
            DO 111 IROW = 1, NTPTRW(IS)
               P1 = TPDPHW(IROW,IS)
               IF (NTPTPR(IROW,IS).EQ.2) THEN
                  TPTPHC(1,IROW,IS) = -P1/2.
                  TPTPHC(2,IROW,IS) =  P1/2.
                  TPTPHW(1,IROW,IS) =  P1/2.
                  TPTPHW(2,IROW,IS) =  P1/2.
               ELSE
                  P2 = ANTPA (IS)
                  TPTPHC(1,IROW,IS) = - (P1+P2)/2.
                  TPTPHC(2,IROW,IS) = - P2/2.
                  TPTPHC(3,IROW,IS) = - TPTPHC(2,IROW,IS)
                  TPTPHC(4,IROW,IS) = - TPTPHC(1,IROW,IS)
                  TPTPHW(1,IROW,IS) =   (P1-P2)/2.
                  TPTPHW(2,IROW,IS) =   P2/2.
                  TPTPHW(3,IROW,IS) =   TPTPHW(2,IROW,IS)
                  TPTPHW(4,IROW,IS) =   TPTPHW(1,IROW,IS)
               ENDIF
C
               DO 110 IT   = 1,LTTPAD
C
C++        Create numbering scheme of the triggerpads
C
                  ITPADG(IT,IROW,IS) = IROW + ITPD1(IT,IS) - ITPD1(1,IS)
                  IF (IS.EQ.1.OR.IT.EQ.1) GO TO 110
                  IF (IROW .GT. 2*ITPD1(1,IS) - ITPD1(2,IS) ) GO TO 110
C  Situation of rows with 2 tpads
                  IF (IT.EQ.2) THEN
                     ITPADG(IT,IROW,IS) =IROW+ ITPD1(4,IS) - ITPD1(1,IS)
                  ELSE
                     ITPADG(IT,IROW,IS) = 0
                  ENDIF
  110          CONTINUE
  111       CONTINUE
  112    CONTINUE
C
C++     compute radii of tpadrows
C
         DO 170 IS = 1, 8
            TPTROW (IS) = TPTRBG (1) + (IS- 1) * TPTRST (1)
  170    CONTINUE
         DO 175 IS = 9,19
            TPTROW (IS) = TPTRBG (2) + (IS- 9) * TPTRST (2)
  175    CONTINUE
      ENDIF
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I            'TSLO/TNLO':        <<<  Slot positions >>>           I
C  I            -----------         -----------------------           I
C  I                                                                  I
C  I  Generate the constants describing the slots in both endplates.  I
C  I                   (A "slot" is a sector position in the wheel.)  I
C  I                                                                  I
C  +------------------------------------------------------------------+
C
C - Determine whether we're using old or new alignment based on
C   presence of TNLO bank.  Setup appropriate HAC parameters
C
      IF (KTNLO.GT.0) THEN
        KTXLO = KTNLO
        JTXLSN = JTNLSN
        JTXLSB = JTNLSB
        JTXLSS = JTNLSS
        JTXLTM = JTNLTM
        JTXLTS = JTNLTN
      ELSE 
        KTXLO = KTSLO
        JTXLSN = JTSLSN
        JTXLSB = JTSLSB
        JTXLSS = JTSLSS
        JTXLTM = JTSLTM
        JTXLTS = JTSLTS
      ENDIF
      IF (KTXLO .GT. 0) THEN
C
         DO  20 IS = 1, LTSTYP*LTSLOT
            ICHT           = ITABL (KTXLO,IS,JTXLSN)
            ISLOT          = ITABL (KTXLO,IS,JTXLSB)
            ITPSEC (ISLOT) = ITABL (KTXLO,IS,JTXLSS)
            ITPTYP (ISLOT) = ITABL (KTXLO,IS,JTXLTM)
            IENDTP (ISLOT) = ITABL (KTXLO,IS,JTXLTS)
C
            TPPHI0 (ISLOT) = ANFIR (ITPTYP (ISLOT)) +
     +                     ANGSP * MOD (ITPSEC (ISLOT) -1, 6)
            TPCPH0 (ISLOT) = COS (TPPHI0(ISLOT))
            TPSPH0 (ISLOT) = SIN (TPPHI0(ISLOT))
   20    CONTINUE
      ENDIF
C
C++  Calculate wire limits
C
      DO 89 I=1,LTWIRE*LTSTYP
         TWIRMN(I,1) = 0.
         TWIRMX(I,1) = 0.
         TWIRLE(I,1) = 0.
   89 CONTINUE
C
      CALL TGEWIR
C
C - Establish TPC alignment matrices in /TALIGN/ using 'TSCO'
C
       IF ((IW(NAMIND('TSCO')) .GT. 0)
     &      .OR. (IW(NAMIND('TNCO')).GT.0)) THEN
         CALL TALINI
       ENDIF
C
      RETURN
      END
#endif
