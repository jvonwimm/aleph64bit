*DK tpugeo
      SUBROUTINE TPUGEO
C ----------------------------------------------------------------------
C. - M.Mermikides - 860921
C!  Fill commom blocks TPGEOM,TPGEOP,TPGEOW,TPGEOT
C.   with useful TPC geometric quantities derived from data base
C ---------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "jobcom.h"
#include "iocom.h"
#include "jqcom.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpgeow.h"
#include "tpgeot.h"
C
      EXTERNAL NAMIND
      DIMENSION ANFIR(LTSTYP),ANTPA(LTSTYP)
      DIMENSION ITPD1 (LTTPAD,LTSTYP)
C
C ---------------------------------------------------------------------
      NTPROW = LTPDRO
      NTSECT = LTSECT
C
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
      IF (KTSGM.EQ.0) THEN
         CALL ALTELL ('TPUGEO: missing bank TSGM ',0,'STOP')
      ENDIF
      IANF = KTSGM + 3
C
      DO 40 IS = 1, LTSTYP
         IFRQ  = (IS-1) * IW(KTSGM+1)
         NTPCRN (IS) = IW (IANF + IFRQ +3)
         DO 30 ICRN = 1,5
            TPCORN (1,ICRN,IS) = RW (IANF + IFRQ + 2 + ICRN +  6)
            TPCORN (2,ICRN,IS) = RW (IANF + IFRQ + 2 + ICRN +  1)
   30    CONTINUE
   40 CONTINUE
C
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
      IF (KTCGD.EQ.0) THEN
         CALL ALTELL ('TPUGEO: missing bank TCGD ',0,'STOP')
      ENDIF
      IANF = KTCGD + 3
C Angular spacing between sectors of the same type (nominally = pi/3)
      ANGSP= RW (IANF +  7)
C  Distance of edgde of first pad from sector border
      BORDS      = RW (IANF + 15)
C
C++     Fill general constants for pads into   /TPGEOP/
C
      DO 220 IS = 1,LTSTYP
         TPDRST (IS) = RW (IANF +  8)
         TPDHGT (IS) = RW (IANF +  9)
         TPDSEP (IS) = RW (IANF + 10)
         TPDWID (IS) = RW (IANF + 11)
         TPDHWD (IS) = RW (IANF + 13)
C
C         General constants for trigger - pads into   /TPGEOT/
C
         TPTRST (IS) = RW (IANF + 16)
         TPTRHG (IS) = RW (IANF + 17)
C
C         General constants for wires into   /TPGEOW/
C
         TWSTEP (IS) = RW (IANF + 18)
C
  220 CONTINUE
C
C     Wall thickness of TPC
C
       DRTPMN      = RW (IANF + 23)
       DRTPMX      = RW (IANF + 24)
       DZTPMX      = RW (IANF + 25)
C
C     Compute inner dimensions (sensitive volume)
C
       RTPCMN = RW(IANF + 20) + DRTPMN
       RTPCMX = RW(IANF + 21) - DRTPMX
       ZTPCMX = RW(IANF + 22) - DZTPMX
C
C    Wheel rib - thickness, rib - width and equivalent thickness
C
       TPFRDZ      = RW (IANF + 26)
       TPFRDW      = RW (IANF + 28)
       TPAVDZ      = RW (IANF + 27)
C
C    Wheel rib offsets (describing the kinks in the ribs)
C
       TPFOF1      = RW (IANF + 29)
       TPFOF2      = RW (IANF + 30)
       TPFOF3      = RW (IANF + 31)
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
      IF (KTMTY.EQ.0) THEN
         CALL ALTELL ('TPUGEO: missing bank TMTY ',0,'STOP')
      ENDIF
      IEND = KTMTY + IW (KTMTY) -4
      IANF = KTMTY + 3
C
      DO 80 IS = 1, LTSTYP
         IFRQ  = (IS-1) * IW(KTMTY+1)
         ANFIR (IS)      = RW (IANF + IFRQ + 4)
C      radius of 1. pad row:
         TPDRBG (IS)      = RW (IANF + IFRQ + 5)
C      number of padrows
         NTPDRW (IS)      = IW (IANF + IFRQ + 6)
C      number of pads in each padrow
         DO  50 IROW = 1,12
            NTPDPR (IROW,IS) = IW (IANF + IFRQ + 6 + IROW)
   50    CONTINUE
C
C      radius of 1. tpad row:
         TPTRBG (IS)      = RW (IANF + IFRQ + 19)
C      number of tpadrows
         NTPTRW (IS)      = IW (IANF + IFRQ + 20)
C
C      number of tpads in each tpadrow
         DO 150 IROW = 1,11
            NTPTPR (IROW,IS) = IW (IANF + IFRQ + 20 + IROW)
  150    CONTINUE
C
         ANTPA(IS) = RW (IANF + IFRQ + 32)
C  Trigger pad numbering
         ITPD1(1,IS) = IW(IANF + IFRQ + 33)
         ITPD1(2,IS) = IW(IANF + IFRQ + 34)
         ITPD1(3,IS) = IW(IANF + IFRQ + 35)
         ITPD1(4,IS) = IW(IANF + IFRQ + 36)
C      radius of center of 1. wire, # of wires:
         TWIRE1 (IS)      = RW (IANF + IFRQ + 37)
         NTWIRE (IS)      = IW (IANF + IFRQ + 38)
C
   80 CONTINUE
C
C++     compute radii of padrows
C
      DO  70 IS = 1,9
         TPPROW (IS) = TPDRBG (1) + (IS- 1) * TPDRST (1)
   70 CONTINUE
      DO  75 IS = 10,21
         TPPROW (IS) = TPDRBG (2) + (IS-10) * TPDRST (2)
   75 CONTINUE
C
C++     Compute angular pad-width, PHI of sector-frame
C++     and leading pad edge
C
      DO 88 I=1,LTSROW*LTSTYP
         TPDPHS(I,1) = 0.
         TPDPHW(I,1) = 0.
         TPDPHF(I,1) = 0.
   88 CONTINUE
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
   90    CONTINUE
   91 CONTINUE
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
                  ITPADG(IT,IROW,IS) =IROW + ITPD1(4,IS) - ITPD1(1,IS)
               ELSE
                  ITPADG(IT,IROW,IS) = 0
               ENDIF
  110       CONTINUE
  111    CONTINUE
  112 CONTINUE
C
C++     compute radii of tpadrows
C
      DO 170 IS = 1, 8
         TPTROW (IS) = TPTRBG (1) + (IS- 1) * TPTRST (1)
  170 CONTINUE
      DO 175 IS = 9,19
         TPTROW (IS) = TPTRBG (2) + (IS- 9) * TPTRST (2)
  175 CONTINUE
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I            'TSLO':             <<<  Slot positions >>>           I
C  I            -------             -----------------------           I
C  I                                                                  I
C  I  Generate the constants describing the slots in both endplates.  I
C  I                   (A "slot" is a sector position in the wheel.)  I
C  I                                                                  I
C  +------------------------------------------------------------------+
C
      KTSLO = IW(NAMIND('TSLO'))
      IF (KTSLO.EQ.0) THEN
         CALL ALTELL ('TPUGEO: missing bank TSLO ',0,'STOP')
      ENDIF
      IEND = KTSLO + IW (KTSLO) -4
      IANF = KTSLO + 3
C
      DO  20 IS = 1, LTSTYP*LTSLOT
         IFRQ           = (IS-1) * IW(KTSLO + 1)
         ICHT           = IW(IANF+IFRQ+3)
         ISLOT          = IW (IANF + IFRQ + 4)
         ITPSEC (ISLOT) = IW (IANF + IFRQ + 5)
         ITPTYP (ISLOT) = IW (IANF + IFRQ + 9)
         IENDTP (ISLOT) = IW (IANF + IFRQ +10)
C
         TPPHI0 (ISLOT) = ANFIR (ITPTYP (ISLOT)) +
     +                     ANGSP * MOD (ITPSEC (ISLOT) -1, 6)
         TPCPH0 (ISLOT) = COS (TPPHI0(ISLOT))
         TPSPH0 (ISLOT) = SIN (TPPHI0(ISLOT))
   20 CONTINUE
C
C++  Calculate wire limits (PRESENTLY NOT DEFINED FOR GALEPH)
C
      DO 89 I=1,LTWIRE*LTSTYP
         TWIRMN(I,1) = 0.
         TWIRMX(I,1) = 0.
         TWIRLE(I,1) = 0.
   89 CONTINUE
C
C  Debug
C
      IF (IPRIJO(3).GT.0) THEN
         WRITE(LOUTIO,901) RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,
     *                     DZTPMX,TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,
     *                     TPFOF2,TPFOF3, TPPROW
 901     FORMAT(' +++TPUGEO+++ TPC Geometry ',//
     *        ' RMIN,RMAX,ZMAX,DRMIN,DRMAX,DZ ',6F10.4,/,
     *        ' Sector support frame dimens. ',6F10.4,//,
     *        ' PADROW RADII ',17X,5F10.4/(31X,5F10.4))
         WRITE(LOUTIO,914) TPTROW
 914     FORMAT(/, ' TRIGGER PADROW RADII ',9X,5F10.4/(31X,5F10.4))
         WRITE(LOUTIO,920) ITPTYP,ITPSEC
 920     FORMAT(//' Sector geometry',//,' ITPTYP ',18I3/8X,18I3//,
     *             ' ITPSEC ',18I3/8X,18I3)
         WRITE(LOUTIO,902) TPPHI0
 902     FORMAT (' Slot positions PHI0',6F10.4/(20X,6F10.4))
C
         DO 201 IT = 1,LTSTYP
            WRITE(LOUTIO, 903) IT, (TPDPHF(M,IT),M=1,NTPDRW(IT))
 903        FORMAT(//,' SECTOR TYPE',I3,' PHIF',6F10.4/20X,6F10.4)
      WRITE (LOUTIO,913) (TPDPHW(M,IT),M=1,NTPDRW(IT))
 913        FORMAT(7X,'PAD PHI START',6F10.5/20X,6F10.5)
            WRITE(LOUTIO,905) (TPCORN(M,1,IT),M=1,2*NTPCRN(IT))
 905        FORMAT(16X,'CORN',6F10.4/20X,6F10.4)
            WRITE(LOUTIO,911) TPDRBG(IT),TPDSEP(IT),TPDWID(IT),
     &                        TPDHWD(IT),TPDHGT(IT),TPDRST(IT)
 911     FORMAT(/, ' PADS ',/ 7X,'R1, PITCH, WID , HALFW , H, DR ',
     *          1X,6F8.4/)
            WRITE(LOUTIO,906) TPTRBG(IT),TPTRST(IT),TPTRHG(IT),
     *                        (NTPTPR(M,IT),M=1,NTPTRW(IT))
  906        FORMAT(/,' TRIGGER PADS  ',/ 7X,'R1, DR, H/2 =', 3F10.4,
     *            /,12X,'PADS/ROW ',11I4)
            WRITE (LOUTIO,907)
  907      FORMAT(/' ROW,  #,  Phipos / Phiwid '/)
            DO 211 IR = 1,NTPTRW(IT)
               WRITE (LOUTIO,908) IR, (ITPADG(IP,IR,IT),IP=1,4),
     &                  (TPTPHC(IP,IR,IT), IP=1,NTPTPR(IR,IT))
               WRITE (LOUTIO,909) (TPTPHW(IP,IR,IT),IP=1,NTPTPR(IR,IT))
  908          FORMAT (/,2X, I3,')', 4I4, 4F10.5)
  909          FORMAT (22X, 4F10.5)
  211       CONTINUE
            WRITE(LOUTIO,912) NTWIRE(IT),TWSTEP(IT),TWIRE1(IT)
  912       FORMAT(/,' SENSE WIRES (No, Step, R0 ',I8,2F10.4)
  201    CONTINUE
      ENDIF
C
      RETURN
      END
#endif
