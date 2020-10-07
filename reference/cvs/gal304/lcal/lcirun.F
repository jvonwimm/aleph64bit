*DK lcirun
      SUBROUTINE LCIRUN
C--------------------------------------------------------------
C! Initialise LCal constants
C! Author   : J.Dines Hansen & P.Hansen - 860417
C! Modified : P.Hansen - 891120
C! Modified : P.Hansen - 950202
C! Description :
C! =============
C!   Fills /LCCOMC/ and /LCCOMD/ from data base
C! Modified : B.Bloch-Devaux - 920215 to access LALI via GTSTUP
C!   BOS bank initialisation
C!   Called by  USIJOB                          from this  .HLB
C!   Calls      LCNAMI                          from this  .HLB
C!              NAMIND,BKFMT                    from BOS
C -----------------------------------------------
#ifndef DOC
      CHARACTER*20 NAVN
      EXTERNAL NAMIND
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "lcnamc.h"
#include "ljjpar.h"
#include "lccomc.h"
#include "lccomd.h"
      INTEGER  ALGTDB,GTSTUP
      CHARACTER*44 LIST
      DATA LIST /'LCALLLAYLMTYLSCOLSLOLWRGLCELLSHOLDRELCCA'/
#include "bmacro.h"
C -------------------------------------------------------------
C
C? Initialize print flag
      IF(ICLCJO(1).GE.2) WRITE(LOUTIO,1000)
      LCPRNT = 0
C
C? Set name indices for BOS banks
      CALL LCNAMI
C
C? Get geometry banks from dbase
      NR = ALGTDB (LRDBIO,LIST,IRUNJO)
      IF(NR.EQ.0) GOTO 997
C? Get LALI bank from setup number and store it in Run header
      ILCST = GTSTUP('LC',1)
      NR = ALGTDB (LRDBIO,'LALI',ILCST)
      CALL BLIST(IW,'C+','LALI')
      IF (NR.EQ.0) GO TO 997
C
C? Get optional geometry banks
      NR = ALGTDB (LRDBIO,'LCPG',IRUNJO)
C
C? Get geometry and constant bank indices
      KLCAL = IW(NALCAL)
      KLMTY = IW(NALMTY)
      KLLAY = IW(NALLAY)
      KLSCO = IW(NALSCO)
      KLSLO = IW(NALSLO)
      KLWRG = IW(NALWRG)
      KLCEL = IW(NALCEL)
      KLSHO = IW(NALSHO)
      KLDRE = IW(NALDRE)
C
C? Get MeV per ADC count
      ADCOLC    = RTABL(KLCEL,1,JLCEAM)
      LCADCO    = NINT(ADCOLC)
C? Get energy sampling unit
      ECRTLC    = RTABL(KLSHO,1,JLSHEC)
C? Get average Zs
      ZMATLC(1) = RTABL(KLSHO,1,JLSHZ1)
      ZMATLC(2) = RTABL(KLSHO,1,JLSHZ2)
C? Get radiation lengths
      RADLLC(1) = RTABL(KLSHO,1,JLSHR1)
      RADLLC(2) = RTABL(KLSHO,1,JLSHR2)
C? Get threshold for starting a shower
      ECUTLC    = RTABL(KLSHO,1,JLSHET)
      IF(ECUTLC.GT.1.) ECUTLC=0.01
C? Get shower parameters
      PAR1LC   = RTABL(KLSHO,1,JLSHP1)
      PAR2LC   = RTABL(KLSHO,1,JLSHP2)
      PAR3LC   = RTABL(KLSHO,1,JLSHP3)
      PAR4LC   = RTABL(KLSHO,1,JLSHP4)
      PAR5LC   = RTABL(KLSHO,1,JLSHP5)
      PAR6LC   = RTABL(KLSHO,1,JLSHP6)
      PAR7LC   = RTABL(KLSHO,1,JLSHP7)
      PAR8LC   = RTABL(KLSHO,1,JLSHP8)
      DPR1LC   = RTABL(KLSHO,1,JLSHD1)
      DPR2LC   = RTABL(KLSHO,1,JLSHD2)
      DPR3LC   = RTABL(KLSHO,1,JLSHD3)
      DPR4LC   = RTABL(KLSHO,1,JLSHD4)
      DPR5LC   = RTABL(KLSHO,1,JLSHD5)
C? Get mip energy per sampling (Ecrtlc units)
      EELALC   = RTABL(KLSHO,1,JLSHEM)
C? Get number of layers in each storey
      LCNLAY(1) = ITABL(KLMTY,1,JLMTNL)
      LCNLAY(2) = ITABL(KLMTY,1,JLMTNL+1)
      LCNLAY(3) = ITABL(KLMTY,1,JLMTNL+2)
      LCNWPL    = LCNLAY(1) + LCNLAY(2) + LCNLAY(3)
C? Get subcomponent coordinates
        PLATE = RTABL(KLMTY,1,JLMTFL)
        DO 310 I=1,2
        DO 300 J=1,3
        JX = JLSCRP + J-1
 300    XYZOLC(J,I) = RTABL(KLSCO,I,JX)
 310    CONTINUE
        XYZOLC(3,1) = XYZOLC(3,1) - PLATE
        XYZOLC(3,2) = XYZOLC(3,2) + PLATE
        Z0 = RTABL(KLCAL,1,JLCAZD) + PLATE
        DZ = 0.
C? Get reference z of each storey
        DO 400 I = 1,3
         JDEPT = JLMTST+I-1
         Z123LC(I) = RTABL(KLMTY,1,JDEPT)
         ZSTPLC(I) = Z123LC(I)/FLOAT(LCNLAY(I))
         ZREFLC(I) = Z0 + DZ + 0.5*Z123LC(I)
         DZ        = DZ + Z123LC(I)
  400   CONTINUE
C? Get steplength for shower sampling in r.l.
         SSTPLC(1) = ZSTPLC(1)/RADLLC(1)
         SSTPLC(2) = ZSTPLC(2)/RADLLC(1)
         SSTPLC(3) = ZSTPLC(3)/RADLLC(2)
C? Hit to energy conversion (MeV)
      CHTOE(1) = 1000.*ECRTLC
      CHTOE(2) = CHTOE(1)
      CHTOE(3) = 1000.*ECRTLC*SSTPLC(3)/SSTPLC(2)
C? Coherent noise in ADC counts
      COHNLC       = RTABL(KLCEL,1,JLCECN)
C? Noise level in wireplanes in ADC counts
      WNOILC(1)    = RTABL(KLCEL,1,JLCEWN)
      WNOILC(2)    = RTABL(KLCEL,1,JLCEWN+1)
      WNOILC(3)    = RTABL(KLCEL,1,JLCEWN+2)
C? Noise level in storeys in ADC counts
      SNOILC(1)    = RTABL(KLCEL,1,JLCESN)
      SNOILC(2)    = RTABL(KLCEL,1,JLCESN+1)
      SNOILC(3)    = RTABL(KLCEL,1,JLCESN+2)
C? Noise level in trigger sums in ADC counts
      TNOILC(1)    = RTABL(KLCEL,1,JLCETN)
      TNOILC(2)    = RTABL(KLCEL,1,JLCETN+1)
      TNOILC(3)    = RTABL(KLCEL,1,JLCETN+2)
C? Readout threshold for storeys in MeV
      LCSTRH(1)    = ITABL(KLCEL,1,JLCETH)
      LCSTRH(2)    = ITABL(KLCEL,1,JLCETH+1)
      LCSTRH(3)    = ITABL(KLCEL,1,JLCETH+2)
C? Sigma of relative gain variation
      GVARLC       = RTABL(KLCEL,1,JLCEGV)
C? Extra DE/E constant term
      SCONLC   = RTABL(KLSHO,1,JLSHSC)
C? Extra DE/E term: SSAMLC/SQRT(E)
      SSAMLC   = RTABL(KLSHO,1,JLSHSS)
C? Trigger threshold in MeV
      LCBHTR       = ITABL(KLCEL,1,JLCETR)
C
C? Places where the pads are cut so that may not cover
C? the wires.
      Y0CUT = RTABL(KLDRE,1,JLDRY0)
      DYCUT = RTABL(KLDRE,1,JLDRDY)
      Y5CUT = RTABL(KLDRE,1,JLDRY5)
C
C? Positions of wiresupport in the middle of the chamber
C? XLSUP=low x position. XHSUP=high x position. YSUP=y position.
C? First index: 1 if odd plane and x*y<0 or even plane and x*y>0.
C?              2 otherwise
C? Second index: support number (two in each quadrant)
      XLSUP(1,1) = RTABL(KLDRE,1,JLDRXL)
      XHSUP(1,1) = RTABL(KLDRE,1,JLDRXH)
      XLSUP(1,2) = RTABL(KLDRE,1,JLDRXL+2)
      XHSUP(1,2) = RTABL(KLDRE,1,JLDRXH+2)
      XLSUP(2,1) = RTABL(KLDRE,1,JLDRXL+1)
      XHSUP(2,1) = RTABL(KLDRE,1,JLDRXH+1)
      XLSUP(2,2) = RTABL(KLDRE,1,JLDRXL+3)
      XHSUP(2,2) = RTABL(KLDRE,1,JLDRXH+3)
      YSUP(1,1) = RTABL(KLDRE,1,JLDRYS)
      YSUP(2,1) = RTABL(KLDRE,1,JLDRYS+1)
      YSUP(1,2) = RTABL(KLDRE,1,JLDRYS+2)
      YSUP(2,2) = RTABL(KLDRE,1,JLDRYS+3)
C
C? Distance from wire-plane to end-of-layer
      DWIRLC = RTABL(KLDRE,1,JLDRDW)
C
C? The wire support covers in each end
      DY1 = RTABL(KLDRE,1,JLDRDT)
C
C? Half width of an internal wiresupport
      DY3 = RTABL(KLDRE,1,JLDRDS)
C
C? Half an inter-pad space
      HUNIT = RTABL(KLMTY,1,JLMTCC)*0.5
C
C? Create job-summary bank
      IF(IDETJO(5).NE.0) THEN
         NALCWS = NAMIND('LCWS')
         CALL BKFMT('LCWS','I')
         LEN = 1 * 12 + LMHLEN
         CALL ALBOS ('LCWS',0,LEN,KINDX,IGARB)
         IW(KINDX+1) = 12
         IW(KINDX+2) = 1
      ENDIF
C
      GOTO 999
  997 CALL ALTELL('LCIRUN: missing data base banks - STOP',0,'STOP')
  999 CONTINUE
 1000 FORMAT(2X,'++ LCIRUN ++')
      END
#endif
