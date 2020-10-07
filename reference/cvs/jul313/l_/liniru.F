      SUBROUTINE LINIRU
C-----------------------------------------------------------------------
C! Lcal run initialisation
C!
C!   Author   : P. H. Hansen  880307
C!   modified : F.Ranjard     920330
C!              get the LALI bank:
C!              montecarlo -from run header or data cards or DB using
C!              the setup code
C!              real data  -from data cards or DB using the run number
C!              F.Ranjard     930416
C!              montecarlo - use AGETDB instead of MDARD to get LALI
C!
C!   Modified :- E. Lancon             17-MAY-1993
C!        create LEHI, LUMI, LBak, LVHI, LONL with nr = run number
C!   Modified :- M. Cattaneo           16-JUN-1997
C!        get BHZ0 bank from database
C!   Modified :- P.Hansen, M. Cattaneo 17-JUL-1997
C!        Enable taking LDST from start of run record
C!
C!   Input    : none
C!   Output   :
C!              LCRE bank (constants used in cluster reconstruction)
C!              LMTY bank (constants describing a LCAL module type)
C!              LSLO bank (position of a LCAL module)
C!              LALI bank (alignment corrections)
C!              LRWG bank (constants specific for a row of towers)
C!              LLAY bank (constants specific for a wire plane)
C!              BHAB bank (Cuts for Bhabha event selection)
C!              BHZ0 bank (Cuts for Bhabha event selection)
C!              LECA bank (Lcal energy calibration)
C!              LDST bank (Lcal dead storey list)
C!              LDWP bank (Lcal dead wire plane list)
C!
C!              LUMI bank (luminosity run statistics)
C!              LBAK bank (luminosity background statistics)
C!              LONL bank (trigger statistics)
C!              LEHI Bbank (energy histograms)
C!              LVHI Bbank (vertex histograms)
C!   Description :
C!   ===========
C?   Read geometry and constant banks from DAF
C?   Temporary quick fixes of data base banks
C?   Reset luminosity, background and trigger statistics bank
C!=====================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "lumijj.h"
#include "lalijj.h"
#include "lonljj.h"
#include "lbakjj.h"
#include "lphist.h"
#include "lehijj.h"
#include "lvhijj.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "rconds.h"
      INTEGER  ALGTDB, GTSTUP, AGETDB
      EXTERNAL ALGTDB, GTSTUP, AGETDB
#include "bmacro.h"
C----------------------------------------------------------------------
C
C - Get LALI bank
C
      NALALI = NAMIND('LALI')
      IF (FMCRUN) THEN
C     montecarlo run:
C      if the LALI bank is on the run header or on data cards take it
C      if not take it from the DB with the setup code
         JLALI = IW(NALALI)
         IF (JLALI.EQ.0) THEN
            ILSTP = GTSTUP ('LC',IRUNRC)
            NR = AGETDB ('LALI',-ILSTP)
         ENDIF
      ELSE
C     real data
C      get LALI bank from DB or data cards
         NR = AGETDB ('LALI',-IRUNRC)
      ENDIF
      IF (IW(NALALI) .EQ. 0) THEN
         CALL REPORT('LINIRU','Missing LALI bank',1)
      ENDIF
C
C - Get other geometry banks
C
      NR = ALGTDB(LRGEOM,'LMTYLSLOLRWGLWRGLLAYLCRELCAL',IRUNRC)
      IF(NR.EQ.0) THEN
        CALL REPORT('LINIRU','Missing LCAL banks on DAF',1)
      ENDIF
C
      NR = ALGTDB(LRGEOM,'BHABLACCLECALDWPLDSTLCPG',-IRUNRC)
      IF(NR.EQ.0.AND.IRUNRC.GT.2000) THEN
        CALL REPORT('LINIRU','Missing LCAL banks on DAF',0)
      ENDIF
C
      NR = ALGTDB(LRGEOM,'BHZ0',-IRUNRC)
      IF(NR.EQ.0.AND.IRUNRC.GT.40000) THEN
        CALL REPORT('LINIRU','Missing LCAL BHZ0 bank on DAF',0)
      ENDIF
C   Read platine gains and run info from data base
      IREADB = ALGTDB(LRCONS,'LCMCRINF',-IRUNRC)
C
C Reset trigger statistics bank
      KLONL = IW(NALONL)
      IF(KLONL.GT.0) THEN
        CALL VZERO(IW(KLONL+LMHLEN+1),LLONLA)
      ELSE
        LEN = LMHLEN + LLONLA
        CALL AUBOS('LONL',IRUNRC,LEN,KLONL,IGARB)
        IF(IGARB.EQ.2)                                     GOTO 998
        IW(KLONL+1) = LLONLA
        IW(KLONL+2) = 1
      ENDIF
C
C Reset luminosity statistics bank
      KLUMI = IW(NALUMI)
      KBHAB = IW(NABHAB)
      IF(KLUMI.GT.0) THEN
        LEN = LLUMIA*LROWS(KLUMI)
        CALL VZERO(RW(KLUMI+LMHLEN+1),LEN)
      ELSE
        LEN = LMHLEN + LLUMIA*LROWS(KBHAB)
        CALL AUBOS('LUMI',IRUNRC,LEN,KLUMI,IGARB)
        IF(IGARB.EQ.2)                                     GOTO 998
        IW(KLUMI+1) = LLUMIA
        IW(KLUMI+2) = LROWS(KBHAB)
      ENDIF
C
C Reset background statistics bank
      KLBAK = IW(NALBAK)
      KBHAB = IW(NABHAB)
      IF(KLBAK.GT.0) THEN
        LEN = LLBAKA*LROWS(KLBAK)
        CALL VZERO(RW(KLBAK+LMHLEN+1),LEN)
      ELSE
        LEN = LMHLEN + LLBAKA*LROWS(KBHAB)
        CALL AUBOS('LBAK',IRUNRC,LEN,KLBAK,IGARB)
        IF(IGARB.EQ.2)                                     GOTO 998
        IW(KLBAK+1) = LLBAKA
        IW(KLBAK+2) = LROWS(KBHAB)
      ENDIF
C
C Reset energy histogram bank
      KLEHI = IW(NALEHI)
      IF(KLEHI.GT.0) THEN
        LEN = LLEHIA*8
        CALL VZERO(RW(KLEHI+LMHLEN+1),LEN)
      ELSE
        LEN = LMHLEN + LLEHIA*8
        CALL AUBOS('LEHI',IRUNRC,LEN,KLEHI,IGARB)
        IF(IGARB.EQ.2)                                     GOTO 998
        IW(KLEHI+1) = LLEHIA
        IW(KLEHI+2) = 8
      ENDIF
C
C Reset vertex histogram bank
      KLVHI = IW(NALVHI)
      IF(KLVHI.GT.0) THEN
        LEN = LLVHIA*4
        CALL VZERO(RW(KLVHI+LMHLEN+1),LEN)
      ELSE
        LEN = LMHLEN + LLVHIA*4
        CALL AUBOS('LVHI',IRUNRC,LEN,KLVHI,IGARB)
        IF(IGARB.EQ.2)                                     GOTO 998
        IW(KLVHI+1) = LLVHIA
        IW(KLVHI+2) = 4
      ENDIF
C
C Fill the two descriptive words in each row
      ELO = FLOAT(INT(ENERRC/2.))-10.
      DO 100 I=1,8
        KL = KROW(KLEHI,I)
        RW(KL+JLEHBI) = BINSI
        RW(KL+JLEHEL) = ELO
  100 CONTINUE
      DO 101 I=1,4
        KL = KROW(KLVHI,I)
        RW(KL+JLVHBI) = BINSX
        RW(KL+JLVHXL) = XLOW
  101 CONTINUE
      GOTO 999
C
  998 CALL REPORT('LINIRU','AUBOS failure',1)
  999 CONTINUE
      END
#endif
