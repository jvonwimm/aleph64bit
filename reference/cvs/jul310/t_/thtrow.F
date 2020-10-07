      SUBROUTINE THTROW(HP,IFLG)
C----------------------------------------------------------------
C! Check which TPC rows are hit by a given track
C
C  R. Johnson    12-6-90
C
C  Input(6) HP(6)     /R    Helix parameters in the ALEPH frame,
C                           as from the FRFT bank
C  Output:  IFLG(21)  /I    Flag for each padrow
C                               1= padrow is hit by this track
C                               0= not hit
C
C  Banks needed:  TCLU
C----------------------------------------------------------------
#ifndef DOC
C
#include "tclujj.h"
#include "bcs.h"
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpccon.h"
#include "tzcorr.h"
C
      COMMON/THTRWB/ ITCLUW
C
      SAVE JBIT
      LOGICAL FIRST
      DIMENSION HP(*),HPT(5),IFLG(*)
      DIMENSION ISLT(LTPDRO),RPHIS(LTPDRO),ZS(LTPDRO)
      DATA FIRST / .TRUE. /
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        ITCLUW=0
        JBIT = 0
        DO IJ = 1 ,32
          JBIT = IBSET (JBIT,IJ-1)
        ENDDO
      ENDIF
C
      DO 10 IR=1,LTPDRO
        IFLG(IR)=0
   10 CONTINUE
C
C++   Find intersection points with all TPC padrows
C
      CALL TPDINT(HP,JBIT,ISLT,RPHIS,ZS)
C
C++   Link to all the TCLU banks
C
      CALL BWIND(IW,'TCLU',LTSECT,ITCLUW)
C
C++   Loop over all TPC padrows
C
      DO 500 IR=1,NTPROW
        IF (ISLT(IR).LE.0) GO TO 500
C
C++     What is the sector row number?
C
        IF (IR.LE.NTPDRW(1)) THEN
          IRS=IR
        ELSE
          IRS=IR-NTPDRW(1)
        ENDIF
C
C++     Which pad did the track pass over?
C
        ISTYP=ITPTYP(ISLT(IR))
        NTPAD=NTPDPR(IRS,ISTYP)
        RPHPD= 0.5*FLOAT(NTPAD+1) - RPHIS(IR)/TPDSEP(ISTYP) + 1.0
C
C++     Link to the cluster bank for this sector
C
        IF (IW(ITCLUW).LT.ISLT(IR)) THEN
          KTCLU=0
        ELSE
          KTCLU=IW(ITCLUW+ISLT(IR))
        ENDIF
        IF (KTCLU.EQ.0) GO TO 500
C
C++     Is there a matching cluster?
C
        DO 400 ICL=1,LROWS(KTCLU)
          IPDN=ITABL(KTCLU,ICL,JTCLPN)
          IF (IPDN.NE.IRS) GO TO 400
          RFP=FLOAT(ITABL(KTCLU,ICL,JTCLFP))-0.5
          IF (RFP.LE.RPHPD) THEN
            RLP=FLOAT(ITABL(KTCLU,ICL,JTCLLP))+0.5
            IF (RLP.GE.RPHPD) THEN
C
C++           It matches in r*phi.  Now see if it matches in z.
C++           From the z coordinate, calculate what the bucket number
C++           should be at the center of the pulse.
C
              ZBUCK= ZS(IR)/TVDRFT(IENDTP(ISLT(IR))) + TVOFFS(ISLT(IR))
              ZBUCK= ZBUCK - TZOFFS(2)
              RFB=FLOAT(ITABL(KTCLU,ICL,JTCLFB))
              IF (ZBUCK.GE.RFB) THEN
                RLB=FLOAT(ITABL(KTCLU,ICL,JTCLLB))
                IF (ZBUCK.LE.RLB) THEN
                  IFLG(IR)=1
                  GO TO 500
                ENDIF
              ENDIF
            ENDIF
          ENDIF
  400   CONTINUE
  500 CONTINUE
C
      CALL WDROP(IW,ITCLUW)
      RETURN
      END
#endif
