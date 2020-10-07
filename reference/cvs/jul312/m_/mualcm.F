        SUBROUTINE MUALCM(IEL,SLTCO,ALECO)
C----------------------------------------------------------------
C! Transformation from slot coord. to ALEPH m.r.s.
C!
C! P.Campana    890608
C!
C?
C!======================================================================
#ifndef DOC
#include "mgnpar.h"
#include "mgdeca.h"
#include "mgdmda.h"
#include "mgdbar.h"
#include "alcons.h"
        INTEGER IEL(*)
        REAL SLTCO(*),ALECO(*),DIST(2)
C Choose subc.
        IF(IEL(1)-2) 1,2,3
C Barrel
 1      IF(MBVOLU(IEL(2)).GE.2.AND.MBVOLU(IEL(2)).LE.6) THEN
             IDIST=1
             DIST(1)=0
             DIST(2)=BPLADI
        ELSE
             IDIST=-1
             DIST(1)=BPLADI
             DIST(2)=0
        ENDIF
        XCS=BRBOXG(MBPNT2(IEL(2)))/2.
        DXCC=SLTCO(1)-XCS-FLOAT(IDIST)*BDELTC(IEL(2))
        RINT=BERREC(IEL(2))+DIST(IEL(11))
        DFI=ATAN2(DXCC,RINT)
        PHI=PIBY6*FLOAT(MBVOLU(IEL(2))-1)
        ALECO(3)=PHI-DFI*FLOAT(IDIST)
        IF(ALECO(3).LT.0) ALECO(3)=ALECO(3)+TWOPI
        RR=RINT/COS(DFI)
        DHEMS=SLTCO(2)-BZBOXG(MBPNT2(IEL(2)))/2.+BZETAC(IEL(2))
        ALECO(1)=SQRT(RR*RR+DHEMS*DHEMS)
        ALECO(2)=ACOS(DHEMS/ALECO(1))
        GOTO 9
C
C End Cap
C
 2      CONTINUE
        ITYPE=MEPNTY(IEL(2))
        XSL=SLTCO(1)-EYYBOX(ITYPE)/2.
        YSL=SLTCO(2)-EXXBOX(ITYPE)/2.
        XAL=YSL+EXXXXC(IEL(2))
        YAL=XSL+EYYYYC(IEL(2))
        ISIGN=1
        IF(EZETAC(IEL(2)).LT.0) ISIGN=-1
        ZAL=EZETAC(IEL(2))+FLOAT(ISIGN*(IEL(11)-1))*EPLADI
        ER=SQRT(XAL*XAL+YAL*YAL)
        ALECO(3)=ATAN2(YAL,XAL)
        IF(ALECO(3).LT.0) ALECO(3)=ALECO(3)+TWOPI
        ALECO(1)=SQRT(ER*ER+ZAL*ZAL)
        ALECO(2)=ASIN(ER/ALECO(1))
        IF(ISIGN.LT.0) ALECO(2)=-ALECO(2)+PI
        GOTO 9
C Middle angle
 3      IF(MAVOLU(IEL(2)).GE.2.AND.MAVOLU(IEL(2)).LE.6) THEN
              IDIST=1
              DIST(1)=0.
              DIST(2)=APLADI
        ELSE
              IDIST=-1
              DIST(1)=APLADI
              DIST(2)=0.
        ENDIF
        XCS=ARRBOX(MAPNTY(IEL(2)))/2.
        DXCC=SLTCO(2)-XCS-FLOAT(IDIST)*ADELTC(IEL(2))
        RINT=AERREC(IEL(2))+DIST(IEL(11))
        DFI=ATAN2(DXCC,RINT)
        PHI=PIBY6*FLOAT(MAVOLU(IEL(2))-1)
        ALECO(3)=PHI-DFI*FLOAT(IDIST)
        IF(ALECO(3).LT.0) ALECO(3)=ALECO(3)+TWOPI
        RR=RINT/COS(DFI)
        DHEMS=SLTCO(1)-0.5*AZZBOX(MAPNTY(IEL(2)))+AZETAC(IEL(2))
        ALECO(1)=SQRT(RR*RR+DHEMS*DHEMS)
        ALECO(2)=ACOS(DHEMS/ALECO(1))
C
 9      RETURN
        END
#endif
