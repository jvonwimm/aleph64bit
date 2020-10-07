          SUBROUTINE MUSLCM(IEL,SLTCO,IOK)
C-------------------------------------------------------------
C! Subroutine transforming from raw data to slot coordinate system
C!
C! P.Campana    890608
C!
C?
C!======================================================================
#ifndef DOC
#include "mgnpar.h"
#include "mgdeca.h"
#include "mgeeca.h"
#include "mgdmda.h"
#include "mgemda.h"
#include "mgdcom.h"
#include "mgdbar.h"
#include "mgebar.h"
          INTEGER IEL(*)
          REAL SLTCO(*)
C Choose subcomponent
          IXINV=0
          IYINV=0
          ICH=0
C
          IOK=1
C
          IF(IEL(1)-2) 1,2,3
C
C Barrel - X strips
C
 1        CONTINUE
C X traitment
C Remove offsets
          IEL(5)=IEL(5)-MBLOBU(IEL(3),IEL(9))
C All bus readout must be inverted, except 9B, 11B
          ITYSL=MBPNT2(IEL(2))
          IF(IEL(2).NE.12.AND.IEL(2).NE.16.AND.
     +       IEL(2).NE.29.AND.IEL(2).NE.33) THEN
                    IXINV=1
                    IEL(5)=MBNSXT(IEL(11),ITYSL)-IEL(5)
          ENDIF
C Evaluation of X local coordinate (taken from left side)
C (if inv. occurs nch starts from str. #1, otherw. from #0)
          N16T=IEL(5)/16
          NCH=IEL(5)-N16T*16
          IF(NCH.GE.(8+IXINV)) ICH=1
          XOFF=BTOFFS(IEL(11),IEL(2))
          SLTCO(1)=XOFF+FLOAT(N16T)*BPIT16(ITYSL)+SPA88T/2.+
     +        TUBCEL*FLOAT(NCH-IXINV)+TUBCEL/2.+SPA88T*FLOAT(ICH)
C Y traitment
C Identify "criminal" bus
          ICRI=0
          ITYBU=MBTYBU(IEL(4),IEL(10))
          IF((ITYBU.EQ.2.OR.ITYBU.EQ.6).AND.IEL(4).NE.2) ICRI=1
C Remove offset
          IF(MBLOBU(IEL(4),IEL(10)).LE.10) THEN
               ICRI=0
C                      ! No additional ten strips for first group
               IEL(6)=IEL(6)-MBLOBU(IEL(4),IEL(10))+1
          ELSE
C Adds intermediate offsets
               IEL(6)=IEL(6)+MBLOBU(IEL(4),IEL(10))-ICRI*10+1
          ENDIF
C Finds physical strip
C Inversion for busse #1 (all strips must start from side B) or for
C busse #2 for int. slots 9B 10 11A
C (if inv. occurs nch starts from str. #0, otherw. from #1)
          IF((IEL(2).GE.12.AND.IEL(2).LE.14).AND.(IEL(11).EQ.2))IYINV=1
          IF((IEL(2).LE.11.OR.IEL(2).GE.15).AND.(IEL(11).EQ.1)) IYINV=1
          IF(IYINV.EQ.1) IEL(6)=MBNSY1(ITYSL)-IEL(6)
C
          SLTCO(2)=BYOFFS(IEL(12),IEL(2))+FLOAT(IEL(6)-1+IYINV)*YPITST+
     +             YPITST/2.
C Check: X & Y coord. must lie within fiduc. volume (IOK= 1  ok,
C IOK=0 assoc. incomp.,IOK=-1 error)
          XD=BRTOOT(ITYSL)
          YD=BZTOOT(ITYSL)
          XTD=BRBOXG(ITYSL)-XD
          YTD=BZBOXG(ITYSL)-YD
          IOK=1
          IF(SLTCO(1).LT.0.OR.SLTCO(1).GT.BRBOXG(ITYSL).OR.
     +       SLTCO(2).LT.0.OR.SLTCO(2).GT.BZBOXG(ITYSL)) THEN
                      IOK=-1
                      RETURN
          ENDIF
          IF(IEL(2).EQ.4.OR.IEL(2).EQ.21) THEN
            IF((SLTCO(1).GT.XTD).AND.
     +         (SLTCO(2).LT.YD.OR.SLTCO(2).GT.YTD)) IOK=0
          ENDIF
          IF(IOK.EQ.0) RETURN
          IF(IEL(2).EQ.5.OR.IEL(2).EQ.22) THEN
            IF((SLTCO(1).LT.XD).AND.
     +         (SLTCO(2).LT.YD.OR.SLTCO(2).GT.YTD)) IOK=0
          ENDIF
          IF(IOK.EQ.0) RETURN
          IF(IEL(2).EQ.10.OR.IEL(2).EQ.27) THEN
            IF(SLTCO(1).LT.XD.AND.SLTCO(2).LT.YD) IOK=0
          ENDIF
          IF(IOK.EQ.0) RETURN
          IF(IEL(2).EQ.12.OR.IEL(2).EQ.29) THEN
            IF(SLTCO(1).LT.XD.AND.SLTCO(2).GT.YTD) IOK=0
          ENDIF
          IF(IOK.EQ.0) RETURN
          IF(IEL(2).EQ.14.OR.IEL(2).EQ.31) THEN
            IF(SLTCO(1).GT.XTD.AND.SLTCO(2).LT.YD) IOK=0
          ENDIF
          IF(IOK.EQ.0) RETURN
          IF(IEL(2).EQ.16.OR.IEL(2).EQ.33) THEN
            IF(SLTCO(1).GT.XTD.AND.SLTCO(2).GT.YTD) IOK=0
          ENDIF
          GOTO 5
C
C End Cap - X strips
C
 2        CONTINUE
          ITYPE=MEPNTY(IEL(2))
C Add offsets
          IEL(5)=IEL(5)+MELOBU(IEL(3),IEL(9))
C Inversion for 2B,3B,1A,4A int/ext
       IF(IEL(2).EQ.1.OR.IEL(2).EQ.4.OR.IEL(2).EQ.5.OR.IEL(2).EQ.8.OR.
     +IEL(2).EQ.10.OR.IEL(2).EQ.11.OR.IEL(2).EQ.14.OR.IEL(2).EQ.15)THEN
              IXINV=1
              IEL(5)=MENXST(ITYPE)-IEL(5)
          ENDIF
C Decoding
          IF(ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.ITYPE.EQ.5.OR.ITYPE.EQ.6)THEN
             XOFF=EXSTOF(IEL(11),IEL(2))
          ELSE
             XOFF=EXSTOC(IEL(11),IEL(2))
          ENDIF
          N16T=IEL(5)/16
          NCH=IEL(5)-N16T*16
          IF(NCH.GE.(8+IXINV)) ICH=1
C
          SLTCO(1)=XOFF+FLOAT(N16T)*EPIT16(ITYPE)+SPA88T/2.+
     +        TUBCEL*FLOAT(NCH-IXINV)+TUBCEL/2.+SPA88T*FLOAT(ICH)
C Y strips
C Add offsets
          IEL(6)=IEL(6)+MELOBU(IEL(4),IEL(10))
C Inversion for 1B,2B,3A,4A int/ext
          IF(IEL(2).EQ.3.OR.IEL(2).EQ.4.OR.
     +      (IEL(2).GE.7.AND.IEL(2).LE.10).OR.
     +       IEL(2).EQ.13.OR.IEL(2).EQ.14) THEN
                IYINV=1
                IEL(6)=MENYST(ITYPE)-IEL(6)
          ENDIF
C Decoding
          IF(ITYPE.EQ.1.OR.ITYPE.EQ.4.OR.ITYPE.EQ.5.OR.ITYPE.EQ.8)THEN
                YOFFS=EYSTOF(IEL(11),IEL(2))
          ELSE
                YOFFS=EYSTOC(IEL(11),IEL(2))
          ENDIF
C
          SLTCO(2)=YOFFS+FLOAT(IEL(6)-IYINV)*YPITST+YPITST/2.
C
C Check: X & Y coord. must lie within box limits
C Here no test on inc. ass. is performed
C (IOK= 1  ok, IOK=-1 error)
          IOK=1
          IF(SLTCO(1).LT.0.OR.SLTCO(1).GT.EYYBOX(ITYPE).OR.
     +       SLTCO(2).LT.0.OR.SLTCO(2).GT.EXXBOX(ITYPE)) THEN
                      IOK=-1
                      RETURN
          ENDIF
C
          GOTO 5
C
C Middle angle
C
C X strips
C Add or remove offsets
 3        IF(MALOBU(IEL(3),IEL(9)).LE.24) THEN
               IEL(5)=IEL(5)-MALOBU(IEL(3),IEL(9))
          ELSE
               IEL(5)=IEL(5)+MALOBU(IEL(3),IEL(9))
          ENDIF
C Inversion when necessary
          IF(MADZFL(IEL(2)).LT.0) THEN
               IXINV=1
               IEL(5)=MANXST(MAPNTY(IEL(2)))-IEL(5)
          ENDIF
          N16T=IEL(5)/16
          NCH=IEL(5)-N16T*16
          IF(NCH.GE.(8+IXINV)) ICH=1
          XOFF=AXOFFS(IEL(11),IEL(2))
          SLTCO(1)=XOFF+FLOAT(N16T)*APIT16+SPA88T/2.+
     +        TUBCEL*FLOAT(NCH-IXINV)+TUBCEL/2.+SPA88T*FLOAT(ICH)
C Y strips
C Add or remove offsets
          IF(MALOBU(IEL(4),IEL(10)).LE.24) THEN
               IEL(6)=IEL(6)-MALOBU(IEL(4),IEL(10))
          ELSE
               IEL(6)=IEL(6)+MALOBU(IEL(4),IEL(10))-
     +                       MALOBU(IEL(4),IEL(10)-1)
          ENDIF
C Inversion for A side
          IF(IEL(2).GE.1.AND.IEL(2).LE.19) THEN
               IYINV=1
               IEL(6)=MANYST(IEL(2))-IEL(6)
          ENDIF
          SLTCO(2)=AYLEOF(IEL(11),IEL(2))+FLOAT(IEL(6)-IYINV)*YPITST
     +            +YPITST/2.
C Check on limits
          IF(SLTCO(1).LT.0.OR.SLTCO(1).GT.AZZBOX(MAPNTY(IEL(2))).OR.
     +       SLTCO(2).LT.0.OR.SLTCO(2).GT.ARRBOX(MAPNTY(IEL(2))))THEN
               IOK=-1
               RETURN
          ENDIF
C Cluster size & baricenters (general treatment)
 5        SLTCO(3)=FLOAT(IEL(7)-1)/2.*XPITST
          SLTCO(4)=FLOAT(IEL(8)-1)/2.*YPITST
          IF(IXINV.EQ.1) THEN
              SLTCO(1)=SLTCO(1)+SLTCO(3)
          ELSE
              SLTCO(1)=SLTCO(1)-SLTCO(3)
          ENDIF
          IF(IYINV.EQ.1) THEN
              SLTCO(2)=SLTCO(2)+SLTCO(4)
          ELSE
              SLTCO(2)=SLTCO(2)-SLTCO(4)
          ENDIF
C
          RETURN
          END
#endif
