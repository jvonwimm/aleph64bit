          SUBROUTINE MUSLCO(IEL,SLTCO,IOK)
C-------------------------------------------------------------
C! Subroutine transforming from raw data to slot coordinate system
C!
C! P.Campana    890608
C! P.Campana    910530 Corr. for 2 layer
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
#include "rcurnt.h"
          INTEGER IEL(*)
          REAL SLTCO(*)
C Choose subcomponent
          IXINV=0
          IYINV=0
          ICH=0
C ======================
C Set to positive flag
          IOK=1
C ======================
          IF(IEL(1)-2) 1,2,3
C
C Barrel - X strips
C
 1        CONTINUE
          IF(IEL(2).GE.18) GOTO 71
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 1st layer
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
C Takes care of error in bus# 18 slot # 8 (0-63 <> 96-159)
          IF(IEL(2).EQ.8.AND.IEL(10).EQ.18) THEN
             IXS=IEL(6)
             IF(IXS.LE.63) IEL(6)=96+IXS
             IF(IXS.GE.96.AND.IXS.LE.159) IEL(6)=IXS-96
          ENDIF
C Trattamento inversione Y1.2 Y1.3 slot 17
          INEW=0
          IF(IEL(2).EQ.17.AND.IEL(10).EQ.15) INEW=14
          IF(IEL(2).EQ.17.AND.IEL(10).EQ.14) INEW=15
          IF(INEW.GT.0) IEL(10)=INEW
C Identify "criminal" bus (nel caso della cam. 10 il bus criminale
C non e' il 2, bensi' il 4
          ICRI=0
          ITYBU=MBTYBU(IEL(4),IEL(10))
          IF((ITYBU.EQ.2.OR.ITYBU.EQ.6).AND.IEL(4).NE.2) ICRI=1
          IF(IEL(2).EQ.13.AND.ITYBU.EQ.2) ICRI=0
          IF(IEL(2).EQ.13.AND.ITYBU.EQ.4) ICRI=1
C Nella camera 10 gli offsets devono tenere conto dell'inversione
C del senso di lettura (andandoseli a prendere in locazioni
C diverse del DB rispetto a quelle attese )
          INDEX=IEL(10)
          IF(IEL(2).EQ.13.AND.ITYBU.LE.4) INDEX=31-IEL(10)
C Remove offset
          IF(MBLOBU(IEL(4),INDEX).LE.10) THEN
               ICRI=0
C                      ! No additional ten strips for first group
               IEL(6)=IEL(6)-MBLOBU(IEL(4),INDEX)+1
          ELSE
C Adds intermediate offsets
               IEL(6)=IEL(6)+MBLOBU(IEL(4),INDEX)-ICRI*10+1
          ENDIF
C Finds physical strip
C Inversion for busse #1 (all strips must start from side B) or for
C busse #2 for int. slots 9B 10 11A 11B (12-13-14-16) (Group IGRINV=1)
C (if inv. occurs nch starts from str. #0, otherw. from #1)
          I2=IEL(2)
          IGRINV=0
C          IF(I2.EQ.12.OR.I2.EQ.13.OR.I2.EQ.14.OR.I2.EQ.16) IGRINV=1
C Correzione 090590 PLC : la 9B non e' piu' del gruppo IGRINV=1
          IF(I2.EQ.13.OR.I2.EQ.14.OR.I2.EQ.16) IGRINV=1
          IF(IGRINV.EQ.1.AND.IEL(12).EQ.2) IYINV=1
          IF(IGRINV.EQ.0.AND.IEL(12).EQ.1) IYINV=1
          IF(IYINV.EQ.1) IEL(6)=MBNSY1(ITYSL)-IEL(6)
C
C                  A.A. 2-oct-1990 correction for Y offset slot 4, 11 ba
          CROFF=0.
          IF(IEL(2).EQ.4.AND.IEL(12).EQ.1)CROFF=1.1
          IF(IEL(2).EQ.11.AND.IEL(12).EQ.1)CROFF=1.9
          SLTCO(2)=BYOFFS(IEL(12),IEL(2))+FLOAT(IEL(6)-1+IYINV)*YPITST+
     +             YPITST/2.+CROFF
C ^^^^^^^^^^^^^^^^^^^^^^^^ end of layer 1
          GOTO 81
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 2nd layer
 71       CONTINUE
C X traitment
C Remove offsets
          IEL(5)=IEL(5)-MBLOBU(IEL(3),IEL(9))
C All bus readout must be inverted, except 9B,11B
C (taking into account layers with different strip number)
          ITYSL=MBPNT2(IEL(2))
          IF(IEL(2).NE.29.AND.IEL(2).NE.33) THEN
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
C Identify bus with internal offset ("criminal")
          ICRI=0
          ITYBU=MBTYBU(IEL(4),IEL(10))
          IF(ITYBU.EQ.2.OR.ITYBU.EQ.6) ICRI=1
C
C Remove offsets
          INDEX=IEL(10)
C Special case of bus Y3 of slot 10 ext.
C MB set to 22 (10/9/91)
          MB=MBLOBU(IEL(4),INDEX)
          IF(IEL(2).EQ.30.AND.ITYBU.EQ.6.AND.INDEX.EQ.19) MB=22
          IF(IEL(2).EQ.30.AND.ITYBU.EQ.6.AND.INDEX.EQ.20) MB=224
          IF(IEL(2).EQ.30.AND.ITYBU.EQ.6.AND.INDEX.EQ.21) MB=384

C -------------------- DB
C
          IB=8
          IF(IEL(2).EQ.30) IB=22
          IF(MB.LE.22) THEN
C No additional ten strips for first group
               IEL(6)=IEL(6)-MB+1
          ELSE
C Adds intermediate offsets
               IEL(6)=IEL(6)+MB-ICRI*IB+1
          ENDIF
C Finds physical strip :
C All bus #1 must be inverted (all strips must start from side B);
C except for:
C ch. 9B 11B which have Y3 readout as Y4 ( not inverted )
C ch. 9A 11A   "     "   "    "     "  " ( but inverted )
C (if inv. occurs nch starts from str. #0, otherw. from #1)
          I2=IEL(2)
          IYINV=0
          IF(IEL(12).EQ.1) IYINV=1
          IF(I2.EQ.27.OR.I2.EQ.31) IYINV=1
          IF(I2.EQ.29.OR.I2.EQ.33) IYINV=0
          IF(IYINV.EQ.1) IEL(6)=MBNSY1(ITYSL)-IEL(6)
C
          SLTCO(2)=BYOFFS(IEL(12),IEL(2))+FLOAT(IEL(6)-1+IYINV)*YPITST+
     +             YPITST/2.
C ^^^^^^^^^^^^^^^^^^^^^^^^ end of layer 2
 81       CONTINUE
C Check: X & Y coord. must lie within fiduc. volume (IOK= 1  ok,
C IOK=0 assoc. incomp.,IOK=-1 error)
          XD=BRTOOT(ITYSL)
          YD=BZTOOT(ITYSL)
          XTD=BRBOXG(ITYSL)-XD
          YTD=BZBOXG(ITYSL)-YD
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
          IF(IEL(2).GE.5.AND.IEL(2).LE.8.OR.
     +       IEL(2).GE.13.AND.IEL(2).LE.16) GOTO 72
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 1st layer
          ITYPE=MEPNTY(IEL(2))
C Add offsets (takes into account error in cabling 4A,i.e. slot 4)
          MOFFS=MELOBU(IEL(3),IEL(9))
          IF(IEL(2).EQ.4.AND.(IEL(9).EQ.2.OR.IEL(9).EQ.4)) MOFFS=64
          IEL(5)=IEL(5)+MOFFS
C Inversion for 2B,3B,1A,4A int/ext
       IF(IEL(2).EQ.1.OR.IEL(2).EQ.4.OR.IEL(2).EQ.5.OR.IEL(2).EQ.8.OR.
     +IEL(2).EQ.10.OR.IEL(2).EQ.11.OR.IEL(2).EQ.14.OR.IEL(2).EQ.15)THEN
              IXINV=1
C Eigth strips less for 1A,2B int.
              IF(IEL(2).EQ.1.OR.IEL(2).EQ.10) IEL(5)=IEL(5)-8
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
C Sixteen strips less for 2A,4A int.
          IF(IEL(2).EQ.2.OR.IEL(2).EQ.4) IEL(6)=IEL(6)-16
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
C ^^^^^^^^^^^^^^^^^^^^^^^^ end of layer 1
          GOTO 82
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 2nd layer
 72       CONTINUE
          ITYPE=MEPNTY(IEL(2))
C Add offsets (only if it is not the first group)
          MOFFS=MELOBU(IEL(3),IEL(9))
          IF(MOFFS.GT.16)  IEL(5)=IEL(5)+MOFFS
C 2A,3A,1B,4B ext. to be inverted
              IXINV=0
          IF(IEL(2).EQ.6.OR.IEL(2).EQ.7.OR.IEL(2).EQ.13.OR.
     +       IEL(2).EQ.16) THEN
              IXINV=1
C Sixteen strips less for 4B ext.
              IF(IEL(2).EQ.16) IEL(5)=IEL(5)-16
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
          MOFFS=MELOBU(IEL(4),IEL(10))
          IF(MOFFS.GT.16)  IEL(6)=IEL(6)+MOFFS
C Sixteen strips less for 4B ext.
          IF(IEL(2).EQ.16) IEL(6)=IEL(6)-16
C Inversion for 1A,2A,3B,4B ext.
          IF(IEL(2).EQ. 5.OR.IEL(2).EQ.6.OR.
     +       IEL(2).EQ.15.OR.IEL(2).EQ.16) THEN
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
C ^^^^^^^^^^^^^^^^^^^^^^^^ end of layer 2
 82       CONTINUE
C
C Check: X & Y coord. must lie within box limits
C Here no test on inc. ass. is performed
C (IOK= 1  ok, IOK=-1 error)
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
 3        CONTINUE
          IF(IEL(2).GE.11.AND.IEL(2).LE.19.OR.
     +       IEL(2).GE.30.AND.IEL(2).LE.38) GOTO 73
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 1st layer
          IF(MALOBU(IEL(3),IEL(9)).LE.24) THEN
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
C chambers 6a,2b,4b  (A.Antonelli 7-aug-90)
          IF(IEL(2).EQ.6.OR.IEL(2).EQ.21.OR.IEL(2).EQ.23) THEN
               IF(IEL(10).EQ.15.OR.IEL(10).EQ.17)IEL(6)=IEL(6)-
     +                                  MALOBU(IEL(4),IEL(10))
               IF(IEL(10).EQ.16.OR.IEL(10).EQ.18)IEL(6)=IEL(6)+
     +            MALOBU(IEL(4),IEL(10))-MALOBU(IEL(4),IEL(10)-1)
          ELSE
C altre camere
              IF(MALOBU(IEL(4),IEL(10)).LE.24) THEN
                  IEL(6)=IEL(6)-MALOBU(IEL(4),IEL(10))
              ELSE
                  IEL(6)=IEL(6)+MALOBU(IEL(4),IEL(10))-
     +                          MALOBU(IEL(4),IEL(10)-1)
              ENDIF
          ENDIF
C Inversion for A side
          IF(IEL(2).GE.1.AND.IEL(2).LE.19) THEN
               IYINV=1
               IEL(6)=MANYST(IEL(2))-IEL(6)
          ENDIF
          SLTCO(2)=AYLEOF(IEL(12),IEL(2))+FLOAT(IEL(6)-IYINV)*YPITST
     +            +YPITST/2.
C ^^^^^^^^^^^^^^^^^^^^^^^^ end of layer 1
          GOTO 83
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 2nd layer
 73       CONTINUE
          IF(MALOBU(IEL(3),IEL(9)).LE.24) THEN
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
C chambers 6a,2b,4b  (A.Antonelli 7-aug-90)
          IF(IEL(2).EQ.6.OR.IEL(2).EQ.21.OR.IEL(2).EQ.23) THEN
               IF(IEL(10).EQ.15.OR.IEL(10).EQ.17)IEL(6)=IEL(6)-
     +                                  MALOBU(IEL(4),IEL(10))
               IF(IEL(10).EQ.16.OR.IEL(10).EQ.18)IEL(6)=IEL(6)+
     +            MALOBU(IEL(4),IEL(10))-MALOBU(IEL(4),IEL(10)-1)
          ELSE
C altre camere
              IF(MALOBU(IEL(4),IEL(10)).LE.24) THEN
                  IEL(6)=IEL(6)-MALOBU(IEL(4),IEL(10))
              ELSE
                  IEL(6)=IEL(6)+MALOBU(IEL(4),IEL(10))-
     +                          MALOBU(IEL(4),IEL(10)-1)
              ENDIF
          ENDIF
C Inversion for A side
          IF(IEL(2).GE.1.AND.IEL(2).LE.19) THEN
               IYINV=1
               IEL(6)=MANYST(IEL(2))-IEL(6)
          ENDIF
          SLTCO(2)=AYLEOF(IEL(12),IEL(2))+FLOAT(IEL(6)-IYINV)*YPITST
     +            +YPITST/2.
C ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ end of layer 2
 83       CONTINUE
C Check on limits
          IF(SLTCO(1).LT.0.OR.SLTCO(1).GT.AZZBOX(MAPNTY(IEL(2))).OR.
     +       SLTCO(2).LT.0.OR.SLTCO(2).GT.ARRBOX(MAPNTY(IEL(2))))THEN
               IOK=-1
               RETURN
          ENDIF
C
C Cluster size & baricenters (general treatment)
C =====================================
 5        SLTCO(4)=FLOAT(IEL(8)-1)/2.*YPITST
 6        SLTCO(3)=FLOAT(IEL(7)-1)/2.*XPITST
C =====================================
C
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
C Here we take into account that barrel ext. 9A 9B 11A 11B
C must be rotated
          IF(IEL(1).EQ.1) THEN
            IF(IEL(2).EQ.27.OR.IEL(2).EQ.29.OR.IEL(2).EQ.31.
     +      OR.IEL(2).EQ.33) THEN
               A1=SLTCO(1)
               A3=SLTCO(3)
               SLTCO(1)=SLTCO(2)
               SLTCO(3)=SLTCO(4)
               SLTCO(2)=A1
               SLTCO(4)=A3
            ENDIF
          ENDIF
C
          RETURN
          END
#endif
