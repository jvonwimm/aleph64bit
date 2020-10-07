*DK DV0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DV0
CH
      SUBROUTINE DV0(NL,NUM1,NUM2,FOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FOUT
      FOUT=.TRUE.
      NUM2=0
      IF(BNUMDB(4,NL).LE.0..OR.BNUMDB(2,NL).EQ.0.) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HSE,J_HSH'
      CALL DPARAM(20
     &  ,J_HSE,J_HSH)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FPIMDP) THEN
C         IF(NL.EQ.VDXYDB.OR.NL.EQ.VDZTDB) THEN
C           IF(MDLPDP.NE.VDXYDB.AND.MDLPDP.NE.VDZTDB) RETURN
C           NUM1=1
C           NUM2=BNUMDB(2,NL)
         IF(NL.EQ.FRTLDB) THEN
           NUM1=1
           NUM2=BNUMDB(2,NL)
         ELSE
           IF(NL.NE.MDLPDP) RETURN
           NUM=BNUMDB(2,NL)
           IF(NPIKDP.GT.NUM) RETURN
           NUM1=NPIKDP
           NUM2=NPIKDP
         END IF
      ELSE
         NUM1=1
         NUM2=BNUMDB(2,NL)
         IF(NUM2.LE.0) RETURN
         IF(PARADA(4,J_HSH).EQ.1.) THEN
            NUM1=PARADA(2,J_HSH)
            IF(NUM1.GT.NUM2.OR.NUM1.LT.1) RETURN
            NUM2=NUM1
         END IF
      END IF
      MDLRDP=NL
C     .... WHY WAS MPLPDP = NL INSERTED ? ONE CANNOT PICK MORE THAN ONE BANK!
C      MDLPDP=NL
      IF     (NL.EQ.ITCODB) THEN
         IF(.NOT.FITCDR) RETURN
         CALL=DVIT0(NN)
         CALL DCTYCD
      ELSE IF(NL.EQ.TPADDB) THEN
         IF(.NOT.FTPCDR) RETURN
         ISEC=PARADA(2,J_HSE)
         CALL=DVPAD0(ISEC,NN)
         CALL DCTYPA
      ELSE IF(NL.EQ.TBCODB) THEN
         IF(.NOT.FTPCDR) RETURN
         CALL=DVTB0(NN)
         CALL DCTYBA
      ELSE IF(NL.EQ.TPCODB) THEN
         IF(.NOT.FTPCDR) RETURN
C         CALL DCUTTR
         CALL=DVTQ0(NN)
         CALL DCTYCD
      ELSE IF(NL.EQ.ESDADB) THEN
         IF(.NOT.FECADR) RETURN
         CALL=DVEC0(NN)
         CALL DCTYCA
      ELSE IF(NL.EQ.PECODB) THEN
         IF(.NOT.FECADR) RETURN
         CALL=DVEO0(NN)
      ELSE IF(NL.EQ.PEWDDB) THEN
         IF(.NOT.FECADR) RETURN
         CALL DVEW0(NN)
      ELSE IF(NL.EQ.PLSDDB) THEN
C         IF(.NOT.FLCADR) RETURN
         CALL=DVLC0(NN)
         CALL DCTYCA
      ELSE IF(NL.EQ.HSDADB) THEN
         IF(.NOT.FHCADR) RETURN
         CALL=DVHC0(NN)
         CALL DCTYCA
      ELSE IF(NL.EQ.HTUBDB) THEN
         IF(.NOT.FHCADR) RETURN
         CALL=DVHT0(NN)
      ELSE IF(NL.EQ.MHITDB) THEN
         IF(.NOT.FMDEDR) RETURN
         IF(FMHTDN) THEN
           CALL DVMD2D
           FMHTDN=.FALSE.
         END IF
         CALL=DVMD0(NN)
      ELSE IF(NL.EQ.VDZTDB) THEN
         IF((.NOT.FVDEDR).OR.(.NOT.FEVDDV)) RETURN
         CALL=DVVDR0(NN)
      ELSE IF(NL.EQ.VDXYDB) THEN
         IF((.NOT.FVDEDR).OR.(.NOT.FEVDDV)) RETURN
         CALL=DVVDX0(NN)
      ELSE IF(NL.EQ.VDCODB) THEN
         IF((.NOT.FVDEDR).OR.(.NOT.FEVDDV)) RETURN
         CALL=DVVDC0(NN)
      ELSE IF(NL.EQ.FRFTDB) THEN
C         IF((.NOT.FITCDR).AND.(.NOT.FTPCDR)) RETURN
C         CALL DCUTTR
         CALL DVTR0(NN)
         CALL DCTYTR
      ELSE IF(NL.EQ.FRTLDB) THEN
         IF((.NOT.FITCDR).AND.(.NOT.FTPCDR)) RETURN
C         CALL DCUTTR
         CALL=DVCHT0(NN)
         CALL=DVCHI0(NN)
         CALL DCTYCD
      END IF
      FOUT=.FALSE.
      END
*DK DV_PICK
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DV_PICK
CH
      SUBROUTINE DV_PICK(MODUL,NHIT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 DT1
      CHARACTER *2 TVDQ(3)
      DATA TVDQ/'XY','ZT','3D'/
      CHARACTER *3 DT3
      CHARACTER *4 DT4,TB
      CHARACTER *5 DT5
      CHARACTER *50 T,TVDC,TVDH
C                123456789 123456789 123456789 123456789 123456789
      DATA TVDH/'VD??: WI=12345 ROW=123 LAY=1 MO=1 FACE=123 TR=123'/
      DATA TVDC/'VDCO: Row=123 QF=1 (XY) Layer=1 TR=123 WI=12345'/
      TR=-99999.
      CL=-99999.
      IF(MODUL.EQ.2000) THEN
        CALL DYXHHP(NHIT)
        GO TO 8
      END IF
      IF(MODUL.EQ.2001.OR.MODUL.EQ.2002) THEN
        CALL DHZPHP(NHIT)
        GO TO 8
      END IF
      IF(MODUL.EQ.2003.OR.MODUL.EQ.2004) THEN
        CALL DRZHEP(NHIT)
        GO TO 8
      END IF
      IF(MODUL.EQ.2005.OR.MODUL.EQ.2006) THEN
        CALL DRZHHP(NHIT)
        GO TO 8
      END IF
      IF(MODUL.EQ.2010) THEN
        CALL DYXHEP(NHIT)
        GO TO 8
      END IF
      IF(MODUL.GE.2011.AND.MODUL.LE.2023) THEN
        CALL DHZPEP(NHIT)
        GO TO 8
      END IF
      IF(MODUL.GT.1000.AND.MODUL.LT.2000) THEN
        IF(NHIT.GT.31) GO TO 9
        CALL DRSPTY
        GO TO 8
      END IF
      IF(MODUL.EQ.2500) THEN
        CALL DTDPIK(MODUL,NHIT)
        TR=NHIT
        GO TO 8
      END IF
      IF(MODUL.EQ.7000.OR.MODUL.EQ.7001) THEN
        CALL DAC_PICK(NHIT,MODUL)
        GO TO 8
      END IF
      IF(MODUL.EQ.PEWDDB) THEN
        CALL DYXWE_PICK(NHIT)
        GO TO 8
      END IF
      IF(MODUL.GT.NUMBDB) GO TO 9
      IF(BNUMDB(4,MODUL).LE.0) GO TO 9
      IF(NHIT.LE.0) GO TO 9
      IF(MODUL.NE.VDXYDB.AND.NHIT.GT.IFIX(BNUMDB(2,MODUL))) GO TO 9
      IF     (MODUL.EQ.ITCODB) THEN
         CALL=DVIT0(NN)
         TR=DVIT(IVNTDV,NHIT,1)
         TB='itco'
      ELSE IF(MODUL.EQ.TPCODB) THEN
         CALL=DVTQ0(NN)
         CALL=DVTPNT(NHIT,KTR)
         TR=KTR
         TB='tpco'
      ELSE IF(MODUL.EQ.ESDADB) THEN
         CALL=DVEC0(NN)
         EN=DVEC(IVENDV,NHIT)
         CL=DVEC(IVU1DV,NHIT)
C        TR=DVEC(IVNTDV,NHIT)
         JJ=DVEC(IVJJDV,NHIT)
         II=DVEC(IVIIDV,NHIT)
         NHEC=NHIT
         TB='esda'
      ELSE IF(MODUL.EQ.PECODB) THEN
         CALL=DVEO0(NN)
         EN=DVEO(IVENDV,NHIT)
         CL=NHIT
C        TR=DVEC(IVNTDV,NHIT)
         NHEC=NHIT
         TB='peco'
      ELSE IF(MODUL.EQ.TPADDB) THEN
         TB='tpad'
      ELSE IF(MODUL.EQ.TBCODB) THEN
         TB='tbco'
      ELSE IF(MODUL.EQ.VDXYDB) THEN
C
COLD     ..... DATA TVDH/'VD??: Hit=123 TR=123 WI=12345 LAY=1 MO=1 FACE=123'/
C        ............     123456789 123456789 123456789 123456789 123456789
C        ..... DATA TVDH/'VD??: WI=12345 ROW=123 LAY=1 MO=1 FACE=123 TR=123'/
C
         TR=  DVVDXY(18,NHIT)
         WAFR=DVVDXY(14,NHIT)
         FACE=DVVDXY(16,NHIT)
         IF(WAFR.LE.2.) FACE=-FACE
         TVDH( 3: 4)='XY'
         TVDH(10:14)=DT5(DVVDXY(19,NHIT))
         TVDH(20:22)=DT3(DVVDXY(20,NHIT))
         TVDH(28:28)=DT1(DVVDXY(15,NHIT))
         TVDH(33:33)=DT1(WAFR)
         TVDH(40:42)=DT3(FACE)
         TVDH(47:49)=DT3(TR)
         CALL DWRT(TVDH)
         GO TO 8
      ELSE IF(MODUL.EQ.VDZTDB) THEN
         TR=ABS(DVVDRZ(18,NHIT,1))
         WAFR=  DVVDRZ(14,NHIT,1)
         FACE=  DVVDRZ(16,NHIT,1)
         IF(WAFR.LE.2.) FACE=-FACE
         TVDH( 3: 4)='ZT'
         TVDH(10:14)=DT5(DVVDRZ(19,NHIT,1))
         TVDH(20:22)=DT3(DVVDRZ(20,NHIT,1))
         TVDH(28:28)=DT1(DVVDRZ(15,NHIT,1))
         TVDH(33:33)=DT1(WAFR)
         TVDH(40:42)=DT3(FACE)
         TVDH(47:49)=DT3(TR)
         CALL DWRT(TVDH)
         GO TO 8
      ELSE IF(MODUL.EQ.VDCODB) THEN
        VDQ=DVVDC(14,NHIT)
        TR=DVVDC(IVNTDV,NHIT)
        TVDC(11:13)=DT3(FLOAT(NHIT))
        TVDC(18:18)=DT1(VDQ)
        TVDC(21:22)=TVDQ(IFIX(VDQ))
        TVDC(31:31)=DT1(DVVDC(13,NHIT))
        TVDC(36:38)=DT3(TR)
        TVDC(43:47)=DT5(DVVDC(12,NHIT))
        CALL DWRT(TVDC)
        GO TO 8
      ELSE IF(MODUL.EQ.HSDADB) THEN
         CALL=DVHC0(NN)
         EN=DVHC(IVENDV,NHIT)
         CL=DVHC(IVU1DV,NHIT)
C        TR=DVHC(IVNTDV,NHIT)
         TB='hsda'
      ELSE IF(MODUL.EQ.PLSDDB) THEN
         CALL=DVLC0(NN)
C                    1         2         3         4         5
C           12345678901234567890123456789012345678901234567890
         T='LCAL hit 123(A) en=1.23(1)+1.23(2)+1.23(3)=1.23'
         T(10:12)=DT4(FLOAT(NHIT))
         IF(DVLC(IVZZDV,NHIT).GT.0.) T(14:14)='B'
         EN=0.
         L=20
         DO 700 I=NHIT,NHIT+2
           E=DVLC(IVENDV,I)
           EN=EN+E
           T(L:L+3)=DT4(E)
           L=L+8
  700    CONTINUE
         T(44:47)=DT4(EN)
         CALL DWRT(T)
         GO TO 8
      ELSE IF(MODUL.EQ.HTUBDB) THEN
         CALL=DVHT0(NN)
C        TR=DVHT(IVNTDV,NHIT)
         TB='htub'
      ELSE IF(MODUL.EQ.MHITDB) THEN
         CALL=DVMD0(NN)
C        TR=DVMD(IVNTDV,NHIT)
         TB='mhit'
      ELSE IF(MODUL.EQ.FRFTDB) THEN
         TR=NHIT
         TB='frft'
      ELSE IF(MODUL.EQ.FRTLDB) THEN
         TR=NHIT
         TB='frtl'
      ELSE IF(MODUL.EQ.FKINDB) THEN
         CALL DMCTYP(NHIT)
         GO TO 8
      END IF
C                 1         2         3         4         5
C        12345678901234567890123456789012345678901234567890
      T=' bank ROw 1234  en=1.23 cl=123  track=123 p=-12.3 '
      T( 2: 5)=TB
      T(11:14)=DT4(FLOAT(NHIT))
      IF(CL.EQ.-99999.) THEN
         T(17:30)=' '
      ELSE
         T(20:23)=DT4(EN)
         T(28:30)=DT3(CL)
      END IF
      IF(TR.EQ.-99999.) THEN
         T(33:49)=' '
      ELSE
         T(39:41)=DT3(TR)
         IF(TR.EQ.0..OR.BNUMDB(2,FRFTDB).EQ.0.
     &     OR.BNUMDB(4,FRFTDB).LE.0.) THEN
            T(43:49)=' '
         ELSE
           CALL DPARGV(20,'HTB',2,FTB)
           T(17:30)='nu = TB = '//DT1(FTB)
           CALL DVTR0(NN)
           CALL DVTRTP(ABS(IFIX(TR)),PTOT)
           CALL DTP(PTOT,5,T(45:49))
         END IF
      END IF
      CALL DWRT(T)
      IF(MODUL.EQ.ESDADB) THEN
         ES=0.
         NS=1
C        ............. DO NHEC=MIN(1,NHIT-2),MAX(NN,NHIT+2)  ! changed 20.9.96 
C        ..... right : DO NHEC=MAX(1,NHIT-2),MIN(NN,NHIT+2) as the old code was
C        ..... fast enough, we loop over all hits:
         DO NHEC=1,NN
           EN=DVEC(IVENDV,NHEC)
           IF(EN.GE.ECMIDU.AND.
     &       DVEC(IVJJDV,NHEC).EQ.JJ.AND.
     &       DVEC(IVIIDV,NHEC).EQ.II) THEN
             CL=DVEC(IVU1DV,NHEC)
             GK=DVEC(IVKKDV,NHEC)
C               123456789 123456789 123456789 123456789 123456789
             T=' same tower: ROw 1234 en=+******=****** cl=123 K=1'
             T(18:21)=DT4(FLOAT(NHEC))
             WRITE(T(27:32),'(F6.3)',ERR=11) EN
   11        ES=ES+EN
             IF(NS.EQ.1) THEN
               T(26:26)=' '
               T(33:39)=' '
             ELSE
               WRITE(T(34:39),'(F6.3)',ERR=12) ES
             END IF
   12        T(44:46)=DT3(CL)
             T(50:50)=DT1(GK)
             CALL DWRT(T)
             NS=NS+1
           END IF
         END DO
         CL=DVEC(IVU1DV,NHIT)
         IF(CL.NE.0.) THEN
           NESD=BNUMDB(2,ESDADB)
           ES=0.
           DO K=1,NESD
             IF(CL.EQ.DVEC(IVU1DV,K)) THEN
               EN=DVEC(IVENDV,K)
               IF(EN.GE.ECMIDU) ES=ES+EN
             END IF
           END DO
C             123456789 123456789 123456789 123456789 123456789
C          T=' same tower: ROw 1234 en=+******=****** cl=123 K=1'
           T=' sum of all stories : en>0  : en=12.456 cl=123'
           WRITE(T(34:39),'(F6.3)',ERR=812) ES
  812      T(44:46)=DT3(CL)
           CALL DWRT(T)
         END IF
      END IF
    8 IF(TR.GT.0.) THEN
        DO K=7,1,-1
          LTRKDP(K+1)=LTRKDP(K)
        END DO
        LTRKDP(1)=NTRKDP
        NTRKDP=TR
        NUMPDP=MIN(9,NUMPDP+1)
      END IF
      IF(MDLPDP.EQ.TPCODB.OR.MDLPDP.EQ.TPADDB) THEN
        CALL UCOPY(EVARDP(1,2),EVARDP(1,3),20)
        CALL UCOPY(EVARDP(1,1),EVARDP(1,2),20)
        IF(MDLPDP.EQ.TPCODB) THEN
          DO K=1,20
            EVARDP(K,1)=DVTP(K,NPIKDP)
          END DO
        ELSE
          DO K=1,20
            EVARDP(K,1)=DVPADA(K,NPIKDP)
          END DO
        END IF
        DO K=MXYZDP,2,-1
          XYZPDP(1,K)=XYZPDP(1,K-1)
          XYZPDP(2,K)=XYZPDP(2,K-1)
          XYZPDP(3,K)=XYZPDP(3,K-1)
        END DO
        XYZPDP(1,1)=EVARDP(1,1)
        XYZPDP(2,1)=EVARDP(2,1)
        XYZPDP(3,1)=EVARDP(3,1)
        NXYZDP=NXYZDP+1
      END IF
      IF(LORPDP.EQ.2) THEN
        BNUMDB(4,FRFTDB)=BNUMDB(1,FRFTDB)
        BNUMDB(1,FRFTDB)=1.
      END IF
      LORPDP=0
      IF(MODUL.EQ.VDXYDB) NPXYDV=NPIKDP
      IF(MODUL.EQ.VDZTDB) NPZTDV=NPIKDP
      RETURN
    9 CALL DWRT('Bank not available or row out of range.')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------------- DV_PICK_0
CH
      ENTRY DV_PICK_0
CH
CH --------------------------------------------------------------------
CH
      NUMPDP=0
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DV0TYP
CH
      SUBROUTINE DV0_PICK_TEXT(MODUL,NHIT,TV,LT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Fill nhit into TXTADW
C     TV is not used here, but in ATLANTIS to draw other variables

      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TV

      TXTADW=' '
      CALL DTINT(NHIT,1,5,TXTADW)
      LT=5
      END

*DK IDVCHT
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  IDVCHT
CH
      FUNCTION IDVCHT(K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     equivalent to IDTP, but uses FRFT, FRTL, etc.  -ecb
*CA BCS
      INCLUDE 'DALI_CF.INC'
*CA BMACRO
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      IDVCHT=IW(ITRA+K)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVCHT0
CH
      ENTRY DVCHT0(NTRAK)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVCHT0=0.
      NTRAK=0
      IF(IW(30).NE.12345) RETURN
      IFRTL=IW(NAMIND('FRTL'))
      IFTCL=IW(NAMIND('FTCL'))
      IF(IFRTL.LE.0.OR.IFTCL.LE.0) THEN
         NTRAK=0
      ELSE
         NTRAK=LROWS(IFRTL)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVCHT
CH
      ENTRY DVCHT(ITRAK,NCO)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     CO comes from columns 2 and 3 in TGTL; 6 and 7 in FRTL -ecb
      NCO=ITABL(IFRTL,ITRAK,7)+ITABL(IFRTL,ITRAK,8)
C     fset in TGCL was in column 1 of TGTL; FTCL offset in position 5
C     FRTL  -ecb
      DVCHT=0.
      ITRA=IFTCL+2+ITABL(IFRTL,ITRAK,6)
      END
*DK IDVCHI
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  IDVCHI
CH
      FUNCTION IDVCHI(K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA BCS
      INCLUDE 'DALI_CF.INC'
*CA BMACRO
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      IDVCHI=IW(ITRA+K)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVCHI0
CH
      ENTRY DVCHI0(NTRAK)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVCHI0=0
      NTRAK=0
      IF(IW(30).NE.12345) RETURN
      IFRTL=IW(NAMIND('FRTL'))
      IFICL=IW(NAMIND('FICL'))
      IF(IFRTL.LE.0.OR.IFICL.LE.0) THEN
         NTRAK=0
      ELSE
         NTRAK=LROWS(IFRTL)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVCHI
CH
      ENTRY DVCHI(ITRAK,NCO)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     Determine number of ITC coordinates (NCO) for FRFT track ITRAK -ecb
C     ITC-spirals after or between TPC-spirals are not used (Garrido!)
      NCO=ITABL(IFRTL,ITRAK,4)
C     change itra to other variable -- used by idtp  -ecb
      DVCHI=0
      ITRA=IFICL+2+ITABL(IFRTL,ITRAK,3)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVCHV
CH
      ENTRY DVCHV(ITRAK,NCO)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C
C!:
C     Determine number of VD coordinates (NCO) for FRFT track ITRAK
      DVCHV=0
      NCO=ITABL(IFRTL,ITRAK,2)
      END
*DK DVCHCC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVCHCC
CH
      SUBROUTINE DVCHCC(NTRK,KTPC,KITC,NFI)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*DA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION NUMI(2),KI(2),NF(2),R2(2)
      DATA NUMI(1)/1/
      KTPC=0
      CALL=DVCHT(NTRK,NUM)
      IF(NUM.EQ.0) RETURN
      CALL=DVCHI(NTRK,NUMI(2))
      IF(NUMI(2).EQ.0) RETURN
      KTPC=IDVCHT(1)
      XT=DVTP(IVXXDV,KTPC)
      YT=DVTP(IVYYDV,KTPC)
      DO 700 K=1,2
         KI(K)=IDVCHI(NUMI(K))
         IF(KI(K).GT.0) THEN
            NF(K)=1
         ELSE
            NF(K)=2
            KI(K)=-KI(K)
         END IF
         XI=DVIT(IVXXDV,KI(K),NF(K))
         YI=DVIT(IVYYDV,KI(K),NF(K))
         R2(K)=(XI-XT)**2+(YI-YT)**2
  700 CONTINUE
      IF(R2(1).LE.R2(2)) THEN
         KITC=KI(1)
         NFI=NF(1)
      ELSE
         KITC=KI(2)
         NFI=NF(2)
      END IF
      END
*DK DVCHIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVCHIT
CH
      SUBROUTINE DVCHIT(NTRK,NCTP,NCIT,NTR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      NTRK=0
      NCIT=0
      NCTP=0
      IF(IW(30).NE.12345) RETURN
      IFRTL=IW(NAMIND('FRTL'))
      IFTCL=IW(NAMIND('FTCL'))
      IFICL=IW(NAMIND('FICL'))
      IF(IFRTL.NE.0) NTRK=LROWS(IFRTL)
      IF(IFICL.NE.0) NCIT=LROWS(IFICL)
      IF(IFTCL.NE.0) NCTP=LROWS(IFTCL)
      NTR=NTRK
      IF(NCIT.EQ.0.AND.NCTP.EQ.0) NTR=0
      END
*DK DVHT
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVHT
CH
      FUNCTION DVHT(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'J_HCNAMC.INC'
      INCLUDE 'A_HTUBJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      GO TO (1,99,99,99,99,99,99,99,99,10,11,12,13),NV
   99 DVHT=0.
      RETURN
C LOCAL COORDINATE OF CENTER OF CLUSTER
    1 DVHT=RTABL(IHTUB,K,JHTULC)
      RETURN
C CLUSTER WIDTH
   10 DVHT=RTABL(IHTUB,K,JHTUCW)
      RETURN
C LAYER NUMBER
   11 DVHT=ITABL(IHTUB,K,JHTULN)
      RETURN
C MODULE NUMBER
   12 DVHT=ITABL(IHTUB,K,JHTUMN)
      RETURN
C SUBCOMPONENT NUMBER
   13 DVHT=ITABL(IHTUB,K,JHTUSN)
      RETURN
C
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVHT0
CH
      ENTRY DVHT0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVHT0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IHTUB=IW(NAMIND('HTUB'))
      IF(IHTUB.EQ.0) RETURN
      NUM=LROWS(IHTUB)
      K1=0
      KRO=0
      RETURN
      END
*DK DVMD
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVMD
CH
      FUNCTION DVMD(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DATA PIDEG/57.29577951/
      INCLUDE 'A_MHITJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C     FOR DGLMUD: LOGICAL FSLOT(38,3)=FSLOT(DVMD(IVNNDV,K),DVMD(IVIIDV,K))
      GO TO(1,2,3,4,5,6,7,99,9,99,99,99,13,99,15,16,99,99,99,99),NV
C--------------------------------------------------------------  UNDEFIN
   99 DVMD=0.
      RETURN
C----------------------------------------------------------------------
    1 RO=RTABL(IMHIT,K,JMHIRH)*SIN(RTABL(IMHIT,K,JMHITH))
      DVMD=RO*COS(RTABL(IMHIT,K,JMHIPH))
      RETURN
C----------------------------------------------------------------------
    2 RO=RTABL(IMHIT,K,JMHIRH)*SIN(RTABL(IMHIT,K,JMHITH))
      DVMD=RO*SIN(RTABL(IMHIT,K,JMHIPH))
      RETURN
C----------------------------------------------------------------------
    3 DVMD=RTABL(IMHIT,K,JMHIRH)*COS(RTABL(IMHIT,K,JMHITH))
      RETURN
C----------------------------------------------------------------------
    4 DVMD=RTABL(IMHIT,K,JMHIRH)*SIN(RTABL(IMHIT,K,JMHITH))
      RETURN
C----------------------------------------------------------------------
    5 DVMD=RTABL(IMHIT,K,JMHIPH)*PIDEG
      RETURN
C----------------------------------------------------------------------
    6 DVMD=999999.
      IF((TCUT.EQ.0.).OR.((TCUT.EQ.1.).AND.(ITABL(IMHIT,K,JMHISC)
     &  .NE.1)).OR.((TCUT.EQ.2.).AND.(ITABL(IMHIT,K,JMHISC).EQ.1)))
     &  DVMD=RTABL(IMHIT,K,JMHITH)*PIDEG
      RETURN
C----------------------------------------------------------------------
    7 DVMD=RTABL(IMHIT,K,JMHIRH)
      RETURN
C----------------------------------------------------------------------
    9 DVMD=RTABL(IMHIT,K,JMHIYL)
      RETURN
C----------------------------------------------------------------------
   13 DVMD=ITABL(IMHIT,K,JMHISN)
      RETURN
C----------------------------------------------------------------------
   15 DVMD=ITABL(IMHIT,K,JMHILN)
      RETURN
C----------------------------------------------------------------------
   16 DVMD=ITABL(IMHIT,K,JMHISC)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVMD0
CH
      ENTRY DVMD0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVMD0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IMHIT = IW(NAMIND('MHIT'))
      IF(IMHIT.EQ.0) RETURN
      NUM=LROWS(IMHIT)
      KX=0
      KY=0
      KR=0
      KT=0
      KB=0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVMDCU
CH
      ENTRY DVMDCU(TC)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVMDCU=0
      TCUT=TC
      END
*DK DVMD2D
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVMD2D
CH
      SUBROUTINE DVMD2D
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 DT2
      DIMENSION GLOST(3)
      LOGICAL FLOST
      CHARACTER *51 T
      IF(BNUMDB(4,MHITDB).LE.0.) RETURN
      CALL=DVMD0(NUM)
      IF(NUM.LE.0) RETURN
      CALL VZERO(GLOST,3)
      FLOST=.FALSE.
      DO 300 K=1,NUM
        YLOCAL=DVMD(IVLVDV,K)
        IF(YLOCAL.EQ.0.) THEN
          NSUBC=DVMD(IVNNDV,K)
          GLOST(NSUBC)=GLOST(NSUBC)+1.
          FLOST=.TRUE.
        END IF
  300 CONTINUE
      IF (FLOST) THEN
        TOT = NUM
C          123456789 123456789 123456789 123456789 123456789 1
C       T='MHIT:total=12  unvisible:middle-angle=12  endcap=12'
        T='MHIT:total=12'
        T(12:13)=DT2(TOT)
        IF(GLOST(2).GT.0.) THEN
          T(16:25)='unvisible:'
          T(26:40)='middle-angle='//DT2(GLOST(2))
        END IF
        IF(GLOST(1).GT.0.) THEN
          T(16:25)='unvisible:'
          T(43:51)='endcap='//DT2(GLOST(1))
        END IF
        CALL DWRT(T)
        IF(GLOST(3).GT.0.) THEN
          T='         barrel: visible only in Y/X and Rho/phi='//
     &      DT2(GLOST(3))
          CALL DWRT(T)
        END IF
      END IF
      END
*DK DVM0
      LOGICAL FUNCTION DVM0(KTRA)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DVM0=.TRUE.
      IF(FPIMDP) THEN
         IF(MDLPDP.NE.FKINDB.OR.KTRA.NE.NPIKDP) RETURN
      END IF
      MDLRDP=FKINDB
      DVM0=.FALSE.
      END
*DK DVEC
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVEC
CH
      FUNCTION DVEC(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'J_ECNAMC.INC'
      INCLUDE 'A_ESDAJJ.INC'
      DATA IDEB/0/
      DATA A30/0.5236/,A180/3.141598/,A360/6.283196/
      DATA II2/2/,II4/4/,II6/6/,II8/8/
      DIMENSION ZB(51:114,3),ZE(3),F1(4),F2(8),F3(12),F4(16),FB(16),
     &  T1(4,1:8),T2(8,9:24),T3(12,25:40),T4(16,41:50),RB(16,3),
     &  DF1(4),DF2(8),DF3(12),DF4(16),DTE(114),DFB(16)
      EQUIVALENCE (F4,FB),(DF4,DFB)
C     821 WORDS
      LOGICAL FSET
      DATA PIDEG/57.29577951/
      DATA FSET/.FALSE./
      DIMENSION PO(4)
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      IF(NV.LE.11.AND.K.NE.K1) THEN
         IF(K.GT.0) THEN
            K1=K
            II=ITABL(IESDA,K,JESDFI)
            JJ=ITABL(IESDA,K,JESDTJ)
            KK=ITABL(IESDA,K,JESDDK)
         ELSE
            II=IECADE
            JJ=JECADE
            KK=KECADE
         END IF
         IF(JJ.LE.114) THEN
            JN=JJ
         ELSE
            JN=229-JJ
         END IF
         I1=II-1
         IF(JN.GT.50) THEN
C           IN=MOD(I1,32)+1  changed 16.7.98
            IN=MOD(I1+32,32)+1
            IM=I1/32
            IF(IN.GT.16) THEN
               IN=33-IN
               SF=1.
            ELSE
               SF=-1.
            END IF
            F=IM*A30+FSYM+SF*FB(IN)
            Z=ZB(JN,KK)
            R=RB(IN,KK)
            T=ATAN2(R,Z)
            DF=DFB(IN)
            GO TO 98
         ELSE IF(JN.GT.40) THEN
C           IN=MOD(I1,32)+1  changed 16.7.98
            IN=MOD(I1+32,32)+1
            IM=I1/32
            IF(IN.GT.16) THEN
               IN=33-IN
               SF=1.
            ELSE
               SF=-1.
            END IF
            IF(IN.GT.0) THEN
              T=T4(IN,JN)
              F=F4(IN)
              DF=DF4(IN)
            ELSE
              T=1.1111
              F=0.
              DF=1.
 9991         IF(IDEB.EQ.1) CALL DWRT('DVEC error at label 9991#')
            END IF
         ELSE IF(JN.GT.24) THEN
C           IN=MOD(I1,24)+1  changed 16.7.98
            IN=MOD(I1+24,24)+1
            IM=I1/24
            IF(IN.GT.12) THEN
               IN=25-IN
               SF=1.
            ELSE
               SF=-1.
            END IF
            IF(IN.GT.0) THEN
              T=T3(IN,JN)
              F=F3(IN)
              DF=DF3(IN)
            ELSE
              T=1.2222
              F=0.
              DF=1.
 9992         IF(IDEB.EQ.1) CALL DWRT('DVEC error at label 9992#')
            END IF
         ELSE IF(JN.GT.8) THEN
C           IN=MOD(I1,16)+1  changed 16.7.98
            IN=MOD(I1+16,16)+1
            IM=I1/16
            IF(IN.GT.8) THEN
               IN=17-IN
               SF=1.
            ELSE
               SF=-1.
            END IF
            IF(IN.GT.0) THEN
              T=T2(IN,JN)
              F=F2(IN)
              DF=DF2(IN)
            ELSE
              T=1.3333
              F=0.
              DF=1.
 9993         IF(IDEB.EQ.1) CALL DWRT('DVEC error at label 9993#')
            END IF
         ELSE
C           IN=MOD(I1,8)+1 changed 16.7.98
            IN=MOD(I1+8,8)+1
            IM=I1/8
            IF(IN.GT.4) THEN
               IN=9-IN
               SF=1.
            ELSE
               SF=-1.
            END IF
            IF(IN.GT.0) THEN
              T=T1(IN,JN)
              F=F1(IN)
              DF=DF1(IN)
            ELSE
              T=1.4444
              F=0.
              DF=1.
 9994         IF(IDEB.EQ.1) CALL DWRT('DVEC error at label 9994#')
            END IF
         END IF
         F=IM*A30+FSYM+SF*F
         Z=ZE(KK)
         R=TAN(T)*Z
   98    IF(JJ.GT.114) THEN
            Z=-Z
            T=A180-T
         END IF
      END IF
      GO TO (1,2,3,4,5,6,7,99,99,10,11,12,13,14,15,99,99,99,19,99),NV
C----------------------------------------------------   UNDEFINED
   99 DVEC=0.
      RETURN
C-----------------------------------------------------------   X
    1 DVEC=R*COS(F)
      RETURN
C-----------------------------------------------------------   Y
    2 DVEC=R*SIN(F)
      RETURN
C-----------------------------------------------------------   Z
    3 DVEC=Z
      RETURN
C-----------------------------------------------------------   RO
    4 DVEC=R
      RETURN
C-----------------------------------------------------------   FI
    5 IF(F.LT.0.) F=F+A360
      DVEC=F*PIDEG
      RETURN
C-----------------------------------------------------------   TE
    6 DVEC=T*PIDEG
      RETURN
C-----------------------------------------------------------   DI
    7 DVEC=SQRT(R**2+Z**2)
      RETURN
C-----------------------------------------------------------   DF
   10 DVEC=DF*PIDEG
      RETURN
C-----------------------------------------------------------   DT
   11 DVEC=DTE(JN)*PIDEG
      RETURN
C--------------------------------------------------------------------  E
   12 DVEC=RTABL(IESDA,K,JESDME)
      RETURN
C--------------------------------------------------------------------  I
   13 DVEC=ITABL(IESDA,K,JESDFI)
      RETURN
C--------------------------------------------------------------------  J
   14 DVEC=ITABL(IESDA,K,JESDTJ)
      RETURN
C--------------------------------------------------------------------  K
   15 DVEC=ITABL(IESDA,K,JESDDK)
      RETURN
C--------------------------------------------------------------------  N
C18    DVEC=ITABL(IESDA,K,JESDTN)
C      RETURN
C--------------------------------------------- CLUSTER NUMBER -------  U
   19 DVEC=ITABL(IESDA,K,JESDEC)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVEC1
CH
      ENTRY DVEC1(I0,J0,K0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C      II=I0
C      JJ=J0
C      KK=K0
      DVEC1=0
      IECADE=I0
      JECADE=J0
      KECADE=K0
      GO TO 111
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVEC2
CH
      ENTRY DVEC2(J0,K0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : J
C    Outputs   : CALCULATE AND STORE J,K, MEAN I
C
C    Called by :
C ---------------------------------------------------------------------
C     ENDCAP:
C     J= 1-  8 AND J=221-228   8 CHANNELS OF I : MEAN=2
C     J= 9- 24 AND J=205-220  16 CHANNELS OF I : MEAN=4
C     J=25- 40 AND J=189-204  24 CHANNELS OF I : MEAN=6
C     J=41- 50 AND J=179-188  32 CHANNELS OF I : MEAN=8
C     BARREL
C     J=51-114 AND J=115-178  32 CHANNELS OF I : MEAN=8
      DVEC2=0
      IF(     J0.GE.41.AND.J0.LE.188) THEN
        IECADE=II8
      ELSE IF(J0.GE.25.AND.J0.LE.204) THEN
        IECADE=II6
      ELSE IF(J0.GE. 9.AND.J0.LE.220) THEN
        IECADE=II4
      ELSE
        IECADE=II2
      END IF
      KECADE=K0
      JECADE=J0
      K1=0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVEC0
CH
      ENTRY DVEC0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVEC0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      KKKK=NAMIND('ESDA')
      IESDA = IW(NAMIND('ESDA'))
      IF(IESDA.EQ.0) RETURN
      NUM=LROWS(IESDA)
      K1=0
111   IF(FSET) RETURN
      FSET=.TRUE.
C---------------------------------------------------   FI SYMM. AXIS
      CALL DVECFT( 2,45,0.,FL,TDUM)
      CALL DVECFT(31,45,0.,FR,TDUM)
      FSYM=-0.5*(FL+FR)
C---------------------------------------------------   END CAP 1 F,T
      DO   700  JJ=1,8
         DO   710  II=1,4
            CALL DVECFT(II,JJ,FSYM,F1(II),T1(II,JJ))
  710    CONTINUE
  700 CONTINUE
C---------------------------------------------------   END CAP 2 F,T
      DO   720  JJ=9,24
         DO   730  II=1,8
            CALL DVECFT(II,JJ,FSYM,F2(II),T2(II,JJ))
  730    CONTINUE
  720 CONTINUE
C---------------------------------------------------   END CAP 3 F,T
      DO   740  JJ=25,40
         DO   750  II=1,12
            CALL DVECFT(II,JJ,FSYM,F3(II),T3(II,JJ))
  750    CONTINUE
  740 CONTINUE
C---------------------------------------------------   END CAP 4 F,T
      DO   760  JJ=41,50
         DO   770  II=1,16
            CALL DVECFT(II,JJ,FSYM,F4(II),T4(II,JJ))
  770    CONTINUE
  760 CONTINUE
C---------------------------------------------------   BARREL RO
      DO   780  II=1,16
         DO   790  KK=1,3
            CALL ESRBC('ALEPH',114,II,KK,PO)
            RB(II,KK)=SQRT(PO(1)**2+PO(2)**2)
  790    CONTINUE
  780 CONTINUE
C---------------------------------------------------   BARREL Z
      DO   800  JJ=51,114
         DO   810  KK=1,3
            CALL ESRBC('ALEPH',JJ,1,KK,PO)
            ZB(JJ,KK)=PO(3)
  810    CONTINUE
  800 CONTINUE
C---------------------------------------------------   END CAP Z
      DO   820  KK=1,3
         CALL ESRBC('ALEPH',1,1,KK,PO)
         ZE(KK)=PO(3)
  820 CONTINUE
C---------------------------------------------------   END CAP 1 DF
      DO   830  II=1,4
         CALL DVECDD(3,4,II, 4,DF1(II))
  830 CONTINUE
C---------------------------------------------------   END CAP 2 DF
      DO   840  II=1,8
         CALL DVECDD(3,4,II,16,DF2(II))
  840 CONTINUE
C---------------------------------------------------   END CAP 3 DF
      DO   850  II=1,12
         CALL DVECDD(3,4,II,32,DF3(II))
  850 CONTINUE
C---------------------------------------------------   END CAP 4 DF
      DO   860  II=1,16
         CALL DVECDD(3,4,II,45,DF4(II))
  860 CONTINUE
C---------------------------------------------------   DT
      DO   870  JJ=1,114
         CALL DVECDD(5,6, 4,JJ,DTE(JJ))
  870 CONTINUE
      CALL DVECHI(ZB,ZE,RB,T1,T2,T3,T4)
      END
*DK DVECDD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVECDD
CH
      SUBROUTINE DVECDD(N,M,I,J,D)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DIMENSION PL(4,6)
      DATA NPLAN/6/
      CALL ESRPL('ALEPH',J,I,K,NPLAN,PL)
      D=0.5*ACOS(ABS(PL(1,N)*PL(1,M)+PL(2,N)*PL(2,M)+PL(3,N)*PL(3,M)))
      END
*DK DVECFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVECFT
CH
      SUBROUTINE DVECFT(I,J,FSYM,F,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DIMENSION PO(4)
      CALL ESRBC('ALEPH',J,I,1,PO)
      F=FSYM-ATAN2(PO(2),PO(1))
      R=SQRT(PO(1)**2+PO(2)**2)
      T=ATAN2(R,PO(3))
      END
*DK DVECHI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVECHI
CH
      SUBROUTINE DVECHI(ZB,ZE,RB,T1,T2,T3,T4)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION ZB(51:114,3),ZE(3),RB(16,3),
     &  T1(4,1:8),T2(8,9:24),T3(12,25:40),T4(16,41:50)
      DATA ZEND/1.01/,RBAR/1.01/
C---------------------------- STORE FOR HISTOGRAM : DRZHE
      ZENDDE=ZEND
      DO   880  JJ=1,8
         RENDDE(JJ)=ZENDDE*TAN(T1(1,JJ))
  880 CONTINUE
      DO   890  JJ=9,24
         RENDDE(JJ)=ZENDDE*TAN(T2(1,JJ))
  890 CONTINUE
      DO   900  JJ=25,40
         RENDDE(JJ)=ZENDDE*TAN(T3(1,JJ))
  900 CONTINUE
      DO   910  JJ=41,50
         RENDDE(JJ)=ZENDDE*TAN(T4(1,JJ))
  910 CONTINUE
      DO   920  JJ=2,50
         RENDDE(JJ)=0.5*(RENDDE(JJ-1)+RENDDE(JJ))
  920 CONTINUE
      DR=RENDDE(3)-RENDDE(2)
      DO   930  JJ=1,-6,-1
         RENDDE(JJ)=RENDDE(JJ+1)-DR
  930 CONTINUE
C     RENDDE( 1)=RENDDE( 2)-(RENDDE( 3)-RENDDE( 2))
      RENDDE(51)=RENDDE(50)+(RENDDE(50)-RENDDE(49))
      RBARDE=RBAR
      Q=0.5*RBARDE/RB(1,3)
      DO 940 JJ=52,114
         ZBARDE(JJ)=Q*(ZB(JJ-1,3)+ZB(JJ,3))
  940 CONTINUE
      ZBARDE( 51)=ZBARDE( 52)-(ZBARDE( 53)-ZBARDE( 52))
      ZBARDE(115)=ZBARDE(114)+(ZBARDE(114)-ZBARDE(113))
      END
C*DK DVEW0
CCH..............+++
CCH
CCH
CCH
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVEW0
CCH
C      SUBROUTINE DVEW0(NMOD)
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
CC---------------------------------------------------------------------
CC get the number of modules from the PEWD bank
CC                                                S.Snow  20/8/89
CC---------------------------------------------------------------------
C      INCLUDE 'BCS.INC'
C      DIMENSION WIRES(45)
C      KPEWD  = NLINK('PEWD',0)
C      IF(KPEWD.EQ.0)THEN
C         NMOD = 0
C      ELSE
C         NMOD = IW(KPEWD+2)
C         LRO = IW(KPEWD+1)
C         NRO = IW(KPEWD+2)
C      END IF
C      RETURN
CCH..............---
CCH
CCH
CCH
CCH
CCH
CCH
CCH --------------------------------------------------------------------  DVEWMX
CCH
C      ENTRY DVEWMX(NBIN,EMAX)
CCH
CCH --------------------------------------------------------------------
CCH
C      MAXE=0
C      NSTEP=NBIN-1
C      DO 5 IRO = 1,NRO
C         I = KPEWD + 2 + ( IRO - 1 ) * LRO
C         DO 6 JB = 1,45,NBIN
C            IEN=0
C            DO 7 J=JB,MIN(45,JB+NSTEP)
C               IEN = IEN+IW(I+1+J)
C    7       CONTINUE
C            MAXE = MAX(IEN,MAXE)
C    6    CONTINUE
C    5 CONTINUE
C      EMAX=MAXE
C      RETURN
CCH..............---
CCH
CCH
CCH
CCH
CCH
CCH
CCH --------------------------------------------------------------------  DVEW
CCH
C      ENTRY DVEW(MODUL,NWIR,WIRES)
CCH
CCH --------------------------------------------------------------------
CCH
CC---------------------------------------------------------------------
CC get the wire digits from the PEWD bank
CC  input ;  MODUL  = module number   ( 1 -  36 )
CC output ;  NWIR = number of wires ( 0 or 45 )
CC        ;  WIRES(45)  the 45 planes in MeV
CC         module  1-12 : endcap A
CC         module 25-36 : endcap B
CC         module 13-24 : barrel
CC                                                S.Snow  20/8/89
CC---------------------------------------------------------------------
C      NWIR = 0
C      IF(KPEWD.NE.0)THEN
C         DO 3 IRO = 1,NRO
C            I = KPEWD + 2 + ( IRO - 1 ) * LRO
C            M = IW(I+1)
C            IF(M.EQ.MODUL)THEN
C               DO 4 J = 1,45
C                  WIRES(J) = IW(I+1+J)
C    4          CONTINUE
C               NWIR = 45
C               GO TO 9
C            END IF
C    3    CONTINUE
C      END IF
C    9 END
*DK DVEW0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVEW0
CH
      SUBROUTINE DVEW0(NMOD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C---------------------------------------------------------------------
C get the number of modules from the PEWD bank
C                                                S.Snow  20/8/89
C modified to get also EWDI and PEWI banks       P.Colas 19/9/89
C---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
C     NAME OF BANKS : EWIR
      DIMENSION WIRES(45)
      KPEWD  = NLINK('PEWD',0)
      IF(KPEWD.EQ.0) THEN
        EFAC=0.001
        KPEWD  = NLINK('PEWI',0)
        IF(KPEWD.EQ.0) KPEWD=NLINK('PWEI',0)
        IF(KPEWD.EQ.0) THEN
          KPEWD  = NLINK('EWDI',0)
            IF(KPEWD.EQ.0) THEN
               NMOD=0
            ELSE
               NMOD = IW(KPEWD+2)
               LRO = IW(KPEWD+1)
               NRO = IW(KPEWD+2)
            END IF
        ELSE
           NMOD = IW(KPEWD+2)
           LRO = IW(KPEWD+1)
           NRO = IW(KPEWD+2)
        END IF
      ELSE
         EFAC=1.
         NMOD = IW(KPEWD+2)
         LRO = IW(KPEWD+1)
         NRO = IW(KPEWD+2)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVEWMX
CH
      ENTRY DVEWMX(NBIN,EMAX)
CH
CH --------------------------------------------------------------------
CH
      MAXE=0
      NSTEP=NBIN-1
      DO 5 IRO = 1,NRO
         I = KPEWD + 2 + ( IRO - 1 ) * LRO
         DO 6 JB = 1,45,NBIN
            IEN=0
            DO 7 J=JB,MIN(45,JB+NSTEP)
               IEN = IEN+IW(I+1+J)
    7       CONTINUE
            MAXE = MAX(IEN,MAXE)
    6    CONTINUE
    5 CONTINUE
      EMAX=EFAC*MAXE
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVEW
CH
      ENTRY DVEW(MODUL,NWIR,WIRES)
CH
CH --------------------------------------------------------------------
CH
C---------------------------------------------------------------------
C get the wire digits from the PEWD bank
C  input ;  MODUL  = module number   ( 1 -  36 )
C output ;  NWIR = number of wires ( 0 or 45 )
C        ;  WIRES(45)  the 45 planes in MeV
C         module  1-12 : endcap A
C         module 25-36 : endcap B
C         module 13-24 : barrel
C                                                S.Snow  20/8/89
C---------------------------------------------------------------------
      NWIR = 0
      IF(KPEWD.NE.0)THEN
         DO 3 IRO = 1,NRO
            I = KPEWD + 2 + ( IRO - 1 ) * LRO
            M = IW(I+1)
            IF(M.EQ.MODUL)THEN
               DO 4 J = 1,45
                  WIRES(J) = EFAC*IW(I+1+J)
    4          CONTINUE
               NWIR = 45
               GO TO 9
            END IF
    3    CONTINUE
      END IF
    9 END
*DK DVETR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVETR
CH
      SUBROUTINE DVETR(K,XYZ)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      LOGICAL FNEW
      DIMENSION XYZ(3)
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_TREXJJ.INC'
      INCLUDE 'A_BMACRO.INC'
      XYZ(1)=RTABL(JTREX,K,JTREX3)
      XYZ(2)=RTABL(JTREX,K,JTREX3+1)
      XYZ(3)=RTABL(JTREX,K,JTREX3+2)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVETR0
CH
      ENTRY DVETR0(N,NUM,XYZ)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
      IF(FNEW) THEN
        CALL DFTRAK
        FNEW=.FALSE.
      END IF
      JTREX=NLINK('TREX',N)
      IF(JTREX.EQ.0) THEN
        NUM=0
      ELSE
        NUM=LROWS(JTREX)
        IF(NUM.NE.0) THEN
          XYZ(1)=RTABL(JTREX,2,JTREX3)
          XYZ(2)=RTABL(JTREX,2,JTREX3+1)
          XYZ(3)=RTABL(JTREX,2,JTREX3+2)
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVETRN
CH
      ENTRY DVETRN
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  new event
C    Called by : DEVNEW
C ---------------------------------------------------------------------
      FNEW=.TRUE.
      END
*DK DVFRID
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVFRID
CH
      SUBROUTINE DVFRID(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      NUM=0
      END
*DK DVFTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DVFTR
CH
      SUBROUTINE DVFTR(JTK,NPNT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Tomas Lohse
C
C!:
C    Inputs    : jtk = track number
C    Outputs   : npnt = # of ITC + TPC points
C
C    Called by :
C ---------------------------------------------------------------------
      DIMENSION HP(66),COV(21)
C
      COMMON / INFDAL / NLOW, NHIGH, XSTS, XME, RF, IUSED, VMEAS
C
      PARAMETER (MPT=40)
      DATA BFILD/15./

      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)
      DATA IERICO/0/
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_FRTLJJ.INC'
      INCLUDE 'A_FRFTJJ.INC'
      INCLUDE 'A_BMACRO.INC'
C
C--->     LOOP OVER TRACK AGAIN TO DO THE FINAL FIT
C
      IF(JTK.LE.0) GO TO 610
C      IF(JTK.EQ.JTOLD) THEN
C        NPNT=NPOLD
C        RETURN
C      END IF
      JTOLD=JTK
      CALL DPARGV(20,'HTB',2,HTB2)
      KFRFT = NLINK('FRFT',IFIX(HTB2))
      KFICL=IW(NAMIND('FICL'))
      KFTCL=IW(NAMIND('FTCL'))
      KFVCL=IW(NAMIND('FVCL'))
      KFRTL=IW(NAMIND('FRTL'))
      IOITC=ITABL(KFRTL,JTK,JFRTII)
      IOTPC=ITABL(KFRTL,JTK,JFRTIT)
      IOVDT=ITABL(KFRTL,JTK,JFRTIV)
      NTPC=ITABL(KFRTL,JTK,JFRTNT)
      NITC=ITABL(KFRTL,JTK,JFRTNI)
      NVDET=ITABL(KFRTL,JTK,JFRTNV)
C
      NDOF=2*(NTPC+NVDET) + NITC -5
      IF (NDOF.LT.1) GO TO 610
      IF (NTPC.EQ.1) GO TO 610
C
C--->     IF THE CREATION OF ICCO FAILED KEEP THE ORIGINAL FIT
C
      IF ( IERICO .GT. 0 )   GOTO 610
C
C++       This is the KALMAN filter fitting routine.  The CHI2 returned
C++       is very large (10**30) if the fit fails.
C qmfld=m.
C feld
C
      CALL DALINF(1)
Cold      CALL UFTTRK(BFILD,RW(KROW(KFRFT,JTK)+JFRFIR),
      CALL UFTTRA(JTK,BFILD,RW(KROW(KFRFT,JTK)+JFRFIR),
     &              RW(KROW(KFRFT,JTK)+JFRFC2),
     &              NTPC,NITC,NVDET,IW(KFTCL+LMHLEN+IOTPC+1),
     &              IW(KFICL+LMHLEN+IOITC+1),IW(KFVCL+LMHLEN+IOVDT+1),
     &              HP,COV,CHI2,NDOF)
      CALL DALINF(0)
C      IF (HP(1).EQ.0.) GO TO 610
C      IF (COV(1).LE.0.) GO TO 610
C      IF (COV(3).LE.0.) GO TO 610
C      IF (COV(6).LE.0.) GO TO 610
C      IF (COV(10).LE.0.) GO TO 610
C      IF (COV(15).LE.0.) GO TO 610
C      IF (COV(21).LT.0.) GO TO 610
      NPNT=NTPC+NITC+NVDET
      NPOLD=NPNT
      DO 740 K=1,NPNT
        VMEAS(1,1,K)=SQRT(VMEAS(1,1,K))
        VMEAS(2,2,K)=SQRT(VMEAS(2,2,K))
  740 CONTINUE
      RETURN
  610 NPNT=0
      JTOLD=0
      END
*DK DVHC
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVHC
CH
      FUNCTION DVHC(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'J_HCNAMC.INC'
      INCLUDE 'A_HTUBJJ.INC'
      INCLUDE 'A_HSDAJJ.INC'
      INCLUDE 'A_HSTOJJ.INC'
      DATA PIDEG/57.29577951/,PIDE2/28.64788975/
CDEB      CHARACTER*1 TDEB
CDEB      DATA TDEB/'N'/
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      IF(K.NE.K1.AND.(NV.EQ.5.OR.NV.EQ.6.OR.NV.EQ.10.OR.NV.EQ.11)) THEN
         K1=K
         II=ITABL(IHSDA,K,JHSDPI)
         JJ=ITABL(IHSDA,K,JHSDTI)
         KK=ITABL(IHSDA,K,JHSDSN)
         NN=ITABL(IHSDA,K,JHSDSC)
C          NS=ITABL(IHSDA,K,JHSDRN)
         CALL HUSRAN(JJ,II,KK,TE,FI,DT,DF)
C          IF(TDEB.EQ.'D') THEN
C              TYPE *,'K,IHSDA ',K,IHSDA
C              TYPE *,'JHSDIT,PI,SN,DC,RN ',
C     &          JHSDIT,JHSDPI,JHSDSN,JHSDSC,JHSDRN
C              TYPE *,'I,J,K,N,NS ',II,JJ,KK,NN,NS
C              TYPE *,'T,F,DT,DF ',TE,FI,DT,DF
C              XD=RTABL(IHSTO,K,JHSTSX)
C              YD=RTABL(IHSTO,K,JHSTSY)
C              ZD=RTABL(IHSTO,K,JHSTSZ)
C              ED=RTABL(IHSTO,K,JHSDDE)
C              TYPE *,'IHSTO ',IHSTO
C              TYPE *,'JHSTSX,Y,Z,DDE ',JHSTSX,JHSTSY,JHSTSZ,JHSDDE
C              TYPE *,'XD,YD,ZD,ED ',XD,YD,ZD,ED
C              RC=SQRT(XD**2+YD**2)
C              XC=RC*COS(FI)
C              YC=RC*SIN(FI)
C              ZC=RC/TAN(TE)
C              TYPE *,'XC,YC,ZC,RC ',XC,YC,ZC,RC
C              ACCEPT '(A)',TDEB
C          END IF
      END IF
      GO TO (1,2,3,4,5,6,7,99,99,10,11,12,13,14,15,16,99,99,19,99),NV
C-------------------------------------------------------------  UNDEFINE
   99 DVHC=0.
      RETURN
C--------------------------------------------------------------------  X
    1 DVHC=RTABL(IHSTO,K,JHSTSX)
      RETURN
C--------------------------------------------------------------------  Y
    2 DVHC=RTABL(IHSTO,K,JHSTSY)
      RETURN
C--------------------------------------------------------------------  Z
    3 DVHC=RTABL(IHSTO,K,JHSTSZ)
      RETURN
C--------------------------------------------------------------------  R
    4 IF(K.NE.KRO) THEN
         R2=RTABL(IHSTO,K,JHSTSX)**2+RTABL(IHSTO,K,JHSTSY)**2
         KRO=K
         R=SQRT(R2)
      END IF
      DVHC=R
      RETURN
C--------------------------------------------------------------------  F
    5 DVHC=FI*PIDEG
      IF(DVHC.LT.0.) DVHC=DVHC+360.
      RETURN
C--------------------------------------------------------------------  T
    6 DVHC=TE*PIDEG
      RETURN
C--------------------------------------------------------------------  D
    7 IF(K.NE.KRO) THEN
         R2=RTABL(IHSTO,K,JHSTSX)**2+RTABL(IHSTO,K,JHSTSY)**2
         KRO=K
      END IF
      DVHC=SQRT(R2+RTABL(IHSTO,K,JHSTSZ)**2)
      RETURN
C--------------------------------------------------------------------  D
   10 DVHC=DF*PIDE2
      RETURN
C--------------------------------------------------------------------  D
   11 DVHC=DT*PIDE2
      RETURN
C--------------------------------------------------------------------  E
   12 DVHC=RTABL(IHSDA,K,JHSDDE)
      RETURN
C--------------------------------------------------------------------  I
   13 DVHC=ITABL(IHSDA,K,JHSDPI)
      RETURN
C--------------------------------------------------------------------  J
   14 DVHC=ITABL(IHSDA,K,JHSDTI)
      RETURN
C--------------------------------------------------------------------  K
   15 DVHC=ITABL(IHSDA,K,JHSDSN)
      RETURN
C--------------------------------------------------------------------  N
   16 DVHC=ITABL(IHSDA,K,JHSDSC)
      RETURN
C--------------------------------------------------------------------  U
   19 DVHC=ITABL(IHSDA,K,JHSDCN)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVHC0
CH
      ENTRY DVHC0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVHC0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IHSDA = IW(NAMIND('HSDA'))
      IHSTO = IW(NAMIND('HSTO'))
      IF(IHSDA.EQ.0.OR.IHSTO.EQ.0) RETURN
      NUM=LROWS(IHSDA)
      K1=0
      KRO=0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVHC1
CH
      ENTRY DVHC1(I0,J0,K0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVHC1=0
      II=I0
      JJ=J0
      KK=K0
      END
*DK DVIT
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVIT
CH
      FUNCTION DVIT(NV,K,NFI)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION NITCF(2)
      DATA ZMAX/217./,RMAX/170.622/
      DATA PIDEG/57.29577951/
      DIMENSION NSTOR(11)
      INCLUDE 'J_ITNAMC.INC'
      INCLUDE 'A_ITCOJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      GO TO(1,2,3,4,5,6,7,8,99,99,99,99,99,99,99,99,99,18,99,99),NV
C--------------------------------------------------------------  UNDEFIN
   99 DVIT=0.
      RETURN
C----------------------------------------------------------------------
    1 DVIT=COS(RTABL(IITCO,K,NITCF(NFI)))*RTABL(IITCO,K,JITCRA)
      RETURN
C----------------------------------------------------------------------
    2 DVIT=SIN(RTABL(IITCO,K,NITCF(NFI)))*RTABL(IITCO,K,JITCRA)
      RETURN
C----------------------------------------------------------------------
    3 DVIT=RTABL(IITCO,K,JITCZH)
      RETURN
C----------------------------------------------------------------------
    4 DVIT=RTABL(IITCO,K,JITCRA)
      RETURN
C----------------------------------------------------------------------
    5 DVIT=RTABL(IITCO,K,NITCF(NFI))*PIDEG
      RETURN
C----------------------------------------------------------------------
    6 Z=RTABL(IITCO,K,JITCZH)-VRDZDV
      DVIT=ATAN2(RTABL(IITCO,K,JITCRA),Z)*PIDEG
      RETURN
C----------------------------------------------------------------------
    7 DVIT=SQRT(RTABL(IITCO,K,JITCRA)**2+(RTABL(IITCO,K,JITCZH))**2)
      RETURN
C----------------------------------------------------------------------
    8 Z0=RTABL(IITCO,K,JITCZH)
      R0=RTABL(IITCO,K,JITCRA)
      ZA=ABS(Z0)
      D0=SQRT(Z0**2+R0**2)
      IF(R0*ZMAX.GT.ZA*RMAX) THEN
         DVIT=D0*(RMAX/R0-1.)
      ELSE
         DVIT=D0*(ZMAX/ZA-1.)
      END IF
      RETURN
C----------------------------------------------------------------------
   18 IF(NSTOR(10).EQ.1) THEN
         N1=NITCDV(10,K)
         IF(N1.EQ.0) THEN
            DVIT=NITCDV(11,K)
         ELSE
            DVIT=N1
         END IF
      ELSE
         DVIT=0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVIT0
CH
      ENTRY DVIT0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVIT0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IITCO = IW(NAMIND('ITCO'))
      IF(IITCO.EQ.0) RETURN
      NUM=LROWS(IITCO)
      NUMST=NUM
      NITCF(1)=JITCP1
      NITCF(2)=JITCP2
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVITCL
CH
      ENTRY DVITCL(NV)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVITCL=0
      IF(NV.EQ.0) THEN
         CALL VZERO(NSTOR,11)
      ELSE
         IV=NV
         IF(NV.EQ.18) IV=10
         NSTOR(IV)=0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVITST
CH
      ENTRY DVITST(NV,J1,J2)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVITST=0
      J1=NV
      IF(J1.EQ.1) THEN
         J2=7
      ELSE IF(J1.EQ.2) THEN
         J2=8
      ELSE IF(J1.EQ.5) THEN
         J2=9
      ELSE IF(J1.EQ.18) THEN
         J1=10
         J2=11
      ELSE
         J2=1
      END IF
      IF(NSTOR(J1).EQ.1) RETURN
      NSTOR(J1)=1
      GO TO(101,102,103,104,105,106,101,101,101,110),J1
  101 DO 701 J=1,NUMST
         VITCDV(J1,J)=COS(RTABL(IITCO,J,NITCF(1)))*RTABL(IITCO,J,JITCRA)
         VITCDV(J2,J)=COS(RTABL(IITCO,J,NITCF(2)))*RTABL(IITCO,J,JITCRA)
  701 CONTINUE
      RETURN
C----------------------------------------------------------------------
  102 DO 702 J=1,NUMST
         VITCDV(J1,J)=SIN(RTABL(IITCO,J,NITCF(1)))*RTABL(IITCO,J,JITCRA)
         VITCDV(J2,J)=SIN(RTABL(IITCO,J,NITCF(2)))*RTABL(IITCO,J,JITCRA)
  702 CONTINUE
      RETURN
C----------------------------------------------------------------------
  103 DO 703 J=1,NUMST
         VITCDV(J1,J)=RTABL(IITCO,J,JITCZH)
  703 CONTINUE
      RETURN
C----------------------------------------------------------------------
  104 DO 704 J=1,NUMST
         VITCDV(J1,J)=RTABL(IITCO,J,JITCRA)
  704 CONTINUE
      RETURN
C----------------------------------------------------------------------
  105 DO 705 J=1,NUMST
         VITCDV(J1,J)=RTABL(IITCO,J,NITCF(1))*PIDEG
         VITCDV(J2,J)=RTABL(IITCO,J,NITCF(2))*PIDEG
  705 CONTINUE
      RETURN
C----------------------------------------------------------------------
  106 DO 706 J=1,NUMST
         Z=RTABL(IITCO,J,JITCZH)-VRDZDV
         VITCDV(J1,J)=ATAN2(RTABL(IITCO,J,JITCRA),Z)*PIDEG
  706 CONTINUE
      RETURN
C----------------------------------------------------------------------
  110 DO 710 J=1,NUMST
         NITCDV(J1,J)=0
         NITCDV(J2,J)=0
  710 CONTINUE
      CALL=DVCHI0(NTRK)
      IF(NTRK.EQ.0) RETURN
      DO 810 N=1,NTRK
         CALL=DVCHI(N,NCO)
         DO 910 KCO=1,NCO
            I=IDVCHI(KCO)
            IF(I.GT.0) THEN
               NITCDV(J1,I)=N
            ELSE
               NITCDV(J2,-I)=N
            END IF
  910    CONTINUE
  810 CONTINUE
      END
*DK DVIT
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVIC
CH
      FUNCTION DVIC(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C     IICO is parrallel to ITCO!
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION NITCF(2)
      DATA ZMAX/217./,RMAX/170.622/
      DATA PIDEG/57.29577951/
C     INCLUDE 'A_ICCOJJ.INC'
      PARAMETER (JICCRV=1,JICCPH=2,JICCZV=3)
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      GO TO(1,2,3,4,5,6),NV
C--------------------------------------------------------------  UNDEFIN
   99 DVIC=0.
      RETURN
C----------------------------------------------------------------------
    1 DVIC=COS(RTABL(IICCO,K,JICCPH))*RTABL(IICCO,K,JICCRV)
      RETURN
C----------------------------------------------------------------------
    2 DVIC=SIN(RTABL(IICCO,K,JICCPH))*RTABL(IICCO,K,JICCRV)
      RETURN
C----------------------------------------------------------------------
    3 DVIC=RTABL(IICCO,K,JICCZV)
      RETURN
C----------------------------------------------------------------------
    4 DVIC=RTABL(IICCO,K,JICCRV)
      RETURN
C----------------------------------------------------------------------
    5 DVIC=RTABL(IICCO,K,JICCPH)*PIDEG
      RETURN
C----------------------------------------------------------------------
    6 Z=RTABL(IICCO,K,JICCZV)-VRDZDV
      DVIC=ATAN2(RTABL(IICCO,K,JICCRV),Z)*PIDEG
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVIC0
CH
      ENTRY DVIC0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVIC0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IICCO = IW(NAMIND('ICCO'))
      IF(IICCO.EQ.0) RETURN
      NUM=LROWS(IICCO)
      END
C"*DK DVFILV
C"CH..............+++
C"CH
C"CH
C"CH
C"CH
C"CH
C"CH
C"CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVFIL
C"CH
C"      INTEGER FUNCTION DVFILN(N)
C"CH
C"CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C"CH
C"C ---------------------------------------------------------------------
C"C
C"C    Created by H.Drevermann                   28-JUL-1988
C"C
C"C!:
C"C    Inputs    :
C"C    Outputs   :
C"C
C"C    Called by :
C"C ---------------------------------------------------------------------
C"      PARAMETER (MFILV=200)
C"      DIMENSION NFILV(MFILV)
C"      IF(NUM.EQ.0) THEN
C"        DVFILN=0
C"      ELSE
C"        IF(N.LE.MFILV) THEN
C"          DVFILN=NFILV(N)
C"        ELSE
C"          DVFILN=0
C"        END IF
C"      END IF
C"      RETURN
C"      ENTRY DVFILV(NTR)
C"      IF(FFFFF) RETURN
C"      FFFFF=.TRUE.
C"      NUM=0
C"      IF(IW(30).NE.12345) RETURN
C"      IPYFR=IW(NAMIND('PYFR'))
C"      IF(IPYFR.EQ.0) RETURN
C"      NUM=MIN(LROWS(IPYFR),MFILV)
C"      DO 700 K=1,NUM
C"        ITRA=ITABL(IPYFR,K,2)
C"        NFILV(ITRA)=ITABL(IPYFR,K,1)
C"  700 CONTINUE
C"      END
*DK DVEF
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVEF
CH
      FUNCTION DVEF(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'A_EFOLJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
C             1  2  3  4  5  6  7  8  9  0
      GO TO ( 1, 2, 3, 4, 5, 6,99,99, 9,99,
     &       99,12,99,99,99,99,99,99,99,99),NV
C-------------------------------------------------------------  UNDEFINE
   99 DVEF=0.
      RETURN
C-------------------------------------------------------------------- PX
    1 DVEF=RTABL(IEFOL,K,JEFOPX)
      RETURN
C-------------------------------------------------------------------- PY
    2 DVEF=RTABL(IEFOL,K,JEFOPY)
      RETURN
C-------------------------------------------------------------------- PZ
    3 DVEF=RTABL(IEFOL,K,JEFOPZ)
      RETURN
C-------------------------------------------------------------------- PT
    4 X=RTABL(IEFOL,K,JEFOPX)
      Y=RTABL(IEFOL,K,JEFOPY)
      DVEF=SQRT(X*X+Y*Y)
      RETURN
C--------------------------------------------------------------------  F
    5 X=RTABL(IEFOL,K,JEFOPX)
      Y=RTABL(IEFOL,K,JEFOPY)
      DVEF=DATN2D(Y,X)
      RETURN
C--------------------------------------------------------------------  T
    6 X=RTABL(IEFOL,K,JEFOPX)
      Y=RTABL(IEFOL,K,JEFOPY)
      Z=RTABL(IEFOL,K,JEFOPZ)
      R=SQRT(X*X+Y*Y)
      DVEF=DATN2D(R,Z)
      RETURN
C--------------------------------------------------------------------  P
CNOW    7 X=RTABL(IEFOL,K,JEFOPX)
CNOW      Y=RTABL(IEFOL,K,JEFOPY)
CNOW      Z=RTABL(IEFOL,K,JEFOPZ)
CNOW      DVEF=SQRT(X*X+Y*Y+Z*Z)
CNOW      RETURN
C--------------------------------------------------------------------  TYPE
    9 DVEF=ITABL(IEFOL,K,JEFOTY)
      RETURN
C--------------------------------------------------------------------  E
   12 DVEF=RTABL(IEFOL,K,JEFOEW)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVEF0
CH
      ENTRY DVEF0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVEF0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IEFOL = IW(NAMIND('EFOL'))
      IF(IEFOL.EQ.0) RETURN
      NUM=LROWS(IEFOL)
      END
*DK DVEO
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVEO
CH
      FUNCTION DVEO(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'A_PECOJJ.INC'
      DATA PIDEG/57.29577951/
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
C             1  2  3  4  5  6  7  8  9  0
      GO TO (99,99,99,99, 5, 6,99,99,99,99,
     &       99,12,99,99,99,16,99,99,99,99),NV
C-------------------------------------------------------------  UNDEFINE
   99 DVEO=0.
      RETURN
C--------------------------------------------------------------------  F
    5 DVEO=RTABL(IPECO,K,JPECPH)*PIDEG
      IF(DVEO.LT.0.) DVEO=DVEO+360.
      RETURN
C--------------------------------------------------------------------  T
    6 DVEO=RTABL(IPECO,K,JPECTH)*PIDEG
      RETURN
C--------------------------------------------------------------------  E
C     ................. CHANGED FROM ER TO EC THE 14.4.92 WITH STEPHEN HAYWOOD
   12 DVEO=RTABL(IPECO,K,JPECEC)
      RETURN
C--------------------------------------------------------------------  N
   16 DVEO=ITABL(IPECO,K,JPECRB)
      RETURN
CC--------------------------------------------------------------------  U
C   19 DVEO=ITABL(IHSDA,K,JHSDCN)
C      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVEO0
CH
      ENTRY DVEO0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVEO0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IPECO = IW(NAMIND('PECO'))
      IF(IPECO.EQ.0) RETURN
      NUM=LROWS(IPECO)
      END
*DK DVLC
CH
CH
CH
CH
CH
CH
CH
      FUNCTION DVLC(NV,K)
C
C       Created by F. Bird          21-JUL-1989
C          from a similar file of H. Drevermann  28-JUL-1988
C
C
C
      COMMON/DLCBNK/PADSDL,CRAKDL,ZPOSDL,SXCEDL(16,15),SYCEDL(16,15)
      DIMENSION SX(4),SZ(4),SY(0:1)
      DATA SX/-1., 1.,-1., 1./
      DATA SZ/-1.,-1., 1., 1./
      DATA SY/-1., 1./
      DIMENSION ZPOS(3)
      DATA ZPOS/270.2,281.2,295.4/
      DATA PIDEG/57.29577951/
      DATA QE/0.005/
C      PARAMETER (JLSDAD= 1, JLSDEN= 2)
C      PARAMETER (JLCRZ1=23, JLCRZ2=24, JLCRZ3=25, JLCRZC=26, JLCRSR=28)
C
      INCLUDE 'A_PLSDJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C
      IF(K.LT.0) GO TO 99
C
      KI=   (K-1)/3 +1
      KK=MOD(K-1 ,3)+1
      IF(NV.GE.12) THEN
        GO TO (12,99,99,15,99,99,99,19,99) NV-11
      END IF
C
      IF(K.NE.K0) THEN
        K0=K
        IAD = ITABL(KPLSD,KI,JPLSAD)
        MDS = 1 + IAD/512
        IDS=MDS-1
        IRW = 1 + (IAD-512*IDS-1)/16
C
        IF(IRW.GE.16) THEN
          IR1=1
          IRS = 31-IRW
        ELSE
          IR1=0
          IRS = IRW
        END IF
        ICS = IAD-512*IDS-16*(IRW-1)
        SGX = SX(MDS)
        SGZ = SZ(MDS)
        SGY = SY(IR1)
      END IF
C
      GO TO (1,2,3,4,5,6,7,8,9,10,11) NV
C
   99 DVLC = 0.                      ! undefined
      RETURN
C
    1 DVLC  = SXCEDL(ICS,IRS)*SGX     ! X POSITION
      RETURN
C
    2 DVLC = SYCEDL(ICS,IRS)*SGY   ! y position
      RETURN
C
    3 DVLC =  ZPOS(KK)*SGZ            ! z position
      RETURN
C
C                                               ! RHO
    4 IF(KR.NE.K) THEN
        KR=K
        X  = SXCEDL(ICS,IRS)*SGX
        Y  = SYCEDL(ICS,IRS)*SGY
        RHO  = SQRT(X*X+Y*Y)
      END IF
      DVLC = RHO
      RETURN
C
C                                                ! phi position
    5 IF(KF.NE.K) THEN
        KF=K
        X  = SXCEDL(ICS,IRS)*SGX
        Y  = SYCEDL(ICS,IRS)*SGY
        FI = ATAN2(Y,X) * PIDEG
        FI = MOD(FI+3600.,360.)
      END IF
      DVLC=FI
      RETURN
C
                                                 ! theta position
    6 IF(KT.NE.K) THEN
        KT=K
        X  = SXCEDL(ICS,IRS)*SGX
        Y  = SYCEDL(ICS,IRS)*SGY
        R  = SQRT(X*X+Y*Y)
        Z  = ZPOS(KK)*SGZ
        TH = ATAN2(R,Z)*PIDEG
      END IF
      DVLC = TH
      RETURN
C
                                                 ! di
    7 X  = SXCEDL(ICS,IRS)*SGX
      Y  = SYCEDL(ICS,IRS)*SGY
      Z  = ZPOS(KK)*SGZ
      DVLC  = SQRT(X*X+Y*Y+Z*Z)
      RETURN
    8 DVLC=IAD
      RETURN
    9 DVLC=IRW
      RETURN
   10 DVLC=IRS
      RETURN
   11 DVLC=ICS
      RETURN
C
   12 DVLC = ITABL(KPLSD,KI,JEN+KK)*QE          ! es
      RETURN
C
   15 DVLC = KK                      ! is
      RETURN
C
   19 DVLC = KI                      ! u
      RETURN
C
      ENTRY DVLC0(NUM)
      DVLC0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
C
      KPLSD=IW(NAMIND('PLSD'))     ! LCAL reconstructed clusters
      IF(KPLSD.EQ.0) RETURN
      NUM=LROWS(KPLSD)*3
      KT=0
      KF=0
      KR=0
      K0=0
      JEN=JPLSE1-1
      CALL DVLCIN
      END
*DK DVLCIN
CH
CH
CH
CH
      SUBROUTINE DVLCIN
      COMMON/DLCBNK/PADSDL,CRAKDL,ZPOSDL,SXCEDL(16,15),SYCEDL(16,15)
      DATA ZPOS,PADS,CRAK/280.,2.975,0.0635/
      LOGICAL FINIT
      DATA FINIT/.FALSE./
C
      INCLUDE 'A_LCREJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C
      IF(FINIT) RETURN
      IF(IW(30).NE.12345) RETURN
      FINIT=.TRUE.

      KLCRE=IW(NAMIND('LCRE'))     ! LCAL reconstruction constants
      IF(KLCRE.EQ.0) THEN
        ZPOSDL=ZPOS
        PADSDL=PADS
        CALL DWRT('Bank LCRE is missing.')
      ELSE
        ZPOSDL = RTABL(KLCRE,1,JLCRZC)
        PADSDL = RTABL(KLCRE,1,JLCRSR)
      END IF
      CRAKDL=CRAK
C
C  define the pad size
C
      XOFFST=PADSDL/2.+CRAKDL
      YOFFST=CRAKDL+CRAKDL/2.
C
      DO IY=1,15
         DO IX=1,16
            SXCEDL(IX,IY)=XOFFST+(IX-1)*PADSDL+PADSDL/2.
            SYCEDL(IX,16-IY)=YOFFST+(IY-1)*PADSDL+PADSDL/2.
         ENDDO
      ENDDO
C
      RETURN
      END
*DK DVMCNV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVMCNV
CH
      SUBROUTINE DVMCNV(NVTX,NTRA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 10-May-1989   C.Grab  Adapted to CERNVM, Added log.definitions
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C-----------------------------------------------------------------------
C Return number of vertices
C-----------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION PTRA(3),CLTM(3),XVER(3)
      LOGICAL FTK,FSH
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      DATA NFKIN/0/
      DATA AMAG/15./,FTK/.TRUE./,FSH/.TRUE./,CUT/0.1/
      INCLUDE 'DALI_KI.INC'
      NVTX=0
      NTRA=0
      IF (IW(30).NE.12345) RETURN
      NAPAR = NAMIND('PART')
      JPAR=IW(NAPAR)
      IF(JPAR.EQ.0) THEN
        IDUM=MDARD(IW,LDBSDG,'PART',0)
        JPAR=IW(NAPAR)
      END IF
      IF(JPAR.EQ.0.AND.IRUNDE(1).LT.2000) THEN
         CALL DWRT('No PART bank. FKINE cannot be displayed.')
         RETURN
      END IF
      MAXTP=LROWS(JPAR)
      IF(NFKIN.EQ.0) THEN
         CALL FYIRUN(AMAG,FTK,FSH,CUT)
         NFKIN=NAMIND('FKIN')
         NFVER=NAMIND('FVER')
      END IF
      JFVER=IW(NFVER)
      IF(JFVER.EQ.0) THEN
         CALL FYKINE('DROP','NOGA',IER)
         IF(IER.NE.0) THEN
            CALL DWRT('Error in FYKINE.')
            RETURN
         END IF
         JFVER=IW(NFVER)
      END IF
      NVTX=LROWS(JFVER)
      JFKIN=IW(NFKIN)
      NTRA=LROWS(JFKIN)
      MTRA=NTRA
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVMCVX
CH
      ENTRY DVMCVX(NUMVR,KTRA1,KTRA2,KTRIN,XVER)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C-----------------------------------------------------------------------
C Input NUMVR vertex number
C Output NTRA Number of outgoing tracks
C        XVER(3) Vertex coordinates
C-----------------------------------------------------------------------
      IF(JFVER.NE.0) THEN
         KFVER=KROW(JFVER,NUMVR)
         KTRA1   = IW(KFVER+6)+1
         KTRA2   = IW(KFVER+7)+IW(KFVER+6)
         KTRIN   = IW(KFVER+5)
         XVER(1) = RW(KFVER+1)
         XVER(2) = RW(KFVER+2)
         XVER(3) = RW(KFVER+3)
      ELSE
         KTRA1=0
      ENDIF
      IVR=NUMVR
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVMCTR
CH
      ENTRY DVMCTR(KTRA,PTRA,CLTM,ITYPE,NVR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C-----------------------------------------------------------------------
C INPUT  KTRA Track number
C Output PTRA(3) Track momenta
C        Q      Charge
C        ITYPE  Particle type
C        NVR    Vertex number at end of track
C-----------------------------------------------------------------------
      IF(JFKIN.NE.0) THEN
         KFKIN=KROW(JFKIN,KTRA)
         NVR=IW(KFKIN+7)
         ITYPE   = IW(KFKIN+5)
         IF(ITYPE.LE.0) THEN
           CALL DWRT(' --> in DVMCTR: ITYPE=<0')
         END IF
C        IF(NVR.EQ.IVR.OR.ITYPE.GT.MAXTP) GO TO 90
C        .................................................. CHANGED 15.6.92
         IF(ITYPE.GT.MAXTP) GO TO 90
         PTRA(1) = RW(KFKIN+1)
         PTRA(2) = RW(KFKIN+2)
         PTRA(3) = RW(KFKIN+3)
CSO         CLTM(1) = CHARGE(ITYPE)
         CLTM(1) = RTABL(JPAR,ITYPE,7)
CSO         TI = TIMLIF(ITYPE)
         TI = RTABL(JPAR,ITYPE,8)
         IF(TI.LT.0.) THEN
            CLTM(2)=-99.
         ELSE IF(TI.EQ.0)THEN
            CLTM(2)=-90.
         ELSE
            CLTM(2) = ALOG10(TI)
         END IF
CSO         CLTM(3)=PARMAS(ITYPE)
         CLTM(3)=RTABL(JPAR,ITYPE,6)
      ELSE
         ITYPE=-9999
      ENDIF
      RETURN
   90 ITYPE=-ITYPE
      END
*DK DVCORD
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVCORD
CH
      FUNCTION DVCORD(XYZ,NV)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3)
      DATA ZMAX/217./,RMAX/170.622/
      DATA PIDEG /57.2957/
      GO TO (1,2,3,4,5,6,7,8),NV
   99 DVCORD=0.
      RETURN
    1 DVCORD=XYZ(1)
      RETURN
    2 DVCORD=XYZ(2)
      RETURN
    3 DVCORD=XYZ(3)
      RETURN
    4 DVCORD=SQRT(XYZ(1)**2+XYZ(2)**2)
      RETURN
    5 DVCORD=ATAN2(XYZ(2),XYZ(1))*PIDEG
      IF(DVCORD.LT.0.) DVCORD=DVCORD+360.
      RETURN
    6 RO=SQRT(XYZ(1)**2+XYZ(2)**2)
      DVCORD=ATAN2(RO,XYZ(3)-VRDZDV)*PIDEG
      RETURN
    7 DVCORD=SQRT(XYZ(1)**2+XYZ(2)**2+XYZ(3)**2)
      RETURN
    8 ZA=ABS(XYZ(3))
      RO=XYZ(1)**2+XYZ(2)**2
      D=SQRT(RO+XYZ(3)**2)
      RO=SQRT(RO)
      IF(RO*ZMAX.GT.ZA*RMAX) THEN
         DVCORD=D*(RMAX/RO-1.)
      ELSE
         DVCORD=D*(ZMAX/ZA-1.)
      END IF
      END
*DK DVPADA
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVPADA
CH
      FUNCTION DVPADA(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION NLIMT(0:36),NSECN(36)
      LOGICAL FONE
      IF(FONE) THEN
         DVPADA=DVPADS(NV,K)
         RETURN
      END IF
      IF(K.EQ.KOLD) THEN
         DVPADA=DVPADS(NV,NP)
         RETURN
      ELSE IF(K.EQ.KOLD+1) THEN
         IF(K.GT.NLIMT(NS)) THEN
            IF(K.GT.NHT0) THEN
               DVPADA=99999.
               RETURN
            END IF
            NS=NS+1
            CALL DVPADF(ISTRT,NS,JSECT,NHITS)
            NP=K-NLIMT(NS-1)
         ELSE
            NP=NP+1
         END IF
         DVPADA=DVPADS(NV,NP)
         KOLD=K
         RETURN
      ELSE
         DO   700  N=1,NTOT
            IF(K.LE.NLIMT(N)) THEN
               NS=N
               CALL DVPADF(ISTRT,NS,JSECT,NHITS)
               NP=K-NLIMT(N-1)
               DVPADA=DVPADS(NV,NP)
               KOLD=K
               RETURN
            END IF
  700    CONTINUE
      END IF
      DVPADA=1.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVPAD0
CH
      ENTRY DVPAD0(ISECT,NHT)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVPAD0=0.
      CALL DVPAD1(NSECT)
      NHT=0
      DO   710  N=1,NSECT
         CALL DVPADN(ISTRT,N,JSECT,NHITS)
         NHT=NHT+NHITS
         NLIMT(N)=NHT
         NSECN(N)=JSECT
  710 CONTINUE
      NTOT=NSECT
      NHT0=NHT
      KOLD=-999
      IF(ISECT.NE.0) THEN
         FONE=.TRUE.
         DO   720  N=1,NSECT
            IF(ISECT.EQ.NSECN(N)) THEN
               CALL DVPADF(ISTRT,N,JSECT,NHT)
               RETURN
            END IF
  720    CONTINUE
         NHT=0
      ELSE
         FONE=.FALSE.
      END IF
      QTETDV=VRDZDV*FTDZDU
      END
*DK DVPAD1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVPAD1
CH
      SUBROUTINE DVPAD1(NSECT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
C     AUTHOR  .................  M.COMIN
C     INSTITUTE  ..............  M.P.I. IN MUNICH  or
C                                ISTITUTO DI FISICA NUCLEARE - TRIESTE
C     LAST UPDATING  ..........  26/11/87
C
C     PURPOSE:
C     THIS ROUTINE IDENTIFIES THE TPAD BANKS CONTAINING THE
C     SAMPLES MEASURED BY THE TPC SECTORS. VARIABLE NSECT
C     SPECIFIES THE NUMBER OF THE BANKS WHOSE STARTING ADDRESSES
C     ARE STORED INTO ARRAY KTPADV.
C
C
      INCLUDE 'A_BCS.INC'
C
      INTEGER ISECDV,KTPADV,NSECDV,MPDIMS
      REAL ZPADDV,RPADDV,FPADDV,TPADDV
      PARAMETER (MPDIMS=1500)
      COMMON/DVSECT/ISECDV(36),KTPADV(36),NSECDV,ZPADDV(MPDIMS),
     +  RPADDV(MPDIMS),FPADDV(MPDIMS),TPADDV(MPDIMS),BPADDV(MPDIMS)
C
      NSECT=0
      DO   700  JSECT=1,36
         IND=NLINK('TPAD',JSECT)
         IF(IND.NE.0) THEN
            NSECT=NSECT+1
            ISECDV(NSECT)=JSECT
            KTPADV(NSECT)=IND
         END IF
  700 CONTINUE
      NSECDV=NSECT
C
      RETURN
      END
C
*DK DVPADN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVPADN
CH
      SUBROUTINE DVPADN(ISTRT,I,JSECT,NHITS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
C
C     AUTHOR  .................  M.COMIN
C     INSTITUTE  ..............  M.P.I. IN MUNICH  or
C                                ISTITUTO DI FISICA NUCLEARE - TRIESTE
C     LAST UPDATING  ..........  26/11/87
C
C     PURPOSE:
C
C     OUTPUTS :
C     -- JSECT , SECTOR NUMBER (1-36)
C     -- NHITS , NUMBER OF DIGITIZATIONS
C
      INCLUDE 'A_BCS.INC'
      PARAMETER (MPDIMS=1500)
      COMMON/DVSECT/ISECDV(36),KTPADV(36),NSECDV,ZPADDV(MPDIMS),
     +  RPADDV(MPDIMS),FPADDV(MPDIMS),TPADDV(MPDIMS),BPADDV(MPDIMS)
C
      NHITS=0
      JSECT=ISECDV(I)
C
      IOFFS=KTPADV(I)
      MXOFF=IW(IOFFS)+IOFFS
C
 10   IF (IOFFS.LT.MXOFF)THEN
         NHITS=NHITS+IW(IOFFS+2)
         IOFFS=IOFFS+2+IW(IOFFS+2)
         GOTO 10
      END IF
C
      RETURN
      END
C
*DK DVPADF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVPADF
CH
      SUBROUTINE DVPADF(ISTRT,I,JSECT,NHITS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
C
C     AUTHOR  .................  M.COMIN
C     INSTITUTE  ..............  M.P.I. IN MUNICH  or
C                                ISTITUTO DI FISICA NUCLEARE - TRIESTE
C     LAST UPDATING  ..........  26/11/87
C
C     PURPOSE:
C     FOR EACH GIVEN SECTOR FILLS THE ARRAYS CONTAINING THE HITS
C     COORDINATES : X, Y, Z, RADIUS OF THE PADROW, THETA .
C
C     OUTPUTS :
C     -- JSECT , SECTOR NUMBER (1-36)
C     -- NHITS , NUMBER OF DIGITIZATIONS
C
C     ------------------------------------------------------------------
C        COMMON DVTGEO DEFINES THE GEOMETRIC PARAMETERS OF
C        THE TPC SECTORS :
C
C        --  RADIDV(J)      , RADIUS (in cm) AT PADROW J
C        --  PHI0DV(J)      , PHI LOCATION OF THE BISECANT OF SECTOR J
C        --  FROWDV(J,JROW) , PHI EXTENT OF PADROW JROW IN SECTOR J
C        --  NPADDV(J,JROW) , NUMB. OF PADS ALONG JROW OF SECTOR J
C        --  ZTPCDV         , HALF TPC SIZE ALONG ZED AXIS
C        --  TBINDV         , TIME LENGTH OF A SINGLE BUCKET (NSEC)
C        --  VELODV         , DRIFT VELOCITY (CM/NSEC)
C     ------------------------------------------------------------------
C
C
C      IMPLICIT NONE
C
      INCLUDE 'A_BCS.INC'
C
C      INTEGER ISECDV,KTPADV,NSECDV,MPDIMS
C      REAL ZPADDV,RPADDV,FPADDV,TPADDV
      PARAMETER (MPDIMS=1500)
      COMMON/DVSECT/ISECDV(36),KTPADV(36),NSECDV,ZPADDV(MPDIMS),
     +  RPADDV(MPDIMS),FPADDV(MPDIMS),TPADDV(MPDIMS),BPADDV(MPDIMS)
C
C      REAL RADIDV,PHI0DV,FROWDV,ZTPCDV,TBINDV,VELODV
C      INTEGER NPADDV
      COMMON/DVTGEO/ RADIDV(21),PHI0DV(18),FROWDV(18,12),NPADDV(18,12),
     +  ZTPCDV,TBINDV,VELODV
C
C      INTEGER JBUCK,JROW,JTIM0,JPAD,JSECT,LABEL,NHITS,JHIT,IOFFS,MXOFF,
C     +  NSAMP,LHEAD,ISTRT,I,JWORD,JSCT
C      REAL SIGN,FIPAD,FIROW,TPRAD,RADSQ,FILOC,ZPAD
C
C
C     ------------------------------------------------------------------
C                       DATA STATEMENTS SECTION
C     ------------------------------------------------------------------
C
      DATA LHEAD / 2 /
C_CG  DATA ZTPCDV,TBINDV,VELODV / 220., 100.0, .00505/
C
C_CG  DATA RADIDV / 39.871,46.271,52.671,59.071,65.471,71.871,
C_CG +  78.271,84.671,91.071,100.222,106.622,113.022,119.422,
C_CG +  125.822,132.222,138.622,145.022,151.422,157.822,164.222,
C_CG +  170.622 /
C
C_CG  DATA FROWDV / 6*0.495883, 12*0.250771, 6*0.499716, 12*0.251433,
C_CG +  6*0.502617, 12*0.252020, 6*0.504890, 12*0.252544, 6*0.506719,
C_CG +  12*0.253015, 6*0.508222, 12*0.253440, 6*0.509479, 12*0.253826,
C_CG +  6*0.510546, 12*0.254178, 6*0.511463, 0.294335, 0.214665,
C_CG +  0.294335, 0.214665, 0.294335, 0.214665, 0.294335, 0.214665,
C_CG +  0.294335, 0.214665, 0.294335, 0.214665, 6*0.0, 0.293016,
C_CG +  0.216577, 0.293016, 0.216577, 0.293016, 0.216577,0.293016,
C_CG +  0.216577, 0.293016, 0.216577, 0.293016, 0.216577, 6*0.0,
C_CG +  0.291799, 0.218339, 0.291799, 0.218339, 0.291799, 0.218339,
C_CG +  0.291799, 0.218339, 0.291799, 0.218339, 0.291799, 0.218339,
C_CG +  6*0.0, 0.290674, 0.219969, 0.290674, 0.219969, 0.290674,
C_CG +  0.219969, 0.290674, 0.219969, 0.290674, 0.219969, 0.290674,
C_CG +  0.219969 /
C
C_CG  DATA NPADDV / 6*59, 12*75, 6*69, 12*80, 6*79, 12*85, 6*89, 12*90,
C_CG +  6*99, 12*95, 6*109, 12*100, 6*119, 12*105, 6*129, 12*110, 6*139,
C_CG +  133, 97, 133, 97, 133, 97, 133, 97, 133, 97, 133, 97, 6*0, 138,
C_CG +  102, 138,102, 138,102, 138,102, 138,102, 138,102, 6*0, 143,107,
C_CG +  143,107, 143,107, 143,107, 143,107, 143,107, 6*0, 148,112,
C_CG +  148,112, 148,112, 148,112, 148,112, 148,112 /
C
C--     THE SECTOR LOCATION ON THE END PLATE IS GIVEN BY THE ANGLE
C--     BETWEEN THE X AXIS AND THE BISECANT AXIS OF THE SECTOR.
C
C_CG  DATA PHI0DV / 0.523599, 1.570796, 2.617993, 3.665191, 4.712388,
C_CG +  5.759585, 0.0, 0.523599, 1.047197, 1.570796, 2.094395, 2.617993,
C_CG +  3.141592, 3.665191, 4.188789, 4.712388, 5.2355987, 5.759585 /
      DATA PIDEG/57.29577951/
      DATA RMAX,ZMAX / 170.622, 217. /
C
C
C---  INITIALIZATION OF GLOBAE L VARIABLES
C
      SIGN=1.
      LABEL=0
      NHITS=0
C
      JSECT=ISECDV(I)
      JSCT=JSECT
      IF(JSECT.GT.18) THEN
         SIGN=-1.
         JSCT=JSCT-18
      END IF
      IF(JSCT.GT.6) LABEL=9
C
      IOFFS=KTPADV(I)
      MXOFF=IW(IOFFS)+IOFFS
C
 20   IF (IOFFS.LT.MXOFF)THEN
C
C---      READ ROW HEADER AND CALCULATE THE VARIABLES WHOSE VALUES
C---      DEPEND ON THE ROW NUMBER ONLY.
C
         JROW=IBITS(IW(IOFFS+1),0,4)
         TPRAD=RADIDV(JROW+LABEL)
         RADSQ=TPRAD**2
         NSAMP=IW(IOFFS+2)
         FIROW=FROWDV(JSCT,JROW)
         FIPAD=2.0*FIROW/FLOAT(NPADDV(JSCT,JROW))
         FILOC=PHI0DV(JSCT)-SIGN*FIROW
C
C---      LOOP OVER THE HITS MEASURED ALONG PADROW JROW
C
         DO   700  JHIT=1,NSAMP
            NHITS=NHITS+1
            JWORD=IW(IOFFS+LHEAD+JHIT)
            JPAD  = IBITS(JWORD,24,8)
            JTIM0 = IBITS(JWORD,0,9)
            JBUCK = IBITS(JWORD,16,8)
C
C---         CALCULATION OF SPATIAL COORDINATES
C
            FPADDV(NHITS) =(FILOC + SIGN*(FLOAT(JPAD)-0.5)*FIPAD)*PIDEG
            ZPAD = ZTPCDV - FLOAT(JTIM0)*TBINDV*VELODV
CCCCCCCC            IF(ZPAD.LT.0.) ZPAD=0.
            ZPAD = -SIGN*ZPAD
CZZZZZZZZZZZZZ SIGN OF Z HAS BEEN CHANGED
            ZPADDV(NHITS) = -ZPAD
            RPADDV(NHITS) = TPRAD
            TPADDV(NHITS) = ACOS(-ZPAD/SQRT(ZPAD**2+RADSQ))*PIDEG
            ZA=ABS(ZPAD)
            D0=SQRT(ZPAD**2+TPRAD**2)
            IF(TPRAD*ZMAX.GT.ZA*RMAX) THEN
               BPADDV(NHITS)=D0*(RMAX/TPRAD-1.)
            ELSE
               BPADDV(NHITS)=D0*(ZMAX/ZA-1.)
            END IF
  700    CONTINUE
         IOFFS=IOFFS+LHEAD+NSAMP
         GOTO 20
      END IF
C
      RETURN
      END
C
*DK DVPADS
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVPADS
CH
      FUNCTION DVPADS(NSEL,JHIT)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------

C
C     AUTHOR  .................  M.COMIN
C     INSTITUTE  ..............  M.P.I. IN MUNICH  or
C                                ISTITUTO DI FISICA NUCLEARE - TRIESTE
C     LAST UPDATING  ..........  26/11/87
C
C
C     PURPOSE :
C     RETURNS BACK THE VALUE OF THE QUANTITY DEFINED BY INDEX NSEL
C     INDEX JHIT REFERS TO THE HIT POSITION IN THE ARRAYS FILLED BY
C     ROUTINE TPOINT
C
C      IMPLICIT NONE
C
      INCLUDE 'DALI_CF.INC'
      INTEGER ISECDV,KTPADV,NSECDV,MPDIMS
      DATA PIDEG/57.29577951/
      REAL ZPADDV,RPADDV,FPADDV,TPADDV
      PARAMETER (MPDIMS=1500)
      COMMON/DVSECT/ISECDV(36),KTPADV(36),NSECDV,ZPADDV(MPDIMS),
     +  RPADDV(MPDIMS),FPADDV(MPDIMS),TPADDV(MPDIMS),BPADDV(MPDIMS)
      REAL DVPADS
      INTEGER JHIT,NSEL
C
      GOTO(1,2,3,4,5,6,7,8,99) NSEL
   99 DVPADS=0.
      RETURN
C----------------------------------------------- >  X COORDINATE
    1 DVPADS = RPADDV(JHIT)*COSD(FPADDV(JHIT))
      RETURN
C----------------------------------------------- >  Y COORDINATE
    2 DVPADS = RPADDV(JHIT)*SIND(FPADDV(JHIT))
      RETURN
C-------------------------------------------- >  Z COORDINATE
    3 DVPADS = ZPADDV(JHIT)
      RETURN
C----------------------------------------------- >  RHO COORDINATE
    4 DVPADS = RPADDV(JHIT)
      RETURN
C----------------------------------------------- >  PHI COORDINATE
    5 DVPADS = FPADDV(JHIT)
      RETURN
C----------------------------------------------- >  THETA COORDINATE
    6 IF(VRDZDV.EQ.0.) THEN
        DVPADS = TPADDV(JHIT)
      ELSE
        T0=ATAN2(RPADDV(JHIT),ZPADDV(JHIT))-VRDZDV
        TEDZ=T0*PIDEG
        IF(TEDZ.LE.90.) THEN
          DVPADS=TEDZ-QTETDV*TEDZ
        ELSE
          DVPADS=TEDZ-QTETDV*(180.-TEDZ)
        END IF
      END IF
      RETURN
C----------------------------------------------- >  DI COORDINATE
    7 DVPADS = SQRT(RPADDV(JHIT)**2 + ZPADDV(JHIT)**2)
      RETURN
C----------------------------------------------- >  B COORDINATE
    8 DVPADS = BPADDV(JHIT)
      RETURN
      END
*DK DVPARTB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVPART
CH
      SUBROUTINE DVPARTB(ITYPE,CH,FOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      LOGICAL FOUT
      FOUT=.FALSE.
      IF(ITYPE.LE.16) THEN
        N=ITYPE
      ELSE IF(ITYPE.EQ.25) THEN
        N=17
      ELSE IF(ITYPE.EQ.47) THEN
        N=47
      ELSE
        FOUT=.TRUE.
      END IF
      END
*DK DVVTX0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVVTX0
CH
      SUBROUTINE DVVTX0(NVX)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION PXYZ(3)
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      KPYER = IW(NAMIND('PYER'))
      IF (KPYER.EQ.0) THEN
        NVX=0
      ELSE
        NVX=LROWS(KPYER)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVVTX
CH
      ENTRY DVVTX(NV,NTYPE,PXYZ)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      NTYPE  =ITABL(KPYER,NV,1)
      PXYZ(1)=RTABL(KPYER,NV,2)
      PXYZ(2)=RTABL(KPYER,NV,3)
      PXYZ(3)=RTABL(KPYER,NV,4)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------  DVVTXD
CH
      ENTRY DVVTXD(NV,NDGFR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      NDGFR=ITABL(KPYER,NV,12)
      END
*DK DVPRVX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVPRVX
CH
      SUBROUTINE DVPRVX(NVX)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      NVX=0
      VRTXDV=0.
      VRTYDV=0.
      VRTZDV=0.
      VRDZDV=0.
      KPYER = IW(NAMIND('PYER'))
      IF (KPYER.NE.0) THEN
         NVX=LROWS(KPYER)
         IF(NVX.EQ.0) RETURN
         DO 10 J=1,NVX
            IF (ITABL(KPYER,J,1).EQ.1) THEN
              VRTXDV=RTABL(KPYER,J,2)
              VRTYDV=RTABL(KPYER,J,3)
              VRTZDV=RTABL(KPYER,J,4)
              IF(ABS(VRTZDV).LE.VRDZDU) THEN
                VRDZDV=VRTZDV
              END IF
              RETURN
            END IF
   10    CONTINUE
      ENDIF
      END
*DK DVSIC
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVSIC
CH
      FUNCTION DVSIC(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C     INCLUDE 'SPDAJJ.INC'
C     PARAMETER (JSPDEN=1,JSPDAD=2,JSPDOV=3,JSPDSC=4,JSPDSM=5)
      PARAMETER (JSPDEN=1,JSPDAD=2,JSPDOV=4,JSPDSC=5,JSPDSM=6)
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      IF(K.NE.KOLD) THEN
        IAD=ITABL(ISPDA,K,JSPDAD)
        KOLD=K
      END IF
C             1  2  3  4  5  6  7  8  9  0
      GO TO ( 1, 2, 3, 4, 5, 6,99,99,99,99,
     &       99,12,13,14,15,99,99,99,99,99),NV
C-------------------------------------------------------------  UNDEFINED
   99 DVSIC=0.
      RETURN
C-------------------------------------------------------------------- X
    1 DVSIC=SIDALI(1,IAD)*COSD(SIDALI(2,IAD))
      RETURN
C-------------------------------------------------------------------- Y
    2 DVSIC=SIDALI(1,IAD)*SIND(SIDALI(2,IAD))
      RETURN
C-------------------------------------------------------------------- Z
    3 DVSIC=SIDALI(3,IAD)
      RETURN
C-------------------------------------------------------------------- RHO
    4 DVSIC=SIDALI(1,IAD)
      RETURN
C-------------------------------------------------------------------- PHI
    5 DVSIC=SIDALI(2,IAD)
      RETURN
C-------------------------------------------------------------------- THETA
    6 DVSIC=DATN2D(SIDALI(1,IAD),SIDALI(3,IAD))
      RETURN
C--------------------------------------------------------------------  E
   12 DVSIC=RTABL(ISPDA,K,JSPDEN)
      RETURN
C--------------------------------------------------------------------  E
   13 DVSIC=ITABL(ISPDA,K,JSPDSC)
      RETURN
C-------------------------------------------------------------------- IAD
   14 DVSIC=IAD
      RETURN
C-------------------------------------------------------------------- DEAD
C     DEAD#0   GOOD=0
   15 DVSIC=ITABL(ISPDA,K,JSPDSM)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVSIC0
CH
      ENTRY DVSIC0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVSIC0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      ISPDA = IW(NAMIND('SPDA'))
      IF(ISPDA.EQ.0) RETURN
      NUM=LROWS(ISPDA)
C     ............................................. DF,DR,DZ from side to side
C     CALL DGRSIC(DF,DR,DZ)
      KOLD=0
      END
*DK DVTP
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVTP
CH
      FUNCTION DVTP(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                               C
C This test program was used to investigate the theta           C
C correction for dz of the vertex, as ecal and hcal cannot use  C
C a dz correction due to the vertex position.                   C
C                                                               C
C      TEcor = TE(DZ)-QTETDV*T90                                C
C                            T90 = TE or 180.-TE                C
C                     QTETDV=DZ*FTDZDU                          C
C                               FTDZDU is set via GT:PM:FZ=...  C
C                                                               C
C      DATA ZM/259.3/,RM/192./,DR/30./,DZZ/40./,Q/0.0033/       C
C    1 ACCEPT 1000,DZ                                           C
C 1000 FORMAT(F4.0)                                             C
C      QS=0.                                                    C
C      NS=0                                                     C
C      DO Z=0.,ZM,40.                                           C
C        T1=ATAN2D(RM,Z)                                        C
C        T2=ATAN2D(RM,(Z+DZ))                                   C
C        DT2=T1-T2                                              C
C        QT=DT2/T1                                              C
C        NS=NS+1                                                C
C        QS=QS+QT                                               C
C        DT4=FTDZDU*T1*DZ                                       C
C        T5=T1-DT4                                              C
C        DT5=T2-T5                                              C
C        TYPE 1001,Z,T1,T2,DT2,QT,DT4,T5,DT5                    C
C 1001   FORMAT(4F8.2,2X,F8.6,3F8.2)                            C
C      END DO                                                   C
C      DO R=RM,20.,-40.                                         C
C        T1=ATAN2D(R,ZM)                                        C
C        T2=ATAN2D(R,(ZM+DZ))                                   C
C        DT2=T1-T2                                              C
C        QT=DT2/T1                                              C
C        NS=NS+1                                                C
C        QS=QS+QT                                               C
C        DT4=FTDZDU*T1*DZ                                       C
C        T5=T1-DT4                                              C
C        DT5=T2-T5                                              C
C        TYPE 1001,R,T1,T2,DT2,QT,DT4,T5,DT5                    C
C      END DO                                                   C
C      QS=QS/NS                                                 C
C      TYPE *,QS,'  ',NS                                        C
C      GO TO 1                                                  C
C      END                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'DALI_CF.INC'
      LOGICAL FPAD
      DATA ZMAX/217./,RMAX/170.622/
      DATA PIDEG/57.29577951/
      DIMENSION NSTOR(9)
      INCLUDE 'J_TPNAMC.INC'
      INCLUDE 'A_TPCOJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      IF(FPAD) THEN
         DVTP=DVPADA(NV,K)
         RETURN
      END IF
      GO TO(1,2,3,4,5,6,7,8,99,99,99,99,99,99,99,99,99,18,99,99),NV
C--------------------------------------------------------------  UNDEFIN
   99 DVTP=0.
      RETURN
C----------------------------------------------------------------------
    1 IF(KX.NE.K) THEN
         X=COS(RTABL(ITPCO,K,JTPCPH))*RTABL(ITPCO,K,JTPCRV)
         KX=K
      END IF
      DVTP=X
      RETURN
C----------------------------------------------------------------------
    2 IF(KY.NE.K) THEN
         Y=SIN(RTABL(ITPCO,K,JTPCPH))*RTABL(ITPCO,K,JTPCRV)
         KY=K
      END IF
      DVTP=Y
      RETURN
C----------------------------------------------------------------------
    3 Z=RTABL(ITPCO,K,JTPCZV)
      IF(IPT0.GT.0.) Z=Z+SIGN(1.,Z)*PT0 
      DVTP=Z
      RETURN
C----------------------------------------------------------------------
    4 DVTP=RTABL(ITPCO,K,JTPCRV)
      RETURN
C----------------------------------------------------------------------
    5 DVTP=RTABL(ITPCO,K,JTPCPH)*PIDEG
      RETURN
C----------------------------------------------------------------------
    6 Z=RTABL(ITPCO,K,JTPCZV)-VRDZDV
      IF(K.NE.KT) THEN
         T0=ATAN2(RTABL(ITPCO,K,JTPCRV),Z)
         KT=K
      END IF
      TEDZ=T0*PIDEG
      IF(TEDZ.LE.90.) THEN
        DVTP=TEDZ-QTETDV*TEDZ
      ELSE
        DVTP=TEDZ-QTETDV*(180.-TEDZ)
      END IF
      RETURN
C----------------------------------------------------------------------
    7 DVTP=SQRT(RTABL(ITPCO,K,JTPCRV)**2+(RTABL(ITPCO,K,JTPCZV))**2)
      RETURN
C----------------------------------------------------------------------
    8 IF(K.NE.KB) THEN
         Z0=RTABL(ITPCO,K,JTPCZV)
         R0=RTABL(ITPCO,K,JTPCRV)
         ZA=ABS(Z0)
         D0=SQRT(Z0**2+R0**2)
         IF(R0*ZMAX.GT.ZA*RMAX) THEN
            B=D0*(RMAX/R0-1.)
         ELSE
            B=D0*(ZMAX/ZA-1.)
         END IF
         KB=K
      END IF
      DVTP=B
      RETURN
C----------------------------------------------------------------------
   18 DVTP=ITABL(ITPCO,K,JTPCTN)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTPNT
CH
      ENTRY DVTPNT(K,NT)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVTPNT=0
      IF(K.LE.MTPCDV.AND.NSTOR(9).EQ.1) THEN
        NT=NTPCDV(9,K)
      ELSE
        NT=ITABL(ITPCO,K,JTPCTN)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTPCL
CH
      ENTRY DVTPCL(NV)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVTPCL=0
      IF(NV.EQ.0) THEN
         CALL VZERO(NSTOR,9)
      ELSE
         IV=NV
         IF(NV.EQ.18) IV=9
         NSTOR(IV)=0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTPS1
CH
      ENTRY DVTPS1(DUMMY)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
      DVTPS1=0
      CALL UFILL(NSTOR,1,9,1)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTPST
CH
      ENTRY DVTPST(NV,JV)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVTPST=0
      JV=NV
      IF(JV.EQ.18) JV=9
      IF(NSTOR(JV).EQ.1) RETURN
      NSTOR(JV)=1
C
      IF(IW(30).NE.12345) RETURN
      ITPCO=IW(NAMIND('TPCO'))
      IF(ITPCO.EQ.0) RETURN
C
      GO TO(101,102,103,104,105,106,107,108,109),JV
C----------------------------------------------------------------------
  101 DO 701 J=1,NUMST
         VTPCDV(JV,J)=COS(RTABL(ITPCO,J,JTPCPH))*RTABL(ITPCO,J,JTPCRV)
  701 CONTINUE
      RETURN
C----------------------------------------------------------------------
  102 DO 702 J=1,NUMST
         VTPCDV(JV,J)=SIN(RTABL(ITPCO,J,JTPCPH))*RTABL(ITPCO,J,JTPCRV)
  702 CONTINUE
      RETURN
C----------------------------------------------------------------------
  103 DO 703 J=1,NUMST
        VTPCDV(JV,J)=RTABL(ITPCO,J,JTPCZV)
  703 CONTINUE
      RETURN
C----------------------------------------------------------------------
  104 DO 704 J=1,NUMST
         VTPCDV(JV,J)=RTABL(ITPCO,J,JTPCRV)
  704 CONTINUE
      RETURN
C----------------------------------------------------------------------
  105 DO 705 J=1,NUMST
         VTPCDV(JV,J)=RTABL(ITPCO,J,JTPCPH)*PIDEG
  705 CONTINUE
      RETURN
C----------------------------------------------------------------------
  106 DO 706 J=1,NUMST
         Z=RTABL(ITPCO,J,JTPCZV)-VRDZDV
         TEDZ=ATAN2(RTABL(ITPCO,J,JTPCRV),Z)*PIDEG
         IF(TEDZ.LE.90.) THEN
           VTPCDV(JV,J)=TEDZ-QTETDV*TEDZ
         ELSE
           VTPCDV(JV,J)=TEDZ-QTETDV*(180.-TEDZ)
         END IF
  706 CONTINUE
      RETURN
C----------------------------------------------------------------------E J
  107 DO 707 J=1,NUMST
         Z0=RTABL(ITPCO,J,JTPCZV)
         R0=RTABL(ITPCO,J,JTPCRV)
         VTPCDV(JV,J)=SQRT(Z0*Z0+R0*R0)
  707 CONTINUE
      RETURN
C----------------------------------------------------------------------
  108 DO 708 J=1,NUMST
         Z0=RTABL(ITPCO,J,JTPCZV)
         R0=RTABL(ITPCO,J,JTPCRV)
         ZA=ABS(Z0)
         D0=SQRT(Z0*Z0+R0*R0)
         IF(R0*ZMAX.GT.ZA*RMAX) THEN
            VTPCDV(JV,J)=D0*(RMAX/R0-1.)
         ELSE
            VTPCDV(JV,J)=D0*(ZMAX/ZA-1.)
         END IF
  708 CONTINUE
      RETURN
C----------------------------------------------------------------------
  109 DO 709 J=1,NUMST
         NTPCDV(JV,J)=ITABL(ITPCO,J,JTPCTN)
  709 CONTINUE
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTP0
CH
      ENTRY DVTP0(IHT6,SECT,NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C       USED ONLY IN DVMDUL FOR LASER TRACKS
      DVTP0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IF(IHT6.EQ.2) THEN
         NSECT=SECT
         CALL=DVPAD0(NSEC,NN)
         FPAD=.TRUE.
      ELSE
         IF(IHT6.EQ.3) THEN
            ITPCO=NLINK('TBCO',0)
         ELSE
            ITPCO=NLINK('TPCO',0)
         END IF
         IF(ITPCO.EQ.0)THEN
            NUM=0
            RETURN
         END IF
         NUM=LROWS(ITPCO)
         FPAD=.FALSE.
      END IF
      KX=0
      KY=0
      KR=0
      KT=0
      KB=0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTQ0
CH
      ENTRY DVTQ0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVTQ0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      ITPCO=IW(NAMIND('TPCO'))
      IF(ITPCO.EQ.0) RETURN
      NUM=LROWS(ITPCO)
      NUMST=MIN(NUM,MTPCDV)
      FPAD=.FALSE.
      KX=0
      KY=0
      KR=0
      KT=0
      KB=0
      QTETDV=VRDZDV*FTDZDU
      CALL DPARGV_24(14,'PT0',PT0,IPT0)
      IF(IPT0.GT.0) SLOWDU=1.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTB0
CH
      ENTRY DVTB0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVTB0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      ITPCO=IW(NAMIND('TBCO'))
      IF(ITPCO.EQ.0) RETURN
      NUM=LROWS(ITPCO)
      FPAD=.FALSE.
      KX=0
      KY=0
      KR=0
      KT=0
      KB=0
      END
*DK DVTC0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVTC0
CH
      SUBROUTINE DVTC0(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DATA PIDEG/57.29577951/
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      CHARACTER *(*) T
      INCLUDE 'A_BCS.INC'
      PARAMETER (BFIELD=15.0,CLGHT=29.9792458)
C
      INCLUDE 'A_FRFTJJ.INC'
C
      INCLUDE 'A_BMACRO.INC'
C
      NUM=0
      IF(IW(30).EQ.12345) THEN
        JCRFT = NLINK('CRFT',0)
        IF(JCRFT.NE.0) NUM = LROWS(JCRFT)
      END IF
      BNUMDB(2,CRFTDB)=NUM
      BNUMDB(3,CRFTDB)=NUM
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTCF
CH
      ENTRY DVTCF(N,NSVX)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C       CONVERSION OF JULIA CONVENTIONS TO OLD TASSO CONVENTIONS
C
      TPHEDT(1) = -RTABL(JCRFT,N,JFRFIR)
      TPHEDT(2) =  RTABL(JCRFT,N,JFRFTL)
      TPHEDT(3) =  RTABL(JCRFT,N,JFRFP0)
      TPHEDT(4) = -RTABL(JCRFT,N,JFRFD0)*SIGN(1.,TPHEDT(1))
      TPHEDT(5) =  RTABL(JCRFT,N,JFRFZ0)
C
      FRFTDT(1)=RTABL(JCRFT,N,JFRFIR)
      FRFTDT(2)=RTABL(JCRFT,N,JFRFTL)
      FRFTDT(3)=RTABL(JCRFT,N,JFRFP0)
      FRFTDT(4)=RTABL(JCRFT,N,JFRFD0)
      FRFTDT(5)=RTABL(JCRFT,N,JFRFZ0)
      FRFTDT(6)=RTABL(JCRFT,N,JFRFAL)
      NSVX   =  ITABL(JCRFT,N,6)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTCP
CH
      ENTRY DVTCP(N,PX,PY,PZ,IC)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
      AIR = -RTABL(JCRFT,N,JFRFIR)
      IC = SIGN(1.,AIR)
      TL = RTABL(JCRFT,N,JFRFTL)
      P0 = RTABL(JCRFT,N,JFRFP0)
      D0 = -RTABL(JCRFT,N,JFRFD0)*SIGN(1.,AIR)
      Z0 = RTABL(JCRFT,N,JFRFZ0)
      RHO = 1./ABS(AIR)
      PT = BFIELD*RHO*CLGHT/100000.
      THETA = ATAN2(1.,TL)
      CPHI = COS(P0)
      SPHI = SIN(P0)
      CTHET = COS(THETA)
      STHET = SIN(THETA)
      DRCSX = STHET*CPHI
      DRCSY = STHET*SPHI
      DRCSZ = CTHET
      PTOT = PT/STHET
      PX=PTOT*DRCSX
      PY=PTOT*DRCSY
      PZ=PTOT*DRCSZ
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTCT1
CH
      ENTRY DVTCT1(NTRKT,T)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(NTRKT.LE.0) THEN
        T='   #    P   dP  phi theta  D0   Z0 frV toV'
        RETURN
      END IF
      AIR = -RTABL(JCRFT,NTRKT,JFRFIR)
      IF(AIR.EQ.0.) RETURN
      TL = RTABL(JCRFT,NTRKT,JFRFTL)
      RHO = ABS(1./AIR)
      PT = BFIELD*RHO*CLGHT/100000.
      TET = ATAN2(1.,TL)
      STHET = SIN(TET)
      PTOTT = PT/STHET
      FIT=RTABL(JCRFT,NTRKT,JFRFP0)
      D0T=RTABL(JCRFT,NTRKT,JFRFD0)
      Z0T=RTABL(JCRFT,NTRKT,JFRFZ0)
      CI2=RTABL(JCRFT,NTRKT,JFRFC2)
      NVX=ITABL(JCRFT,NTRKT,6)
      CHT=SIGN(1.,AIR)
      DIC = SQRT(MAX(0.,RTABL(JCRFT,NTRKT,7)))
      DTL = SQRT(MAX(0.,RTABL(JCRFT,NTRKT,9)))
C
C Error in transverse momentum:
      DPT = ABS(PTOTT*RHO*DIC)
C
C Error in total momentum:
      DP = (DPT*DPT) + (DTL*PTOTT*TL/(1+TL*TL))**2
      DP = SQRT(MAX(0.,DP))
C
C        123456789 123456789 123456789 123456789 1
C     T='   #    P   dP  phi theta  D0   Z0 VX
      T=' *** -23.5 1.2 1.34 1.34 12.4 -99. 12'
      WRITE(T(2:4),1000,ERR=2) NTRKT
 1000 FORMAT(I3)
    2 IF(CHT.GT.0.) T(6:6)='+'
      T( 7:10)=DT4(PTOTT)
      T(12:14)=DT3(DP)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_LRG'
      CALL DPARAM(33
     &  ,J_LRG)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_LRG).EQ.0.) THEN
        WRITE(T(16:19),1000,ERR=3) IFIX(PIDEG*FIT)
    3   WRITE(T(21:24),1000,ERR=5) IFIX(PIDEG*TET)
      ELSE
        WRITE(T(16:19),1001,ERR=4) FIT
    4   WRITE(T(21:24),1001,ERR=5) TET
 1001   FORMAT(F4.2)
      END IF
    5 T(26:29)=DT4(D0T)
      T(31:34)=DT4(Z0T)
      WRITE(T(35:37),1000) NVX
      END
*DK DVTN0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVTN0
CH
      SUBROUTINE DVTN0(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DATA PIDEG/57.29577951/
      CHARACTER *4 DT4
      CHARACTER *5 DT5
      CHARACTER *(*) T
      INCLUDE 'A_BCS.INC'
      PARAMETER (BFIELD=15.0,CLGHT=29.9792458)
C
      INCLUDE 'A_FRFTJJ.INC'
C
      INCLUDE 'A_BMACRO.INC'
C
      NUM=0
      IF(IW(30).EQ.12345) THEN
        JNRFT = NLINK('NRFT',0)
        IF(JNRFT.NE.0) NUM = LROWS(JNRFT)
      END IF
      BNUMDB(2,NRFTDB)=NUM
      BNUMDB(3,NRFTDB)=NUM
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTNF
CH
      ENTRY DVTNF(N,NSVX)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      NSVX=ITABL(JNRFT,N,6)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTNP
CH
      ENTRY DVTNP(N,PX,PY,PZ)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
      PTOT= RTABL(JNRFT,N,JFRFIR)
      TL  = RTABL(JNRFT,N,JFRFTL)
      P0  = RTABL(JNRFT,N,JFRFP0)
      THETA = ATAN2(1.,TL)
      CPHI  = COS(P0)
      SPHI  = SIN(P0)
      CTHET = COS(THETA)
      STHET = SIN(THETA)
      PX=PTOT*STHET*CPHI
      PY=PTOT*STHET*SPHI
      PZ=PTOT*CTHET
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTNT1
CH
      ENTRY DVTNT1(NTRKT,T)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(NTRKT.LE.0) THEN
        T='   #    P   dP  phi theta  D0   Z0 chiq'
        RETURN
      END IF
      PTOT=RTABL(JNRFT,NTRKT,JFRFIR)
      TL  =RTABL(JNRFT,NTRKT,JFRFTL)
      FIT =RTABL(JNRFT,NTRKT,JFRFP0)
      D0T =RTABL(JNRFT,NTRKT,JFRFD0)
      Z0T =RTABL(JNRFT,NTRKT,JFRFZ0)
      NVX =ITABL(JNRFT,NTRKT,6)
      TET = ATAN2(1.,TL)
C
C        123456789 123456789 123456789 123456789 1
C     T='   #    P   dP  phi theta  D0   Z0 chiq
      T=' ***  23.5 1.2 1.34 1.34 12.4 -99.0 1.3'
      WRITE(T(2:4),1000,ERR=2) NTRKT
 1000 FORMAT(I3)
    2 T( 7:10)=DT4(PTOT)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_LRG'
      CALL DPARAM(33
     &  ,J_LRG)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_LRG).EQ.0.) THEN
        IFIT=MOD(PIDEG*FIT+3600.,360.)
        ITET=MOD(PIDEG*TET+3600.,360.)
        WRITE(T(16:19),1000,ERR=3) IFIT
    3   WRITE(T(21:24),1000,ERR=5) ITET
      ELSE
        WRITE(T(16:19),1001,ERR=4) FIT
    4   WRITE(T(21:24),1001,ERR=5) TET
 1001   FORMAT(F4.2)
      END IF
    5 T(26:29)=DT4(D0T)
      T(31:35)=DT5(Z0T)
      WRITE(T(35:37),1000) NVX
      END
*DK DVTR0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVTR0
CH
      SUBROUTINE DVTR0(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION NUBA(4)
      DATA PIDEG/57.29577951/
      CHARACTER *2 DT2
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      CHARACTER *5 DT5
      CHARACTER *(*) T
      LOGICAL FTPC
      INCLUDE 'A_BCS.INC'
      PARAMETER (BFIELD=15.0,CLGHT=29.9792458)
C
      INCLUDE 'A_FRFTJJ.INC'
C
      INCLUDE 'A_BMACRO.INC'
C
      NUM=0
      IF(IW(30).NE.12345) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HTB'
      CALL DPARAM(20
     &  ,J_HTB)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DVLBKN('FRFT',4,NUBA,NUMB)
      IF(NUMB.GT.0) THEN
        NTB=PARADA(2,J_HTB)
        DO K=1,NUMB
          IF(NTB.EQ.NUBA(K)) GO TO 1
        END DO
        WRITE(TXTADW,2000) NTB,NUBA
C       ....... 123456789 123456789 123456789 123456789 123456789
C       ....... Non existent bank FRFT 0 set to 1; others = 2 3 4
 2000   FORMAT('Non existent bank FRFT',I2,' set to',I2,'; others =',
     &    3I2,'#')
        CALL DWRC
        NTB=NUBA(1)
        PARADA(2,J_HTB)=NTB
    1   JFRFT = NLINK('FRFT',NTB)
        IF(JFRFT.GT.0) THEN
          PARADA(4,J_HTB)=1.
          NUM = LROWS(JFRFT)
          IF(NUM.GT.MTRKDT) THEN
            IF(NUML.LT.MTRKDT) 
     &        CALL DWRT('Too many tracks! # of tracks set to 100.#')
            NUM=MTRKDT
          END IF
          NUML=NUM
        END IF
      END IF
      RETURN
C
C    1 IF(PARADA(4,J_HTB).NE.1.) THEN
C        PTB=0.
C      ELSE
C        PTB=PARADA(2,J_HTB)
C      END IF
C        IF(PTB.NE.0.) THEN
C          JFRFT = NLINK('FRFT',0)
C          IF(JFRFT.GT.0) THEN
C            CALL DWRT('Non existent bank FRFT # '//DT2(PTB)//
C     &        ' # SET TO 0')
C            PARADA(2,J_HTB)=0.
C            PARADA(4,J_HTB)=1.
C          END IF
C        ELSE
C          JFRFT = NLINK('FRFT',2)
C          IF(JFRFT.GT.0) THEN
C            CALL DWRT('Non existent bank FRFT 0 SET TO 2')
C            PARADA(2,J_HTB)=2.
C            PARADA(4,J_HTB)=1.
C          END IF
C        END IF
C      END IF
C      IF(JFRFT.EQ.0) THEN
C        NUM=0
C      ELSE
C        NUM = LROWS(JFRFT)
C      END IF
C      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRFI
CH
      ENTRY DVTRFI(NTRKF,FIF)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
      FIF=RTABL(JFRFT,NTRKF,JFRFP0)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRLP
CH
      ENTRY DVTRLP(NTRKL,NLAST,FTPC)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      NLAST=0
      IF(NTRKL.LE.BNUMDB(2,FTCLDB)) THEN
        CALL=DVCHT(NTRKL,LASTC)
        IF(LASTC.GT.0) THEN
          NLAST=IDVCHT(LASTC)
          FTPC=.TRUE.
          RETURN
        END IF
      END IF
      FTPC=.FALSE.
      IF(NLAST.EQ.0) THEN
        IF(NTRKL.LE.BNUMDB(2,FICLDB)) THEN
          CALL=DVCHI(NTRKL,LASTC)
          IF(LASTC.GT.0) THEN
            NLAST=IDVCHI(LASTC)
            RETURN
          END IF
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRSX
CH
      ENTRY DVTRSX(NSX,WSX,TLSX,FISX,D0SX,Z0SX,ASX)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
      WSX =RTABL(JFRFT,NSX,JFRFIR)
      TLSX=RTABL(JFRFT,NSX,JFRFTL)
      FISX=RTABL(JFRFT,NSX,JFRFP0)
      D0SX=RTABL(JFRFT,NSX,JFRFD0)
      Z0SX=RTABL(JFRFT,NSX,JFRFZ0)
      ASX =RTABL(JFRFT,NSX,JFRFAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRP
CH
      ENTRY DVTRP(N,PX,PY,PZ,IC)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C
      AIR = -RTABL(JFRFT,N,JFRFIR)
      IC = SIGN(1.,AIR)
      TL = RTABL(JFRFT,N,JFRFTL)
      P0 = RTABL(JFRFT,N,JFRFP0)
      D0 = -RTABL(JFRFT,N,JFRFD0)*SIGN(1.,AIR)
      Z0 = RTABL(JFRFT,N,JFRFZ0)
      RHO = 1./ABS(AIR)
      PT = BFIELD*RHO*CLGHT/100000.
      THETA = ATAN2(1.,TL)
      CPHI = COS(P0)
      SPHI = SIN(P0)
      CTHET = COS(THETA)
      STHET = SIN(THETA)
      DRCSX = STHET*CPHI
      DRCSY = STHET*SPHI
      DRCSZ = CTHET
      PTOT = PT/STHET
      PX=PTOT*DRCSX
      PY=PTOT*DRCSY
      PZ=PTOT*DRCSZ
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRTP
CH
      ENTRY DVTRTP(NTRKP,PTOT1)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      AIR = -RTABL(JFRFT,NTRKP,JFRFIR)
      IF(AIR.EQ.0.) THEN
        PTOT1=0.
        RETURN
      END IF
      TL = RTABL(JFRFT,NTRKP,JFRFTL)
      RHO = 1./AIR
      PT = BFIELD*RHO*CLGHT/100000.
      THETA = ATAN2(1.,TL)
      STHET = SIN(THETA)
      PTOT1 = PT/STHET
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRPD
CH
      ENTRY DVTRPD(NTRKP,PTOT1,DD0,DZ0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      AIR = -RTABL(JFRFT,NTRKP,JFRFIR)
      IF(AIR.EQ.0.) THEN
        PTOT1=0.
        RETURN
      END IF
      TL = RTABL(JFRFT,NTRKP,JFRFTL)
      RHO = 1./AIR
      PT = BFIELD*RHO*CLGHT/100000.
      THETA = ATAN2(1.,TL)
      STHET = SIN(THETA)
      PTOT1 = PT/STHET
      DD02=RTABL(JFRFT,NTRKP,JFRFEM+ 9)
      DD0=SQRT(DD02)
      DZ02=RTABL(JFRFT,NTRKP,JFRFEM+14)
      DZ0=SQRT(DZ02)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRT1
CH
      ENTRY DVTRT1(NTRKT,T)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(NTRKT.LE.0) THEN
        T='   #    P   dP  phi theta  D0   Z0 chiq'
        RETURN
      END IF
      AIR = -RTABL(JFRFT,NTRKT,JFRFIR)
      IF(AIR.EQ.0.) RETURN
      TL = RTABL(JFRFT,NTRKT,JFRFTL)
      RHO = ABS(1./AIR)
      PT = BFIELD*RHO*CLGHT/100000.
      TET = ATAN2(1.,TL)
      STHET = SIN(TET)
      PTOTT = PT/STHET
      FIT=RTABL(JFRFT,NTRKT,JFRFP0)
      D0T=RTABL(JFRFT,NTRKT,JFRFD0)
      Z0T=RTABL(JFRFT,NTRKT,JFRFZ0)
      CI2=RTABL(JFRFT,NTRKT,JFRFC2)
      CHT=SIGN(1.,AIR)
      DIC = SQRT(MAX(0.,RTABL(JFRFT,NTRKT,7)))
      DTL = SQRT(MAX(0.,RTABL(JFRFT,NTRKT,9)))
C
C Error in transverse momentum:
      DPT = ABS(PTOTT*RHO*DIC)
C
C Error in total momentum:
      DP = (DPT*DPT) + (DTL*PTOTT*TL/(1+TL*TL))**2
      DP = SQRT(MAX(0.,DP))
C
      IF(BNUMDB(2,FTCLDB).GT.0.) THEN
        CALL=DVCHT(NTRKT,NCTPC)
      ELSE
        NCTPC=0
      END IF
      IF(BNUMDB(2,FICLDB).GT.0.) THEN
        CALL=DVCHI(NTRKT,NCITC)
      ELSE
        NCITC=0
      END IF
C        123456789 123456789 123456789 123456789 1
C     T='   #    P   dP  phi theta  D0   Z0 chiq
      T=' *** -23.5 1.2 1.34 1.34 12.4 -99.0 1.3'
      IF(NCTPC.LE.0.AND.NCITC.GT.0) T(1:1)='i'
      IF(NCTPC.GT.0.AND.NCITC.LE.0) T(1:1)='T'
      WRITE(T(2:4),1000,ERR=2) NTRKT
 1000 FORMAT(I3)
    2 IF(CHT.GT.0.) T(6:6)='+'
      T( 7:10)=DT4(PTOTT)
      T(12:14)=DT3(DP)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_LRG'
      CALL DPARAM(33
     &  ,J_LRG)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_LRG).EQ.0.) THEN
        WRITE(T(16:19),1000,ERR=3) IFIX(PIDEG*FIT)
    3   WRITE(T(21:24),1000,ERR=5) IFIX(PIDEG*TET)
      ELSE
        WRITE(T(16:19),1001,ERR=4) FIT
    4   WRITE(T(21:24),1001,ERR=5) TET
 1001   FORMAT(F4.2)
      END IF
    5 T(26:29)=DT4(D0T)
      T(31:35)=DT5(Z0T)
      T(37:39)=DT3(CI2)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRT2
CH
      ENTRY DVTRT2(NTRKT,T)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(NTRKT.LE.0) THEN
        IF(FMONDT) THEN
          T=' #  PT LA   '
        ELSE
          T=' #  PT LA CO'
        END IF
        RETURN
      END IF
      AIR = -RTABL(JFRFT,NTRKT,JFRFIR)
      IF(AIR.EQ.0.) THEN
        PT=99.
      ELSE
        RHO = ABS(1./AIR)
        PT = BFIELD*RHO*CLGHT/100000.
      END IF
      CHT=SIGN(1.,AIR)
C        123456789 12
C     T=' #  PT LA CO'
C     T='12 -1. LA CO
      T='   -   ..   '
      WRITE(T(1:2),1003,ERR=2) NTRKT
 1003 FORMAT(I2)
      IF(CHT.GT.0.) T(4:4)='+'
      T(5:6)=DT2(PT)
      IF(ULABDT(NTRKT).NE.0.) T( 8: 9)=DT2(ULABDT(NTRKT))
      IF((.NOT.FMONDT).AND.COLRDT(NTRKT).NE.0.)
     &                        T(11:12)=DT2(COLRDT(NTRKT))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRV
CH
      ENTRY DVTRV(N)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C       CONVERSION OF JULIA CONVENTIONS TO OLD TASSO CONVENTIONS
C
      TPHEDT(1) = -RTABL(JFRFT,N,JFRFIR)
      TPHEDT(2) = RTABL(JFRFT,N,JFRFTL)
      TPHEDT(3) = RTABL(JFRFT,N,JFRFP0)
      TPHEDT(4) = -RTABL(JFRFT,N,JFRFD0)*SIGN(1.,TPHEDT(1))
      TPHEDT(5) = RTABL(JFRFT,N,JFRFZ0)
C
      FRFTDT(1)=RTABL(JFRFT,N,JFRFIR)
      FRFTDT(2)=RTABL(JFRFT,N,JFRFTL)
      FRFTDT(3)=RTABL(JFRFT,N,JFRFP0)
      FRFTDT(4)=RTABL(JFRFT,N,JFRFD0)
      FRFTDT(5)=RTABL(JFRFT,N,JFRFZ0)
      FRFTDT(6)=RTABL(JFRFT,N,JFRFAL)
C      RHO = 1./ABS(TPHEDT(1))
C      PT = BFIELD*RHO*CLGHT/100000.
C      THETA = ATAN2(1.,TPHEDT(2))
C      PTOT = PT/SIN(THETA)
      END
*DK DVTRHT
CH..............***
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVTRHT
CH
      SUBROUTINE DVTRHT(NTRK,NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :NO
C    Outputs   :FTPC=.TRUE IF TRACK HAS COORDINATES IN THE TPC
C
C    Called by :DSCTR0
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      IF(NTRK.EQ.0) THEN
        NUM=0
        IF(IW(30).EQ.12345) THEN
          IFRTL = IW(NAMIND('FRTL'))
          IF(IFRTL.NE.0) NUM=LROWS(IFRTL)
        END IF
        NTR=NUM
      ELSE
        IF(NTRK.LE.NTR) THEN
          NUM=ITABL(IFRTL,NTRK,7)
        ELSE
          NUM=-1
        END IF
      END IF
      END
C*DK IDTP
CCH..............***
CCH
CCH
CCH
CCH
CCH
CCH
CCH ********************************************************************  IDTP
CCH
C      FUNCTION IDTP(K)
CCH
CCH ********************************************************************
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC
CC!:
CC    Inputs    :
CC    Outputs   :
CC
CC    Called by :
CC ---------------------------------------------------------------------
C*CA BCS
C      INCLUDE 'DALI_CF.INC'
C*CA BMACRO
C      INCLUDE 'BCS.INC'
C      INCLUDE 'RCURNT.INC'
C      INCLUDE 'BMACRO.INC'
C      IDTP=IW(ITRA+K)
C      RETURN
CCH..............---
CCH
CCH
CCH
CCH
CCH
CCH
CCH --------------------------------------------------------------------  DNTP0
CCH
C      ENTRY DNTP0(NTRAK)
CCH
CCH --------------------------------------------------------------------
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC
CC!:
CC    Inputs    :
CC    Outputs   :
CC
CC    Called by :
CC ---------------------------------------------------------------------
C      NTRAK=0
C      IF(IW(30).NE.12345) RETURN
C      ITGTL=IW(NAMIND(TGTLDT))
C      ITGCL=IW(NAMIND('TGCL'))
C      IF(ITGTL.LE.0.OR.ITGCL.EQ.0) THEN
C         NTRAK=0
C         RETURN
C      ELSE
C         NTRAK=LROWS(ITGTL)
C         IF(NTRAK.LE.0) RETURN
C      END IF
C      ITGCL=IW(NAMIND(TGCLDT))
CC      IF(IHTRDO(6).NE.1) CALL DVTP0(1,0,NUM)
C      RETURN
CCH..............---
CCH
CCH
CCH
CCH
CCH
CCH
CCH --------------------------------------------------------------------  DNTP
CCH
C      ENTRY DNTP(ITRAK,NCO)
CCH
CCH --------------------------------------------------------------------
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC
CC!:
CC    Inputs    :
CC    Outputs   :
CC
CC    Called by :
CC ---------------------------------------------------------------------
C      NCO=ITABL(ITGTL,ITRAK,2)+ITABL(ITGTL,ITRAK,3)
C      ITRA=ITGCL+2+ITABL(ITGTL,ITRAK,1)
C      END
*DK DVVEC
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVVEC
CH
      FUNCTION DVVEC(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'J_ECNAMC.INC'
      INCLUDE 'A_ESDAJJ.INC'
      DIMENSION PO(4),PL(4,6)
      DATA NPLAN/6/,A360/6.283196/
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
      IF(NV.LT.12.AND.K.NE.K1) THEN
         IF(K.GT.0) THEN
            K1=K
            II=ITABL(IESDA,K,JESDFI)
            JJ=ITABL(IESDA,K,JESDTJ)
            KK=ITABL(IESDA,K,JESDDK)
         END IF
         CALL ESRBC('ALEPH',JJ,II,KK,PO)
      END IF
      GO TO (1,2,3,4,5,6,7,99,99,10,11,12,13,14,15,99,99,99,19,99),NV
C-------------------------------------------------------------  UNDEFINE
   99 DVVEC=0.
      RETURN
C--------------------------------------------------------------------  X
    1 DVVEC=PO(1)
      RETURN
C--------------------------------------------------------------------  Y
    2 DVVEC=PO(2)
      RETURN
C--------------------------------------------------------------------  Z
    3 DVVEC=PO(3)
      RETURN
C--------------------------------------------------------------------  R
    4 IF(K.NE.KR) THEN
         R=SQRT((PO(1))**2+(PO(2))**2)
         KR=K
      END IF
      DVVEC=R
      RETURN
C--------------------------------------------------------------------  F
    5 FI=ATAN2(PO(2),PO(1))
      IF(FI.LT.0.) FI=FI+A360
      DVVEC=FI
      RETURN
C--------------------------------------------------------------------  T
    6 IF(KT.NE.K) THEN
         IF(K.NE.KR) THEN
            R=SQRT((PO(1))**2+(PO(2))**2)
            KR=K
         END IF
         T=ATAN2(R,PO(3))
         KT=K
      END IF
      DVVEC=T
      RETURN
C--------------------------------------------------------------------  D
    7 DVVEC=SQRT((PO(1))**2+(PO(2))**2+(PO(3))**2)
      RETURN
C--------------------------------------------------------------------  D
   10 IF(K.NE.K2) THEN
         CALL ESRPL('ALEPH',JJ,II,KK,NPLAN,PL)
         K2=K
      END IF
      DVVEC=ACOS(ABS(PL(1,3)*PL(1,4)+PL(2,3)*PL(2,4)+PL(3,3)*PL(3,4)))
      RETURN
C--------------------------------------------------------------------  D
   11 IF(K.NE.K2) THEN
         CALL ESRPL('ALEPH',JJ,II,KK,NPLAN,PL)
         K2=K
      END IF
      DVVEC=ACOS(ABS(PL(1,5)*PL(1,6)+PL(2,5)*PL(2,6)+PL(3,5)*PL(3,6)))
      RETURN
C--------------------------------------------------------------------  E
   12 DVVEC=RTABL(IESDA,K,JESDME)
      RETURN
C--------------------------------------------------------------------  I
   13 DVVEC=ITABL(IESDA,K,JESDFI)
      RETURN
C--------------------------------------------------------------------  J
   14 DVVEC=ITABL(IESDA,K,JESDTJ)
      RETURN
C--------------------------------------------------------------------  K
   15 DVVEC=ITABL(IESDA,K,JESDDK)
      RETURN
C--------------------------------------------- CLUSTER NUMBER -------  U
   19 DVVEC=ITABL(IESDA,K,JESDEC)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVVEC0
CH
      ENTRY DVVEC0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVVEC0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IESDA = IW(NAMIND('ESDA'))
      IF(IESDA.EQ.0) RETURN
      NUM=LROWS(IESDA)
      K1=0
      K2=0
      KR=0
      KT=0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVVEC1
CH
      ENTRY DVVEC1(I0,J0,K0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVVEC1=0
      II=I0
      JJ=J0
      KK=K0
      END
*DK DVVMCT0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVVMCT0
CH
      SUBROUTINE DVVMCT0(NVTX,NTRA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 10-May-1989   C.Grab  Adapted to CERNVM, Added log.definitions
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CD BCS
      DIMENSION PTRA(3),CLTM(3),XVER(3)
      LOGICAL FTK,FSH
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      DATA AMAG/15./,FTK/.TRUE./,FSH/.TRUE./,CUT/0.1/
      INCLUDE 'DALI_KI.INC'
      NVTX = 0
      NTRA = 0
      IF (IW(30).NE.12345) RETURN
      NAPAR = NAMIND('PART')
      JPAR=IW(NAPAR)
C      IF(JPAR.EQ.0) THEN
C         CALL GPARTB
C         JPART=IW(NAPAR)
C      END IF
      IF(JPAR.EQ.0) THEN
        CALL DWRT('No PART bank. FKINE cannot be displayed.')
        RETURN
      END IF
      MAXTP=LROWS(JPAR)
      IF(NFKIN.EQ.0) THEN
         CALL FYIRUN(AMAG,FTK,FSH,CUT)
         NFKIN=NAMIND('FKIN')
         NFVER=NAMIND('FVER')
      END IF
      JFVER=IW(NFVER)
      IF(JFVER.EQ.0) THEN
         CALL FYKINE('DROP','NOGA',IER)
         IF(IER.NE.0) THEN
           CALL DWRT('Error in FYKINE')
           RETURN
         END IF
         JFVER=IW(NFVER)
      END IF
      NVTX=LROWS(JFVER)
      JFKIN=IW(NFKIN)
      NTRA=LROWS(JFKIN)
      MTRA=NTRA
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVVMCVX
CH
      ENTRY DVVMCVX(NUMVR,KTRA1,KTRA2,KTRIN,XVER)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C-----------------------------------------------------------------------
C Input NUMVR vertex number
C Output NTRA Number of outgoing tracks
C        XVER(3) Vertex coordinates
C-----------------------------------------------------------------------
C                                       ! ONLY DVMCT0 MUST BE CALLED BEFORE
      IF(JFVER.NE.0) THEN
         KFVER=KROW(JFVER,NUMVR)
         KTRA1   = IW(KFVER+6)+1
         KTRA2   = IW(KFVER+7)+IW(KFVER+6)
         KTRIN   = IW(KFVER+5)
         XVER(1) = RW(KFVER+1)
         XVER(2) = RW(KFVER+2)
         XVER(3) = RW(KFVER+3)
      ELSE
         KTRA1=0
      ENDIF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVVMCTR
CH
      ENTRY DVVMCTR(KTRA,PTRA,CLTM,ITYPE,NVIN,NVOUT)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C                                       ! ONLY DVMCT0 MUST BE CALLED BEFORE
C-----------------------------------------------------------------------
C INPUT  KTRA Track number
C Output PTRA(3) Track momenta
C        Q      Charge
C        ITYPE  Particle type
C        NVIN     Vertex number at beg. of track
C        NVOUT    Vertex number at end  of track
C-----------------------------------------------------------------------
      IF(JFKIN.NE.0) THEN
         KFKIN=KROW(JFKIN,KTRA)
         NVOUT=IW(KFKIN+7)
         NVIN =IW(KFKIN+6)
         ITYPE   = IW(KFKIN+5)
         IF(ITYPE.LE.0) THEN
           CALL DWRT('--> in DVVMCTR: ITYPE=<0')
           RETURN
         ENDIF
         IF(NVIN.EQ.NVOUT.OR.ITYPE.GT.MAXTP) GO TO 90
         PTRA(1) = RW(KFKIN+1)
         PTRA(2) = RW(KFKIN+2)
         PTRA(3) = RW(KFKIN+3)
CSO         CLTM(1) = CHARGE(ITYPE)
         CLTM(1) = RTABL(JPAR,ITYPE,7)
CSO         TI = TIMLIF(ITYPE)
         TI = RTABL(JPAR,ITYPE,8)
         IF(TI.LT.0.) THEN
            CLTM(2)=-99.
         ELSE IF(TI.EQ.0)THEN
            CLTM(2)=-90.
         ELSE
            CLTM(2) = ALOG10(TI)
         END IF
CSO         CLTM(3)=PARMAS(ITYPE)
         CLTM(3)=RTABL(JPAR,ITYPE,6)
C         PNAME(1:4) = CHAINT(ITABL(JPAR,ITYPE,2)
C         PNAME(5:8) = CHAINT(ITABL(JPAR,ITYPE,3)
C         PNAME(9:12) = CHAINT(ITABL(JPAR,ITYPE,4)
      ELSE
         ITYPE=-9999
      ENDIF
      RETURN
   90 ITYPE=-ITYPE
      END
*DK DVVDC
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVVDC
CH
      FUNCTION DVVDC(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DATA ZMAX/217./,RMAX/170.622/
      DATA PIDEG/57.29577951/
      INCLUDE 'A_VDCOJJ.INC'
      INCLUDE 'A_VCPLJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      IF(NV.LT.10.AND.KK.NE.K) THEN
        KK=K
        RO=RTABL(IVDCO,K,JVDCR0)
        FI=RTABL(IVDCO,K,JVDCPH)
        ZZ=RTABL(IVDCO,K,JVDCZ0)
      END IF
      GO TO(1,2,3,4,5,6,7,8,99,99,99,12,13,14,99,99,99,18,99,99),NV
C--------------------------------------------------------------  UNDEFIN
   99 DVVDC=0.
      RETURN
C----------------------------------------------------------------------
    1 DVVDC=RO*COS(FI)
      RETURN
C----------------------------------------------------------------------
    2 DVVDC=RO*SIN(FI)
      RETURN
C----------------------------------------------------------------------
    3 DVVDC=ZZ
      RETURN
C----------------------------------------------------------------------
    4 DVVDC=RO
      RETURN
C----------------------------------------------------------------------
    5 DVVDC=FI*PIDEG
      RETURN
C----------------------------------------------------------------------
    6 Z=ZZ-VRDZDV
      IF(K.NE.KT) THEN
         TEDZ=PIDEG*ATAN2(RO,Z)
         KT=K
      END IF
      IF(TEDZ.LE.90.) THEN
        DVVDC=TEDZ-QTETDV*TEDZ
      ELSE
        DVVDC=TEDZ-QTETDV*(180.-TEDZ)
      END IF
      RETURN
C----------------------------------------------------------------------
    7 DVVDC=SQRT(ZZ*ZZ+RO*RO)
      RETURN
C----------------------------------------------------------------------
    8 IF(K.NE.KB) THEN
         ZA=ABS(ZZ)
         D0=SQRT(ZZ**2+RO**2)
         IF(RO*ZMAX.GT.ZA*RMAX) THEN
            B=D0*(RMAX/RO-1.)
         ELSE
            B=D0*(ZMAX/ZA-1.)
         END IF
         KB=K
      END IF
      DVVDC=B
      RETURN
C----------------------------------------------------------------------
C     ................................................ WAFER IDENTIFIER
   12 NBNUM=ITABL(IVDCO,K,JVDCWI)
      DVVDC=NBNUM
      RETURN
C----------------------------------------------------------------------
C     .................................................... LAYER NUMBER
   13 NBNUM=ITABL(IVDCO,K,JVDCWI)
      CALL VADEWA(NBNUM,ILAY,IWAF,IPHI,IVIEW)
      DVVDC=ILAY
      RETURN
C----------------------------------------------------------------------
C     ................................... QUALITY 1:PHI   2:Z    3:BOTH
   14 I=ITABL(IVDCO,K,JVDCQF)
      DVVDC=IBITS(I,0,2)
      RETURN
C----------------------------------------------------------------------
   18 DVVDC=ITABL(IVDCO,K,JVDCTN)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------- DVVDC1
CH
      ENTRY DVVDC1(NHIT,IWFXY,IHTXY,IWFZT,IHTZT)
CH
CH --------------------------------------------------------------------
CH
      DVVDC1=0
      IWFZT = ITABL(IVDCO,NHIT, JVDCWI)
      IWFXY = IWFZT
      II = MOD(IWFZT/1000,10)
      IF(II.EQ.2) IWFXY = IWFXY + 1000
      IF(II.EQ.1) IWFXY = IWFXY - 1000
      ITRK =ITABL(IVDCO,NHIT, JVDCTN)
      IVCPL=NLINK('VCPL', ITRK)
      IF(IVCPL .EQ. 0) THEN
        CALL DWRT('Error: VCPL is missing')
        IWFXY=-1
        RETURN
      END IF
      IHTXY=-1
      IHTZT=-1
      DO I = 1, LROWS(IVCPL)
        IF(IWFZT.EQ.ITABL(IVCPL,I,JVCPZB))
     &    IHTZT=ITABL(IVCPL,I,JVCPNZ)
        IF(IWFXY.EQ.ITABL(IVCPL,I,JVCPXB))
     &    IHTXY=ITABL(IVCPL,I,JVCPNX)
      END DO
      IF(IHTXY.EQ.-1.OR.IHTZT.EQ.-1)
     &  CALL DWRT('Error: could not find hit')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVVDC0
CH
      ENTRY DVVDC0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVVDC0=0
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IVDCO = IW(NAMIND('VDCO'))
      IF(IVDCO.EQ.0) RETURN
      NUM=LROWS(IVDCO)
      KK=0
      KT=0
      KB=0
      END
*DK DVVDRZ
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVVDRZ
CH
      FUNCTION DVVDRZ(NV,K,MUPA)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C               FACEDV(SIDE,X or Y,PHI,LAYER)
      DIMENSION FACE(2,2,15,2)
      EQUIVALENCE (FACE,CRILDV)
      CHARACTER *4 DT4
C     DIMENSION MFRW(4)
C     DATA MFRW/-1,-1,0,0/
      DIMENSION KVD(999),JVD(999),MVD(999)
      COMMON /DVDTRZ/ NNVDX(999),NBVDX(999),NTVDX(999),
     &                NNVDZ(999),NBVDZ(999),NTVDZ(999),
     &                MXVDX,MXVDZ,NRELX(3,1000),NRELZ(2,1000)
      DIMENSION RVD1(999),RVD2(999),ZVD(2,999),TVD(2,999),FVD(999),
     &  PVD(999),WVD(2,999)
C                   1,2  LAYER
      DIMENSION ROVD(2,   2)
C     DATA RO/9.5,10.0389, 11.3459,11.9/
C      DIMENSION FI(0:15,2)
C      DATA FI/345.,
C     &    15.,  45., 75.,105.,135.,165.,195.,225.,255.,285.,
C     &   315., 345.,            0.,  0., 0.,
C     &         342.,
C     &      6., 30., 54., 78.,102.,126.,150.,174.,198.,222.,
C     &    246.,270.,294.,318.,342./
      DATA IB30/536870912/
C     INCLUDE 'MHITJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C            1  2  3  4  5  6  7  8  9 10
      GO TO(99,99, 3, 4, 5, 6,99,99, 9,10
     &     ,99,12,99,14,15,16,99,18,19,20),NV
C--------------------------------------------------------------  UNDEFIN
   99 DVVDRZ=0.
      RETURN
C----------------------------------------------------------------------
    3 DVVDRZ=ZVD(MUPA,K)
      RETURN
C----------------------------------------------------------------------
    4 DVVDRZ=RVD1(K)
      RETURN
C----------------------------------------------------------------------
    5 DVVDRZ=FVD(K)
      RETURN
C----------------------------------------------------------------------
    6 DVVDRZ=TVD(MUPA,K)
      RETURN
C----------------------------------------------------------------------
    9 DVVDRZ=RVD2(K)
      RETURN
C----------------------------------------------------------------------
   10 DVVDRZ=WVD(MUPA,K)
      RETURN
C----------------------------------------------------------------------
   12 DVVDRZ=PVD(K)
      RETURN
C------------------------------------------------------------ module 1 - 4
   14 DVVDRZ=JVD(K)
      RETURN
C---------------------------------------------------------- layer 1,2
   15 DVVDRZ=KVD(K)
      RETURN
C------------------------------------------------------ face 1-9 , 1-15
   16 DVVDRZ=MVD(K)
      RETURN
C-------------------------------------------------------------- track
   18 DVVDRZ=NTVDZ(K)
      RETURN
C-------------------------------------------------------------- bank
   19 DVVDRZ=NBVDZ(K)
      RETURN
C-------------------------------------------------------------- z-module 1-4
   20 DVVDRZ=NNVDZ(K)
      RETURN
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVMDR0
CH
      ENTRY DVVDR0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVVDR0=0
      NUM=NUM0
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVVDR1
CH
      ENTRY DVVDR1(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     CALLED BY DEVSET ONCE PER EVENT
C ---------------------------------------------------------------------
      DVVDR1=0
      NUM=0
      IF(IW(30).NE.12345) GO TO 900
      CALL VZERO(NRELZ,2000)
      CALL DGIVDR(ROVD)
      FREJ=0.
      MXVDZ=0.
      IMVD = IW(NAMIND('VDZT'))
  100 IF(IMVD.EQ.0) GO TO 102
      NB=IW(IMVD-2)
      IF(NWAFDV.EQ.6) NVDMR=NLINK('VDMR',NB)
      CALL VADEWA(NB,NL,NW,NF,ND)
C     MO=2*NF+MFRW(NW)
      NU=LROWS(IMVD)
      IF(NU.EQ.0) GO TO 101
      R1=ROVD(1,NL)
      R2=ROVD(2,NL)
      RM=0.5*(R1+R2)
      FF=FIMDDV(NF,NL)
      DO 700 N=1,NU
        IF(IAND(IB30,ITABL(IMVD,N,6)).EQ.IB30) THEN
          FREJ=FREJ+1.
        ELSE
          NUM=NUM+1
          PVD(NUM)=RTABL(IMVD,N,5)
          Z=RTABL(IMVD,N,1)
          ZVD(1,NUM)=Z
          WVD(1,NUM)=RTABL(IMVD,N,2)
          RVD1(NUM)=R1
          RVD2(NUM)=R2
          TVD(1,NUM)=DATN2D(RM,Z-VRDZDV)
          FVD(NUM)=FF
C         .......................................................... LAYER 1,2
          KVD(NUM)=NL
C         ....................................................... WAFER 1 OR 4
          JVD(NUM)=NW
C         ................................................... face 1-9 or 1-15
          MVD(NUM)=NF
C         .......................................................... ROW
          NNVDZ(NUM)=N
C         .......................................................... BANK
          NBVDZ(NUM)=NB
C         ........................................................ hit relation
          IF(NWAFDV.EQ.6) THEN
            LPOS=ITABL(NVDMR,N,1)
            IF(     NRELZ(1,LPOS).EQ.0) THEN
              NRELZ(1,LPOS)=NUM
            ELSE
              NRELZ(2,LPOS)=NUM
            END IF
            MXVDZ=MAX(LPOS,MXVDZ)
          END IF
        END IF
        IF(NUM.GE.999) GO TO 900
  700 CONTINUE
  101 IMVD=IW(IMVD-1)
      GO TO 100
  102 IF(FREJ.GT.0.)
     &   CALL DWRT(' VDZT: '//DT4(FREJ)//' dead hits rejected.')
  900 NUM0=NUM
      END
*DK DVVDXY
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVVDXY
CH
      FUNCTION DVVDXY(NV,K)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *4 DT4
      DIMENSION VUW(3),XYZ(3)
      DATA VUW/0.,0.,0./,NUC/2/
      DIMENSION XVD(999),YVD(999),UVD(999)
      DATA IB30/536870912/
      DIMENSION KVD(999),JVD(999),MVD(999),PVD(999)
      COMMON /DVDTRZ/ NNVDX(999),NBVDX(999),NTVDX(999),
     &                NNVDZ(999),NBVDZ(999),NTVDZ(999),
     &                MXVDX,MXVDZ,NRELX(3,1000),NRELZ(2,1000)
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C            1  2  3  4  5  6  7  8  9 10
      GO TO( 1, 2,99, 4, 5,99,99,99,99,10
     &     ,99,12,99,14,15,16,99,18,19,20),NV
C--------------------------------------------------------------  UNDEFIN
   99 DVVDXY=0.
      RETURN
C----------------------------------------------------------------------
    1 DVVDXY=XVD(K)
      RETURN
C----------------------------------------------------------------------
    2 DVVDXY=YVD(K)
      RETURN
C----------------------------------------------------------------------
    4 DVVDXY=SQRT(XVD(K)*XVD(K)+YVD(K)*YVD(K))
      RETURN
C----------------------------------------------------------------------
    5 DVVDXY=DATN2D(YVD(K),XVD(K))
      RETURN
C----------------------------------------------------------------------
   10 DVVDXY=UVD(K)
      RETURN
C------------------------------------------------------------ pulse hight
   12 DVVDXY=PVD(K)
      RETURN
C--------------------------------- WAFER 1 or 4; 2 connected to 1, 3 to 4
   14 DVVDXY=JVD(K)
      RETURN
C----------------------------------------------------------- layer 1 or 2
   15 DVVDXY=KVD(K)
      RETURN
C------------------------------------------------------- face 1-9 or 1-15
   16 DVVDXY=MVD(K)
      RETURN
C---------------------------------------------------------------- track #
   18 DVVDXY=NTVDX(K)
      RETURN
C----------------------------------------------------- bank example:13110
   19 DVVDXY=NBVDX(K)
      RETURN
C--------------------------------------------------------- row in bank
   20 DVVDXY=NNVDX(K)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVMDX0
CH
      ENTRY DVVDX0(NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DVVDX0=0
      NUM=NUM0
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVVDX1
CH
      ENTRY DVVDX1(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     CALLED BY DEVSET ONCE PER EVENT
C ---------------------------------------------------------------------
      DVVDX1=0
      NUM=0
      IF(IW(30).NE.12345) GO TO 900
      CALL VZERO(NRELX,2000)
      FREJ=0.
      MXVDX=0.
      IMVD = IW(NAMIND('VDXY'))
  100 IF(IMVD.EQ.0) GO TO 102
      NU=LROWS(IMVD)
      NB=IW(IMVD-2)
      IF(NWAFDV.EQ.6) NVDMR=NLINK('VDMR',NB+1)
      CALL VADEWA(NB,NL,NW,NF,ND)
C     MO=2*NF+MFRW(NW)
      DO 700 N=1,NU
        IF(IAND(IB30,ITABL(IMVD,N,8)).EQ.IB30) THEN
          FREJ=FREJ+1.
        ELSE
          NUM=NUM+1
          PVD(NUM)=RTABL(IMVD,N,7)
          XVD(NUM)=RTABL(IMVD,N,1)
          YVD(NUM)=RTABL(IMVD,N,2)
          UVD(NUM)=RTABL(IMVD,N,3)
C         .......................................................... LAYER 1,2
          KVD(NUM)=NL
C         ....................................................... WAFER 1 OR 4
          JVD(NUM)=NW
C         ................................................... face 1-9 or 1-15
          MVD(NUM)=NF
C         .......................................................... ROW
          NNVDX(NUM)=N
C         .......................................................... BANK
          NBVDX(NUM)=NB
C         ........................................................ hit relation
          IF(NWAFDV.EQ.6) THEN
            LPOS=ITABL(NVDMR,N,1)
            IF(     NRELX(1,LPOS).EQ.0) THEN
              NRELX(1,LPOS)=NUM
            ELSE IF(NRELX(2,LPOS).EQ.0) THEN
              NRELX(2,LPOS)=NUM
            ELSE
              NRELX(3,LPOS)=NUM
            END IF
            MXVDX=MAX(LPOS,MXVDX)
          END IF
        END IF
        IF(NUM.GE.999) GO TO 900
  700 CONTINUE
      IMVD=IW(IMVD-1)
      GO TO 100
  102 IF(FREJ.GT.0.)
     &   CALL DWRT(' VDXY: '//DT4(FREJ)//' dead hits rejected.')
  900 NUM0=NUM
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVVD3D
CH
      ENTRY DVVD3D(K3D,N3D1,N3D2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C      N3D1=LVD1(K3D)
C      N3D2=LVD2(K3D)
      DVVD3D=0
      N3D1=1
      N3D2=2
      END
*DK DVVDTR
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVVDTR
CH
      SUBROUTINE DVVDTR(NWAF,NTRK,NUVDX,NUVDZ,NUM)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :DEVSET ONCE PER EVENT
C        DVTR0, DVVDX1, DVVDR1   MUST BE CALLED FIRST
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      COMMON /DVDTRZ/ NNVDX(999),NBVDX(999),NTVDX(999),
     &                NNVDZ(999),NBVDZ(999),NTVDZ(999),
     &                MXVDX,MXVDZ,NRELX(3,1000),NRELZ(2,1000)
c     remove when VZMRJJ existiert
C      PARAMETER(JVZMNR=1,JVZMRO=2,LVZMRA=2)
C     INCLUDE 'VZMRJJ.INC'
      INCLUDE 'A_VCPLJJ.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      NUM=0
      IF(IW(30).NE.12345) RETURN
      IF(NTRK.LE.0) RETURN
      IF(NUVDX.LE.0.AND.NUVDZ.LE.0) RETURN
      CALL VZERO(NTVDX,NUVDX)
      CALL VZERO(NTVDZ,NUVDZ)
      DO NT=1,NTRK
        IVCPL=NLINK('VCPL',NT)
        IF(IVCPL.NE.0) THEN
          NR=LROWS(IVCPL)
          IF(NR.GT.0) THEN
            DO N=1,NR
              NX=ITABL(IVCPL,N,JVCPNX)
              IF(NX.GT.0) THEN
                NXB=ITABL(IVCPL,N,JVCPXB)
C               ................. track NT has a VD hit #NX in VDXY NXB
C               ................. loop over all phi strips and store NT 
                DO K=1,NUVDX
                  IF(NBVDX(K).EQ.NXB.AND.NNVDX(K).EQ.NX) THEN
                    NTVDX(K)=NT
                    GO TO 1
                  END IF
                END DO
 1000           FORMAT(1X,A,I8,2I4)
    1           NUM=NUM+1
              END IF
              NZ=ITABL(IVCPL,N,JVCPNZ)
              IF(NZ.GT.0) THEN
                NZB=ITABL(IVCPL,N,JVCPZB)
C               ................. track NT has a VD hit #NZ in VDZT NXB
C               ................. loop over all z strips and store NT 
                DO K=1,NUVDZ
                  IF(NBVDZ(K).EQ.NZB.AND.NNVDZ(K).EQ.NZ) THEN
                    NTVDZ(K)=NT
                    GO TO 2
                  END IF
                END DO
    2           NUM=NUM+1
              END IF
            END DO
          END IF
        END IF
      END DO
C     ......................................... negative track # for ghost hit
C     ....................... track # = MCTRDC for hits with more than 1 track
C
C     ......................... Old code before 5.6.96
C      DO K=1,MXVDX
C        L1=NRELX(1,K)
C        L2=NRELX(2,K)
C        L3=NRELX(3,K)
C        IF(     NTVDX(L1).NE.0) THEN
C          IF(NTVDX(L2).NE.0.OR.NTVDX(L3).NE.0) GO TO 10
C          NTVDX(L2)=-NTVDX(L1)
C          NTVDX(L3)=-NTVDX(L1)
C        ELSE IF(NTVDX(L2).NE.0) THEN
C          IF(NTVDX(L3).NE.0) GO TO 10
C          NTVDX(L1)=-NTVDX(L2)
C          NTVDX(L3)=-NTVDX(L2)
C        ELSE IF(NTVDX(L3).NE.0) THEN
C          NTVDX(L1)=-NTVDX(L3)
C          NTVDX(L2)=-NTVDX(L3)
C        END IF
C        GO TO 11
C   10   NTVDX(L1)=MCTRDC
C        NTVDX(L2)=MCTRDC
C        NTVDX(L3)=MCTRDC
C   11 END DO
C      DO K=1,MXVDZ
C        L1=NRELZ(1,K)
C        L2=NRELZ(2,K)
C        IF(     NTVDZ(L1).NE.0) THEN
C          IF(NTVDZ(L2).NE.0) GO TO 20
C          NTVDZ(L2)=-NTVDZ(L1)
C        ELSE IF(NTVDZ(L2).NE.0) THEN
C          NTVDZ(L1)=-NTVDZ(L2)
C        END IF
C        GO TO 21
C   20   NTVDZ(L1)=MCTRDC
C        NTVDZ(L2)=MCTRDC
C        NTVDZ(L3)=MCTRDC
C   21 END DO
C
C     ...................H.D. MCTRDC -> MTRKDT on 15.10.96 as in DAPPV
C     ...................... FNOHDT(NTRVD) is used with NTRVD=MCTRDC
C     ....................... new code by D.Casper 5.6.96
      DO K=1,MXVDX
        L1=NRELX(1,K)
        L2=NRELX(2,K)
        L3=NRELX(3,K)
        IF (L1.GT.0) THEN    ! A hit somewhere
          IASSOC = 0
          NASSOC = 0
          IF(NTVDX(L1).GT.0) THEN
            IASSOC = L1
            NASSOC = NASSOC+1
          END IF
          IF(L2.GT.0 .AND. NTVDX(L2).GT.0) THEN
            IASSOC = L2
            NASSOC = NASSOC+1
          END IF
          IF(L3.GT.9 .AND. NTVDX(L3).GT.0) THEN
            IASSOC = L3
            NASSOC = NASSOC+1
          END IF
          IF(NASSOC.EQ.1) THEN
            IF(IASSOC.EQ.L1) THEN
              NTVDX(L2) = -NTVDX(L1)
              NTVDX(L3) = -NTVDX(L1)
            ELSE IF(IASSOC.EQ.L2) THEN
              NTVDX(L1) = -NTVDX(L2)
              NTVDX(L3) = -NTVDX(L2)
            ELSE
              NTVDX(L1) = -NTVDX(L3)
              NTVDX(L2) = -NTVDX(L3)
            ENDIF
          ELSE IF(NASSOC.GT.1) THEN
            NTVDX(L1) = MTRKDT
            NTVDX(L2) = MTRKDT
            NTVDX(L3) = MTRKDT
          END IF
        END IF
   11 END DO
      DO K=1,MXVDZ
        L1=NRELZ(1,K)
        L2=NRELZ(2,K)
        IF(L2.GT.0) THEN
            IF(     NTVDZ(L1).GT.0 .AND. NTVDZ(L2).GT.0) THEN
              NTVDZ(L1) = MTRKDT
              NTVDZ(L2) = MTRKDT
            ELSE IF(NTVDZ(L1).GT.0) THEN
                NTVDZ(L2) = -NTVDZ(L1)
            ELSE IF(NTVDZ(L2).GT.0) THEN
                NTVDZ(L1) = -NTVDZ(L2)
            END IF
        END IF
   21 END DO
      END
*DK DVVX0
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVVX0
CH
      SUBROUTINE DVVX0(TPR0,NVX,FACT,SCA)
C ---------------------------------------------------------------------
C
C    Created by J.Lauber                   27-MAY-1991
C     update 7-sep-1991
C     update 28-oct-1991 introduce beam crossing as Vertex number 0
C!:
C    Inputs    : Contents of bank PYER
C    Outputs   : returns number of vertices in bank PYER
C    Outputs from entry DVVX
C              :  cross hairs of vertex coordinates error ellips
C              :  points on the ellips
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'A_PYERJJ.INC'
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TPR,TPR0
      INCLUDE 'A_BCS.INC'
C
C     ......................................................... 180 degrees/pi
      PARAMETER (DSCAL = 57.2957795130824)
      PARAMETER (PI=3.141592653589)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
C     ......................................................  points on ellips
C To change number of points make NPT a variable and transfer operation on
C thinc to DVVX
C
      PARAMETER (NPT=61)
      DIMENSION H(2,2),V(2,2)
      DIMENSION RKOVM(3),XE(NPT),YE(NPT),XLKP(NPT),YLKP(NPT)
      DIMENSION XEL(4),YEL(4),ZEL(4)
      DIMENSION AR(3,3),RKVI(3,3),WR(3),ZR(3,3),WORK(3),RINV(6)
      DIMENSION RBETR(3)
      DIMENSION SCA(2,3)
      DIMENSION RCT(3,3)
      DIMENSION RCOM(6)
      DIMENSION XYZ(3)
C     .... beam crossing related
      LOGICAL FNWR/.TRUE./
      LOGICAL LGARB
      LOGICAL LFAC
      DIMENSION VRBCR(6),RBCR(3),dbcr(3)
C     ....
      INTEGER INIT/0/
C     .... bank nummer of bank PYER
      INTEGER NRPY/0/
C     ... CUT ON MINIMUM SIGMA FOR ELLIPS CALCULATION
      DATA RIF  /0.01/
C     .... reduce phi scan range of FT projection because of numerical
C     .... instability
      DATA R1ME /0.99/    !... must be .lt.1.0   i.e. 1.0-epsilon 0.995 is bad
C     .............................................. always after declarations
      INCLUDE 'A_BMACRO.INC'
C     ........................................................................
      IF(INIT.EQ.0) THEN
C       ......................... this needs to be done only once in a session
C       ........................................... or if you need more points
        THETA  = 0.0
        THINC = 6.283185307/FLOAT(NPT-1)
        DO I=1,NPT-1
          XLKP(I)  = COS(THETA)
          YLKP(I)  = SIN(THETA)
          THETA    = THETA + THINC
        END DO
      END IF
C
      TPR = TPR0
      NVX = 0
C     .... links to the first found bank
C      KPYER = IW(NAMIND('PYER'))
C     .... links to the bank with a given number
      KPYER = NLINK('PYER',NRPY)
      IF (KPYER.NE.0) THEN
C       .... number of vertices
        NVX = LROWS(KPYER)
        IF(NVX.EQ.0) RETURN
      END IF
C     .... rotation in kartesian room sin/cos  phi theta gamma
C     ..... store for later use
C
C
C
      IF(TPR .EQ. 'FT') THEN
C
C
        ZOFF=SCA(1,1)
C
C
      ELSEIF(TPR .EQ. 'RO') THEN
C
C
        SF=SCA(1,1)
        CF=SCA(2,1)
        ST=SCA(1,2)
        CT=SCA(2,2)
        SG=SCA(1,3)
        CG=SCA(2,3)
C      X1=..X..
C      Y1=..Y..
C      Z1=..Z..
C         .................................................... X2= CF*X1+SF*Y1
C         .................................................... Y2=-X1*SF+CF*Y1
C         .................................................... Z2= Z1
C         .................................................... X3= CT*X2+ST*Z2
C         .................................................... Y3= Y2
C         .................................................... Z3=-ST*X2+CT*Z2
C         ...................................................H=X4= X3
C         .................................................. V=Y4= CG*Y3+SG*Z3
C         .................................................... Z4=-SG*Y3+CG*Z3
C      X2= CF*X1+SF*Y1
C      Y2=-SF*X1+CF*Y1
C      H = CT*X2+ST*Z1
C      Z3=-ST*X2+CT*Z1
C      V = CG*Y2+SG*Z3
C      Z4=-SG*Y2+CG*Z3
C     ..... h v as output
C
C     ..... rotation matrix for the covariance matrices, first index row,second
C     ..... index column
        RCT(1,1) =  CT*CF
        RCT(2,1) =  SF*CT
        RCT(3,1) =  ST
        RCT(1,2) = -ST*SG*CF - SF*CG
        RCT(2,2) = -ST*SG*SF + CF*CG
        RCT(3,2) =  CT*SG
        RCT(1,3) = -CF*ST*CG + SG*SF
        RCT(2,3) = -ST*CG*SF - SG*CF
        RCT(3,3) =  CT*CG
C
C
      ELSEIF(TPR .EQ. 'YZ') THEN
C
C
C       .... rotation in x-y-plane  and y=y'=x2
C       .... X2= CF*X1+SF*Y1
C       .... Y2=-SF*X1+CF*Y1
        SF=SCA(1,1)
        CF=SCA(2,1)
      END IF
C
      FAC = FACT
      IF(FAC.EQ.0.0) THEN
        FAC  = 1.0
        LFAC = .TRUE.
      ELSE
        LFAC = .FALSE.
      END IF
      RETURN
C     .......................................................................
C
C
C
C
      ENTRY DVVX(KVX,H,V,XE,YE,RMISC,XYZ)
C
C
C
C     ........ KVX is the number of the Vertex, special case =0 then beam
C     ........                                  crossing is requested
C     ........ You always have to transform the kovariance matrix
C     ........ rmisc is an output parameter dependant on projection
C
C
      IF(KVX.EQ.0) THEN
C       ... we look at the beam crossing now
        IF(FNWR) THEN
C         .... calculate BEAM CROSSING values  only when needed for new run
          FNWR=.FALSE.
C         ..... reset beam crossing position
          RBCR(1) = 0.0
          RBCR(2) = 0.0
          RBCR(3) = 0.0
C         ..... reset beam crossing variance matrix
          DO II=1,6
            VRBCR(II)=0
          ENDDO
          IF(KRUN.GT.2000) THEN
C           ..... real data
            CALL GETLEP(KRUN,IFOUN,IFILL,NV,ELEP,RBCR,DBCR)
            IF(IFOUN.EQ.0) THEN
C             ...... no lep information for run, use default values
C             ... x 150. micron y 20. micron z 1.0 cm
              VRBCR(1)= 0.0150**2
              VRBCR(3)= 0.0020**2
              VRBCR(6)= 1.0**2
            ELSE
C             ... if(ifoun.eq.2) then
C             ...   lep information for run from header rec
C             ... endif
              VRBCR(1)=DBCR(1)**2*FLOAT(NV-1)
              VRBCR(3)=DBCR(2)**2*FLOAT(NV-1)
              VRBCR(6)=DBCR(3)**2*FLOAT(NV-1)
            ENDIF
          ELSE
C           ...... monte carlo data
            KKHVF = IW(NAMIND('KHVF'))
            IF(KKHVF.EQ.0) THEN
C             ...... bank KHVF missing for  monte carlo data, use default values
C             ... x 180. micron y 10. micron z 1.0 cm
              VRBCR(1)= 0.0180**2
              VRBCR(3)= 0.0010**2
              VRBCR(6)= 1.0**2
            ELSE
              VRBCR(1)=RW(KKHVF+LMHLEN+7)**2
              VRBCR(3)=RW(KKHVF+LMHLEN+8)**2
              VRBCR(6)=RW(KKHVF+LMHLEN+9)**2
            ENDIF
          ENDIF
C         ..... put it to Bosbank PYBC, for easier access structure !
C         ..................... save the Beam Crossing in the bank PYBC
          KPYBC=IW(NAMIND('PYBC'))
          IF(KPYBC.GT.0) THEN
C           ...................... bank already exists
C            KLAST = LROWS(KPYBC)+1
C           .... ? don't increment
            KLAST = LROWS(KPYBC)
          ELSE
C
C           ..... book format !
            CALL BKFMT('PYBC','2I,(I,10F,I)')
            KLAST = 1
          ENDIF
          KYWI  = LPYERA*KLAST
C         .....................   we book here the space for the bank
          CALL AUBOS('PYBC',0,LMHLEN+KYWI,KPYBC,IRET)
          LGARB=.FALSE.
          IF(IRET.EQ.1) LGARB=.TRUE.
          IF(IRET.EQ.2) THEN
C           ........................? no space for creating this bank, use
C           .... first entry in PYER (not to store, but to retrieve!!!)
            IPYBC=KROW(KPYER,1)
          ELSE
            IW(KPYBC+LMHCOL) = LPYERA
            IW(KPYBC+LMHROW) = KLAST
C           ......?
            IPYBC = KROW(KPYBC,KLAST)
C           ...... store information
C           ......... type of vertex 0..255 1=main 2=v0,3=main for 2-prongs
C                                                4=conversion
C                                                0=beam crossing
            IW(IPYBC+JPYETY)      = 0
C           .................................... copy the vertex position
            CALL UCOPY(RBCR(1),RW(IPYBC+JPYEVX),3)
C           .......................................... copy the variances
C           ... covariance matrix 1 2 4
C           ...                     3 5
C           ...                       6
            CALL UCOPY(VRBCR(1),RW(IPYBC+JPYEVM),6)
C           ......... c2 chisquare 0.0 ...255.
            RW(IPYBC+JPYEC2) = 0
C           ..........  copy the number of degrees of freedom,
C                    2x2 for each track - 3 for vertex constraint
            IW(IPYBC+JPYEDF) = 1
          ENDIF
          IF(LGARB)      KPYER = IW(NAMIND('PYER'))
        ENDIF       ! end of putting beam crossing for new run
C       .... get beam crossing index
        KPYBC=IW(NAMIND('PYBC'))
        IF(KPYBC.GT.0) THEN
C         ...................... bank already exists
          IPYER = KROW(KPYBC,KLAST)
        ELSE
C         ..... this is a serious error
          IPYER = KROW(KPYER,1)
        ENDIF
      ELSE
        IPYER = KROW(KPYER,KVX)
      ENDIF
C
C     ...... transformation of coordinates and covariance matrices
C     ...... according to Drevermann rotation
      XC     = RW(IPYER+JPYEVX)
      XYZ(1) = XC
      YC     = RW(IPYER+JPYEVY)
      XYZ(2) = YC
      ZC     = RW(IPYER+JPYEVZ)
      XYZ(3) = ZC
C
      IF(TPR.EQ.'  ') RETURN
C
C
      IPYVM = IPYER + JPYEVM - 1
C
C
      IF(TPR.EQ.'YX') THEN
C
C       ................................. get the midpoint of the ellips/cross
        XCENT = XC
        YCENT = YC
        DO I=1,3
          RCOM(I)=RW(IPYVM+I)
        END DO
C
C
      ELSEIF(TPR.EQ.'RO') THEN
C
C
C
C       ......... apply global Rotation(if any) first to vertex
C       ..... (xc,yc,zc)->(xc,yc,zc)' as output
C
        X2= CF*XC+SF*YC
        Y2=-SF*XC+CF*YC
        XC= CT*X2+ST*ZC
        Z3=-ST*X2+CT*ZC
        YC= CG*Y2+SG*Z3
        ZC=-SG*Y2+CG*Z3
C       ................................. get the midpoint of the ellips/cross
        XCENT = XC
        YCENT = YC
C
C       ......... apply global Rotation second to covariance matrix
C       .... use CERNLIB Fxxx
C       .... unpack covariance matrix to symmetric matrix ar F112
        CALL TRUPCK(RW(IPYER+JPYEVM),AR,3)
C       .... ar' = rct(transposed) * ar * rct F004
        CALL RMMLT(3,3,3,AR(1,1),AR(1,2),AR(2,1),
     $          RCT(1,1),RCT(1,2),RCT(2,1),ZR(1,1),ZR(1,2),ZR(2,1),WORK)
        CALL RMMLT(3,3,3,RCT(1,1),RCT(2,1),RCT(1,2),
     $          ZR(1,1),ZR(1,2),ZR(2,1),AR(1,1),AR(1,2),AR(2,1),WORK)
C       .... pack symmetric matrix ar to covariance matrix
        CALL TRPCK(AR,RCOM,3)
C       ...... end of 3d rotation
C
C
      ELSEIF(TPR .EQ. 'YZ') THEN
C
C
C       .... rotation in x-y-plane  and y=y'=x2
        XCENT = ZC
        YCENT = -SF*XC + CF*YC
C       ......... apply Rotation to covariance matrix
        RCOM(1) =  RW(IPYVM+6)
        RCOM(2) = -SF*RW(IPYVM+4) + CF*RW(IPYVM+5)
        RCOM(3) =  SF**2 * RW(IPYVM+1) - 2.0*CF*SF * RW(IPYVM+2)
     $           + CF**2 * RW(IPYVM+3)
C
C
      ELSE
        DO I=1,6
          RCOM(I)=RW(IPYVM+I)
        END DO
      ENDIF

C
C
      IF(TPR.EQ.'YX' .OR. TPR.EQ.'RO' .OR. TPR.EQ.'YZ') THEN
C
C
C
C       ................ xcent , ycent define the midpoint of the ellips/cross
C       ... ? other definition for rmisc in 'YZ'
        RMISC = SQRT(XCENT**2+YCENT**2)
        CALL DVVEPA(XCENT,YCENT,RCOM,FAC,RMIN,RMAX,EPAR,PHIL,PHIU
     $                  ,A,B,CPHI,SPHI)
C
        IF(.NOT.LFAC) THEN
C         ....................... calculate points on ellips:
          DO I=1,NPT-1
            X     = A * XLKP(I)
            Y     = B * YLKP(I)
C           ............................................ positive rotation !!!
            XE(I) = XCENT +X*CPHI -Y*SPHI
            YE(I) = YCENT +X*SPHI +Y*CPHI
          END DO
          XE(NPT)=XE(1)
          YE(NPT)=YE(1)
        ELSE
C       .......................... cross-hairs on ellips
C       ....          (1,2)
C       .... (1,1)------|------(2,1)
C       ....          (2,2)
          AC     = A * CPHI
          AS     = A * SPHI
          H(1,1) = XCENT + AC
          V(1,1) = YCENT + AS
          H(2,1) = XCENT - AC
          V(2,1) = YCENT - AS
C
          BC     = B * CPHI
          BS     = B * SPHI
          H(1,2) = XCENT - BS
          V(1,2) = YCENT + BC
          H(2,2) = XCENT + BS
          V(2,2) = YCENT - BC
C
          DO I=1,NPT
            XE(I) = XCENT
            YE(I) = YCENT
          END DO
          XE(2)=H(1,1)
          YE(2)=V(1,1)
          XE(3)=H(2,1)
          YE(3)=V(2,1)
          XE(5)=H(1,2)
          YE(5)=V(1,2)
          XE(6)=H(2,2)
          YE(6)=V(2,2)
        END IF
C       ..... end of 'YX'
C
C
C
      ELSE IF(TPR.EQ.('FR')) THEN
C
C
C         ...... directly transform ellips points to phi,r
        CALL DVVEPA(XC,YC,RCOM,FAC,RMIN,RMAX,EPAR,PHIL,PHIU
     $               ,A,B,CPHI,SPHI)
        REMAX  = 0.0
        REMIN  = 1000.0
        PHIVX  = DATN2D(YC,XC)
        DO I=1,NPT-1
          X     = A * XLKP(I)
          Y     = B * YLKP(I)
C         .............................................. positive rotation !!!
          X1 = XC +X*CPHI -Y*SPHI
          Y1 = YC +X*SPHI +Y*CPHI
C         ...... directly transform ellips points to phi,r
          XE(I) = SQRT(X1**2 + Y1**2)
          IF(XE(I).GT.REMAX) REMAX = XE(I)
          IF(XE(I).LT.REMIN) REMIN = XE(I)
C         ........................................ 360 degrees period crossing
C         .................................................... angle of vertex
          YE(I) = DFINXT(PHIVX , DATN2D(Y1,X1))
        END DO

        XE(NPT)=XE(1)
        YE(NPT)=YE(1)
C       .... end of 'FR'
C
C
      ELSE IF( (TPR.EQ.('FZ')) .OR. (TPR.EQ.('RZ')) ) THEN
C
C
C       .... to each z slice - there have to be two phi's
        SIGZQ = RCOM(6)
        SIGZ  = FAC * SQRT(SIGZQ)
C       ... not checked for even npt!, npt should be odd
        ITOP  = (NPT+1)/2
C       ...... need the inverse of the kovariance matrix????
C       ...... invert packed symmetric matrix, to packed symmetric rinv
        CALL TRSINV(RCOM,RINV,3)
C       ...... rkovm cannot be kept, but must be the inverse of
C       ...... the left upper square of rinv
        RDET = RINV(1) * RINV(3) - RINV(2)**2
        IF(RDET.LE.0.0) THEN
C           ... ' matrix not positive definite', RDET
C           ... error code
          RDET = 1.0
        ENDIF
        RDET =  1.0 / RDET
        RKOVM(1)  =  RDET * RINV(3)
        RKOVM(2)  = -RDET * RINV(2)
        RKOVM(3)  =  RDET * RINV(1)
C
        DO I  = 1 , ITOP
          ZREL  = -SIGZ * XLKP(I)
C         ..... the midpoint,and fac, have to be found for each slice
          ZPX = RINV(4) * ZREL
          ZPY = RINV(5) * ZREL
C         ... the midpoint is dependant on zrel displaced
          BX   = -( RKOVM(1) * ZPX + RKOVM(2) * ZPY )
          BY   = -( RKOVM(2) * ZPX + RKOVM(3) * ZPY )
          XCS  = XC + BX
          YCS  = YC + BY
C         ...... need square of FAC ! here   xtAx - fac**2=0
          RCSL = FAC**2 - RINV(6)*ZREL**2 -
     $        (RINV(1) * BX**2 + 2.0*RINV(2)*BX*BY + RINV(3)*BY**2)
     $        -2.0 * ( BX*ZPX + BY*ZPY )
          IF(RCSL.LE.RIF) THEN
C           .... phi is defined by xcs/ycs
            PHIL = DATN2D(YCS,XCS)
            PHIU = PHIL
C           .... radius is
            RMIN = SQRT(XCS**2 + YCS**2)
            RMAX = RMIN
          ELSE
            CALL DVVEPA(XCS,YCS,RKOVM,RCSL,RMIN,RMAX,EPAR,PHIL,PHIU
     $                     ,A,B,CPHI,SPHI)
C           ................................. 360 degrees period crossing
            IF(PHIU .LT. PHIL) THEN
C             .... the vertex as reference
              IF(YC .LT. 0.0) THEN
                PHIU = PHIU + 360.0
              ELSE
                PHIL = PHIL - 360.0
              ENDIF
            ENDIF
          ENDIF
          XE(I)        = ZC + ZREL
          XE(NPT+1-I)  = XE(I)
C
          IF(TPR.EQ.('FZ')) THEN
            YE(I)        = PHIL
            YE(NPT+1-I)  = PHIU
          ELSE
C           ... RZ
            YE(I)        = RMIN
            YE(NPT+1-I)  = RMAX
          ENDIF
        END DO
C       ... end of 'FZ','RZ'
C
C
C
      ELSE IF(TPR.EQ.('FT')) THEN
C
C       ..... the z-offsett of the primary vertex can be tuned, it is passed as
C       ..... sca(1,1)
        ZC = ZC - ZOFF
C
C
C       ............................................... theta 0..180 degrees
C       .... need the inverse of the covariance matrix
C       .... invert packed symmetric matrix, to packed symmetric rinv
        CALL TRSINV(RCOM,RINV,3)
C       .... first find range of phi's
        CALL DVVEPA(XC,YC,RCOM,FAC,RMIN,RMAX,EPAR,PHIL,PHIU
     $               ,A,B,CPHI,SPHI)
C
C       ........ then to fixed phi calculate theta('s)
C
        ITOP  = (NPT+1)/2
C       ..... for the angles there has to be a special treatement because of
C       ..... zero crossing
C       ... instead of -1
        IF(PHIU .LT. PHIL) THEN
C         ................................. 360 degrees period crossing
C         .... the centre of phi-values, fimid
C         .... ranges from negative to positive angles
          FIMID =  0.5*(PHIU+PHIL-360.0)
        ELSE
          FIMID =  0.5*(PHIU+PHIL)
        ENDIF
        FISIG =  (PHIU-FIMID)*R1ME
C       .... xc A-1 (00z)
        ZECOF = XC*RINV(4) + YC*RINV(5) + ZC*RINV(6)
C
        DO I  = 1 , ITOP
          PHIT = FIMID -FISIG*XLKP(I)
          CFI  = COSD(PHIT)
          SFI  = SIND(PHIT)
C         ... xc A-1 (xy0)
          ZFCOF = XC*(CFI*RINV(1)+SFI*RINV(2))+
     $            YC*(CFI*RINV(2)+SFI*RINV(3))+
     $            ZC*(CFI*RINV(4)+SFI*RINV(5))
C         ... const -fac, xc A-1 xc
C         need square of fac here too
          ZGCOF = -FAC**2 + XC**2*RINV(1)+ YC**2*RINV(3)+ ZC**2*RINV(6)
     $      +         2.0*( XC*YC*RINV(2)+XC*ZC*RINV(4)+ZC*YC*RINV(5))
C         ... (00z) A-1 (00z)
          ZHCOF = RINV(6)
C         ... (xy0) A-1 (00z)
          ZICOF = CFI*RINV(4) + SFI*RINV(5)
C         ... (xy0) A-1 (xy0)
          ZJCOF = CFI**2*RINV(1) +2.0*CFI*SFI*RINV(2) +SFI**2*RINV(3)
          ZPA   = 4.0 * (ZECOF**2    - ZGCOF*ZHCOF)
          ZPB   = 8.0 * (ZECOF*ZFCOF - ZGCOF*ZICOF)
          ZPC   = 4.0 * (ZFCOF**2    - ZGCOF*ZJCOF)
C         .... nullstellen
C         .... discriminant
          RCSID = ZPB**2 - 4.0*ZPA*ZPC
          IF(RCSID .GE. 0.0)THEN
            RCSID = SQRT(RCSID)
C           ... !!! check if not zero
            ZPA  = 0.5 / ZPA
            ZER1 = ZPA * ( - ZPB + RCSID)
            ZER2 = ZPA * ( - ZPB - RCSID)
C           ..... this is the cotangens of the solution
            THET1 = DSCAL * (PIBY2 - ATAN(ZER1))
            THET2 = DSCAL * (PIBY2 - ATAN(ZER2))
          ELSE
C           .... no solutions
C           .... so put theta of vertex
            RTHE =   DATN2D(ZC,SQRT(XC**2+YC**2))
            THET1 = 90.0 - RTHE
            IF(RTHE .GT. 90.0) THET1 = THET1+360.0
            THET2 = THET1
          ENDIF
          XE(I)        = THET1
          XE(NPT+1-I)  = THET2
          YE(I)        = PHIT
          YE(NPT+1-I)  = YE(I)
        ENDDO
C       .... to  close the ellips
        XE(61) = XE(1)
C       ..... end of 'FT'
      END IF
      RETURN
C
C
C
      ENTRY DVVXHA(KVX,XEL,YEL,ZEL)
C
C
C
C     ... calculate      IF POSSIBLE:1=CENTER 2=LONGEST  4=SHORTEST HAUPTACHSE
C     ... hauptachsen und Zentrum der Ellipse im 3d Raum
C     ........ KVX is the number of the Vertex
C
      IF(KVX.EQ.0) then
C       .... get beam crossing index
        KPYBC=IW(NAMIND('PYBC'))
        IF(KPYBC.GT.0) THEN
C         ...................... bank really exists
          IPYER = KROW(KPYBC,KLAST)
        ELSE
C         ..... this is a serious error
          IPYER = KROW(KPYER,1)
        ENDIF
      ELSE
        IPYER  = KROW(KPYER,KVX)
      ENDIF
      XEL(1) = RW(IPYER+JPYEVX)
      YEL(1) = RW(IPYER+JPYEVY)
      ZEL(1) = RW(IPYER+JPYEVZ)
C     .... use CERNLIB
C     .... invert packed symmetric matrix
      CALL TRSINV(RW(IPYER+JPYEVM),RINV,3)
C     .... unpack inverse covariance matrix to symmetric matrix rkvi
      CALL TRUPCK(RINV,RKVI,3)
C     .... evaluate eigenvalues wr and eigenvectors zr by eispack
      CALL EISRS1(3,3,RKVI,WR,ZR,IERR,WORK)
C     ... it's a known problem that sometimes the covariance matrix is not
C     positiv definite, because of loss of precision, then no inverse exist
C     ... -> ierr .ne. 0
      DO I = 1,3
        RBETR(I) = SQRT(1.0/MAX(WR(I),1.0E-16))
        XEL(1+I) = RBETR(I) * ZR(1,I) + XEL(1)
        YEL(1+I) = RBETR(I) * ZR(2,I) + YEL(1)
        ZEL(1+I) = RBETR(I) * ZR(3,I) + ZEL(1)
      END DO
C
      RETURN
C
C
      ENTRY DVVXRN(NWRUN)
C
C     ..... use for run initialization of beam crossing (interaction point?)
C     ..... the run number NWRUN has to be known, stored
      IF (NWRUN.NE.KRUN) THEN
        KRUN = NWRUN
        FNWR =.TRUE.
      ENDIF
C
      RETURN
C
C
      ENTRY DVVXBN(NBPY)
C     ..... select other Banknumber of PYER-bank, default=0
      NRPY = NBPY
C
      RETURN
C     ..... end of deck DVVX
      END


      SUBROUTINE DVVEPA(XCENT,YCENT,RKOVM,FAC,RMIN,RMAX,EPAR,PHIL,PHIU
     $                      ,A,B,CPHI,SPHI)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Jochen Lauber         12-AUG-1991, 2-sep-1991
C!
C!   Inputs:
C!        - xcent,ycent  midpoint of ellips
C!        - rkovm        covariance matrix (defining correlation -> a, b ,
C!        -                   inclination)
C!        - fac          defining size in units of sigma
C!        -
C!        -
C!
C!   Outputs:
C!        -
C!        - rmin        nearest point to origin on a ellips
C!        - rmax        farest  point to origin on a ellips
C!        - a , b       the  axis of the ellips
C!        - cphi, sphi  cos,sin(alpha)
C!        -
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!    calculate a,b and angle alpha of ellips from x/y-correlation:
C!    and the scissors ...
C?
C!======================================================================
C
C
      DIMENSION RKOVM(3)

      PARAMETER (PI=3.141592653589)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (DSCAL = 57.2957795130824)
      PARAMETER (PI270 = 1.5 * PI)
      LOGICAL LPRIM
      DATA RPHCR /1.0/     ! 1 degrees
C     ................. after declarations
      SIGQX = RKOVM(1)
      SIGQY = RKOVM(3)
      COVXY = RKOVM(2)

      IF(SIGQX .NE. SIGQY) THEN
        TALPHA = 2.  * COVXY / (SIGQX-SIGQY)
        ALPHA  = 0.5 * ATAN(TALPHA)
      ELSE
C       .... seems to be arbitrary, not covered by mathematics rather +-
C       .... 45DEGREES
        ALPHA = SIGN(0.785398163,COVXY)
      END IF
      CPHI  = COS(ALPHA)
      SPHI  = SIN(ALPHA)
      CPHIQ = CPHI**2
      SPHIQ = SPHI**2
      CSPHI = 2.*COVXY*CPHI*SPHI
      RDIND = SIGQX*SIGQY - COVXY**2
      IF(RDIND.GT.0) THEN
C       ...... by mathematics the divisors should be greater zero
        RDIV1  = SIGQY*CPHIQ  - CSPHI + SIGQX*SPHIQ
        RDIV2  = SIGQX*CPHIQ  + CSPHI + SIGQY*SPHIQ
C       ...... A and B are the half axes, but not! a>b
        A      = FAC * SQRT(RDIND / RDIV1)
        B      = FAC * SQRT(RDIND / RDIV2)
      ELSE
C       ............ if korrelation rho =+-1 then ellips goes to straight
C       ...............  line !
        RHA      = SQRT( SIGQX + SIGQY )
        IF( ALPHA .GE. 0.0  ) THEN
          A      = FAC * RHA
          B      = 0.0
        ELSE
          A      = 0.0
          B      = FAC * RHA
        END IF
      END IF
C     .... inclination of the longer halfaxis, ranges from -45 to 135 degrees
C     .... the parameters of ellips e=c/a   p=b**2/a
      AQ = A*A
      BQ = B*B
      IF(A.GT.B) THEN
        PHINC = ALPHA
        CPAR  = SQRT( AQ - BQ )
        AINV  = 1./A
        EPAR  = CPAR*AINV
        PPAR  = BQ*AINV
        APAR  = A
        BPAR  = B
      ELSE
        PHINC = ALPHA + PIBY2
        CPAR  = SQRT( BQ - AQ )
        AINV  = 1./B
        EPAR  = CPAR*AINV
        PPAR  = AQ*AINV
        APAR  = B
        BPAR  = A
      END IF
      CINCL = COS(PHINC)
      SINCL = SIN(PHINC)
C     ...... the nearest point to origin on the crosses
      AVE1 =  APAR * CINCL
      AVE2 =  APAR * SINCL
      CALL DVVLNP(XCENT,YCENT,AVE1,AVE2,RNRAQ,RFRAQ,RENAQ)
      BVE1 = -BPAR * SINCL
      BVE2 =  BPAR * CINCL
      CALL DVVLNP(XCENT,YCENT,BVE1,BVE2,RNRBQ,RFRBQ,RENBQ)
C     .....
C     ..... the two centers of the ellips
      XC1 = XCENT + CPAR*CINCL
      YC1 = YCENT + CPAR*SINCL
      XC2 = XCENT - CPAR*CINCL
      YC2 = YCENT - CPAR*SINCL
C     ..... find nearest and farest center to origin
      IF((XC1**2+YC1**2) .GT. (XC2**2+YC2**2)) THEN
        XCN=XC2
        YCN=YC2
        XCF=XC1
        YCF=YC1
      ELSE
        XCN=XC1
        YCN=YC1
        XCF=XC2
        YCF=YC2
      END IF
C     ......
      IF(EPAR .LE. 0.01) THEN
C     ..... ellips is circle
        RC   = SQRT( XCENT**2 + YCENT**2)
        RMIN = RC - APAR
        RMAX = RC + APAR
      ELSEIF(EPAR .GE. 0.99)THEN
C     ..... ellips is straight line
        RMIN = SQRT(MIN(RNRAQ,RNRBQ))
        RMAX = SQRT(RFRAQ)
      ELSE
        PH1   = PIBY2 + PHINC
C       .... mimimum distance
        RPAR  = EPAR
        CALL DVVENV(PH1,XCN,YCN,PPAR,RR2Q,RQMA)
        CALL DVVENF(-1,RPAR,RENBQ,RR2Q,RENAQ,RMIN)
        PHMIN = RPAR
C       .... maximum distance
        RPAR  = EPAR
        CALL DVVENV(PH1,XCF,YCF,PPAR,RQMI,RR2Q)
        CALL DVVENF(1,RPAR,RFRBQ,RR2Q,RFRAQ,RMAX)
        PHMAX = RPAR
      ENDIF
C
C
C
C     ... find angles of scissors touching ellips: phil,phiu
C
C     .... using rkovm,xcent,ycent ... fac
C     .... we need the inverse 'aii' of the covariance matrix
      RDET = RKOVM(1) * RKOVM(3) - RKOVM(2)**2
      IF(RDET.LE.0.0) THEN
C       ... error code
        RDET = 1.0
      ENDIF
C     ..... a factor of 0.5 has to be introduced !?
      RDET =  1.0 / RDET
      A11  =  RDET * RKOVM(3)
      A12  = -RDET * RKOVM(2)
      A22  =  RDET * RKOVM(1)
C     ...... translation vector, sign
      A1   = (XCENT*A11 + YCENT*A12)
      A2   = (YCENT*A22 + XCENT*A12)
C     ...... constant term, sign
C     ......   square of FAC, again !
      ACHE = - FAC**2 + XCENT*A1 + YCENT*A2
C     ..... sign of ache decides whether origin is inside ellips or outside
C     .... ache >0 origin outside
C     .... ache <=0 origin on or inside ellips
C
      IF (ACHE.LE.0.0) THEN
C       .... if origin is inside ellips  then rmin should be set to zero
        RMIN = 0.0
      ENDIF
C
      A1   = -2.0 * A1
      A2   = -2.0 * A2
C     ...... Nullstellen
      ACHE = ACHE * 4.0
      ZPA  = A2**2 - ACHE * A22
      ZPB  = 2.0 * (A1 * A2 - ACHE * A12)
      ZPC  = A1**2 - ACHE * A11
C     .... discriminant
      RCSID = ZPB**2 - 4.0*ZPA*ZPC
C       ..... angle of midpoint
      PHIP = DATN2D(YCENT,XCENT)
C     ..... angle to nearest center
      PHIN = DATN2D(YCN,XCN)
C     ...... angle to farest center
      PHIF = DATN2D(YCF,XCF)
      IF(RCSID .GE. 0.0)THEN
        RCSID = SQRT(RCSID)
        IF(ZPA.NE.0.0)THEN
          ZPA = 0.5 / ZPA
        ELSE
C       ..... error code !
        ENDIF
        ZER1 = ZPA * (-ZPB+RCSID)
        ZER2 = ZPA * (-ZPB-RCSID)
C       ..... this is the tangens of the solution, still + pi ambiguity
        PHI1 = DSCAL*ATAN(ZER1)
        PHI2 = DSCAL*ATAN(ZER2)
        IF(PHI1 .LT. PHI2 ) THEN
C           .... exchange
          PHIME = PHI1
          PHI1  = PHI2
          PHI2  = PHIME
        ENDIF
        LPRIM = .FALSE.
C       .... first reflect phip to the range -90,90 degrees as phi1,phi2
        DO I=1,2
          IF(PHIP .GT. 90.0) THEN
            PHIP  = PHIP-180.0
            LPRIM = .NOT. LPRIM
          ENDIF
        ENDDO
        IF( ABS(PHIF-PHIN) .LT. RPHCR) THEN
          IF(LPRIM)THEN
            PHIU=PHI1+180.0
            PHIL=PHI2+180.0
          ELSE
            PHIU=PHI1
            PHIL=PHI2
          ENDIF
        ELSE
          IF(PHIP.GT.PHI1)THEN
            IF(LPRIM)THEN
              PHIU=PHI2
              PHIL=PHI1+180.0
            ELSE
              PHIU=PHI2+180.0
              PHIL=PHI1
            ENDIF
          ELSE        IF(PHIP.GE.PHI2)THEN
            IF(LPRIM)THEN
              PHIU=PHI1+180.0
              PHIL=PHI2+180.0
            ELSE
              PHIU=PHI1
              PHIL=PHI2
            ENDIF
          ELSE
            IF(LPRIM)THEN
              PHIU=PHI2+180.0
              PHIL=PHI1
            ELSE
              PHIU=PHI2
              PHIL=PHI1+180.0
            ENDIF
          ENDIF
        ENDIF
        IF(PHIL .LT. 0.0) PHIL = 360.0 + PHIL
        IF(PHIU .LT. 0.0) PHIU = 360.0 + PHIU
C       ....
      ELSE
C       ... error return code ? off shell
C       ...  ' No solution '
        IF(PHIF.LT.PHIN) THEN
          PHIL = PHIF
          PHIU = PHIN
        ELSE
          PHIL = PHIN
          PHIU = PHIF
        ENDIF
      ENDIF
C     ....... end of DVVEPA
      END

      SUBROUTINE DVVENV(PH1,XCN,YCN,PPAR,RQMI,RQMA)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Jochen Lauber          3-SEP-1991
C!
C!   Inputs:
C!        -       PH1  = PIBY2+PHINC , angle perpendicular to longer axis
C!        -       xcn,ycn  coordinates of the nearest to origin center
C!        -       ppar p-parameter in local radial representation of the ellips
C!   Outputs:
C!        -      rqmi square minor distance
C!        -      rqma square major distance
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   find nearest  to origin of the two points perpendicular to the longer axis
C!   over the nearest center.
C!   Corresponds to   phi=pi/2 or 3pi/2 inside local radial representation of
C!   the ellips
C?
C!======================================================================
*IF .NOT.DOC
      CPH1 = COS(PH1)
      SPH1 = SIN(PH1)
      XR1  = XCN + PPAR*CPH1
      YR1  = YCN + PPAR*SPH1
      XR1A = XCN - PPAR*CPH1
      YR1A = YCN - PPAR*SPH1
C
      RQ1   = (XR1**2  + YR1**2)
      RQ2   = (XR1A**2 + YR1A**2)
      IF(RQ1 .GT. RQ2 ) THEN
        RQMI=RQ2
        RQMA=RQ1
      ELSE
        RQMI=RQ1
        RQMA=RQ2
      END IF
      END

      SUBROUTINE DVVLNP(XC,YC,AVE1,AVE2,RNEAQ,RFARQ,RENQ)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Jochen Lauber          2-SEP-1991
C!
C!   Inputs:
C!        -    xc,yc      midpoint of ellips
C!        -    ave1,ave2  direction vector with length of halfaxis a
C!
C!   Outputs:
C!        -  all distances are squared !!
C!        -  rneaq          nearest
C!        -  rfarq          farest
C!        -  renq           opposite to farest
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!    calculates the nearest and the farest point on a line within the range of
C!    +- the length of a (ellips halfaxis) around the midpoint
C?
C!======================================================================
*IF .NOT.DOC
      AVEQ = AVE1**2 + AVE2**2
      IF (AVEQ .NE. 0.0) THEN
        RLAMB = -(AVE1 * XC + AVE2 * YC) / AVEQ
        IF(ABS(RLAMB) .GT. 1.0) RLAMB = SIGN(1.0,RLAMB)
      ELSE
        RLAMB = 0.0
      END IF
C     ..... nearest
      RNEAQ = (XC + RLAMB * AVE1)**2 + ( YC + RLAMB * AVE2)**2
C     ..... distances of endpoints of axis
      RFARQ = (XC-AVE1)**2+(YC-AVE2)**2
      RENQ  = (XC+AVE1)**2+(YC+AVE2)**2
      IF(RLAMB.LT.0.0) THEN
C     .... exchange
        RMEM  = RFARQ
        RFARQ = RENQ
        RENQ  = RMEM
      END IF
      END

      SUBROUTINE DVVENF(IMA,RPAR,RR1Q,RR2Q,RR3Q,REXT)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Jochen Lauber          3-SEP-1991
C!
C!   Inputs:    rpar=c/a
C!        -     rr1q,rr2q,rr3q squares of radii
C!        -     ima >0 search maximum
C!   Outputs:
C!        -     rpar as place of min/max
C!        -     rext   (minimum/maximum)
C!        -
C!        -
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!    interpolates 3 points by a parabola and gives extremum (minimum or
C!    maximum)
C!       call only if rpar>0., else circle and min/max is rc+-apar
C!       call only if rpar<1., else line
C?
C!======================================================================
*IF .NOT.DOC
C     ..... now there are three points (ph(1,2,3),rr(1,2,3)q) defining parabel
C     ..... the maximum is given by
C     ..... ph1 = 0 ph2 = rpar ph3=1.
      YBX21  = (RR2Q - RR1Q) / RPAR
      Y31    =  RR3Q - RR1Q
      PARA   = (Y31 - YBX21)/(1. - RPAR)
      PARB   =  Y31 - PARA
C     .... para=0 : all rq values are equal!! or on a straight line !

      IF(PARA .NE. 0.0) GOTO 333
      PHEXT  = -0.5 * PARB/PARA
C       ..... phext must be between 0. and 1.
      RPAR   = PHEXT
      IF(PHEXT.LT.0.0 .OR. PHEXT.GT.1.0) GO TO 333

C     .... check for edge extrema
      IF(IMA.GT.0) THEN
C       .... para<0  maximum
        IF(PARA.GE.0) THEN
          REXT   = SQRT(MAX(RR1Q,RR2Q,RR3Q))
          RETURN
        ENDIF
      ELSE
C       .... para>0  minimum
        IF(PARA.LE.0) THEN
          REXT   = SQRT(MIN(RR1Q,RR2Q,RR3Q))
          RETURN
        ENDIF
      ENDIF
C     ... propper extremum
      RRMAQ  = RR1Q + 0.5 * PARB * PHEXT
C     ..... problem case !
      IF(RRMAQ.LT.0.0) GOTO 333
      REXT   = SQRT(RRMAQ)
      RETURN
  333 IF(IMA.GT.0) THEN
        REXT   = SQRT(MAX(RR1Q,RR2Q,RR3Q))
      ELSE
        REXT   = SQRT(MIN(RR1Q,RR2Q,RR3Q))
      ENDIF
      END
C======================================= code before 16.1.2002
c!======================================================================c
cc*IF .NOT.DOC
cC     ..... now there are three points (ph(1,2,3),rr(1,2,3)q) defining parabel
cC     ..... the maximum is given by
cC     ..... ph1 = 0 ph2 = rpar ph3=1.
c      YBX21  = (RR2Q - RR1Q) / RPAR
c      Y31    =  RR3Q - RR1Q
c      PARA   = (Y31 - YBX21)/(1. - RPAR)
c      PARB   =  Y31 - PARA
cC     .... para=0 : all rq values are equal!! or on a straight line !
c      IF(PARA .EQ. 0.0) GOTO 333
c      PHEXT  = -0.5 * PARB/PARA
cC       ..... phext must be between 0. and 1.
c      RPAR   = PHEXT
c      IF(PHEXT.LT.0.0 .OR. PHEXT.GT.1.0) THEN
cC       .......................... jump into loop 7.10.97 is o.k. HD
c  333   IF(IMA.GT.0) THEN
c          REXT   = SQRT(MAX(RR1Q,RR2Q,RR3Q))
c        ELSE
c          REXT   = SQRT(MIN(RR1Q,RR2Q,RR3Q))
c        ENDIF
c        RETURN
c      ENDIF
cC     .... check for edge extrema
c      IF(IMA.GT.0) THEN
cC       .... para<0  maximum
c        IF(PARA.GE.0) THEN
c          REXT   = SQRT(MAX(RR1Q,RR2Q,RR3Q))
c          RETURN
c        ENDIF
c      ELSE
cC       .... para>0  minimum
c        IF(PARA.LE.0) THEN
c          REXT   = SQRT(MIN(RR1Q,RR2Q,RR3Q))
c          RETURN
c        ENDIF
c      ENDIF
cC     ... propper extremum
c      RRMAQ  = RR1Q + 0.5 * PARB * PHEXT
cC     ..... problem case !
c      IF(RRMAQ.LT.0.0) GOTO 333
c      REXT   = SQRT(RRMAQ)
c      END
C=================================================================
*DK DVWI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVWI
CH
      SUBROUTINE DVWI(PHI, NUMW,WR,WZ,WE)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C! Find wire coordinates for laser tracks.
C!
C! INPUT :  PHI    = phi of laser tracks (in degrees)
C! OUTPUT:  NUMW   = number of wire hits
C!          WR     = radial positions of hits
C!          WZ     = Z coordinates of hits
C!          WE     = dE/dx measurements on wires - not yet implemented
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'J_TPNAMC.INC'
      INCLUDE 'A_TPCOJJ.INC'
      INCLUDE 'A_TWPUJJ.INC'
      INCLUDE 'A_TCONJJ.INC'
      DATA PIDEG/57.29577951/
      DIMENSION WR(2000),WZ(2000),WE(2000)
      CHARACTER *80 T80
C
C++   Note: the description of the TPC geometry is idealistic, and corre
C++   to setup of JULIA for November 1987.
C++   This may require review in the future.
C
C++   ISECT contains the sector numbers crossed by the laser tracks.
C++   WIRE1 contains the position of first wire in inner/outer sector.
C++   WSTEP contains the wire spacing in inner/outer sector.
C++   VDRFT0 is the default drift velocity in the absence of TCON bank.
C++   ZTPCMX is the half length of the TPC.
C
      DIMENSION ISECT(3,2), WIRE1(2), WSTEP(2)
      DATA ISECT / 20,24,22, 28,36,32 /
      DATA WIRE1, WSTEP / 32.2,95.2, 0.400,0.400 /
      DATA VDRFT0, ZTPCMX / 0.505, 220.0 /
      DATA SCALEF / 20. /
*CA BCS
*CA BMACRO
      INCLUDE 'A_BCS.INC'
      INCLUDE 'J_RCURNT.INC'
      INCLUDE 'A_BMACRO.INC'
C
C++   Identify the laser beams at 12, 8 and 4 o'clock: IPHI = 1,2,3.
C
      NUMW=0
C      IF(BNUMDB(2,TWPUDB).EQ.0..OR.BNUMDB(4,TWPUDB).LE.0.) RETURN
      IPHI = NINT( (PHI+30.)/120. )
C
C ++  Find drift velocity. If it is not available, use a default.
C
      ITCON = IW(NAMIND('TCON'))
      IF(ITCON.NE.0) THEN
         TVDRFT = RTABL(ITCON,1,JTCODV) * RTABL(ITCON,1,JTCOSL)
      ELSE
         TVDRFT = VDRFT0
         WRITE(T80,*)'WARNING: no TCON banks, use v(drift) = ',TVDRFT
         CALL DWRT(T80(1:LENOCC(T80)))
      ENDIF
C
C++   Loop over sectors and then over wires in each sector.
C++   Find Rho and Z.
C++   For Rho, need secant of phi angle relative to normal to wires.
C++   Also store the pulse heights in the array WE, dividing them
C++   by the scale factor SCALEF.
C
      PHIREL = PHI + 30. - IPHI*120.
      SECPHI = 1. / COS(PHIREL/PIDEG)
      K = 0
C
      DO 710 IS=1,2
         KSECT = ISECT(IPHI,IS)
         ITWPU = NLINK('TWPU',KSECT)
         NUMWS = LROWS(ITWPU)
         IF(ITWPU.EQ.0 .OR. NUMWS.EQ.0) GO TO 710
         CONS1 = SECPHI * WIRE1(IS)
         CONS2 = SECPHI * WSTEP(IS)
         IF(KSECT.LE.18) THEN
            SIGNZ = +1.
         ELSE
            SIGNZ = -1.
         ENDIF
         DO 700 IWHIT=1,NUMWS
            K = K + 1
            WR(K) = CONS1 +
     &        CONS2 * FLOAT( ITABL(ITWPU,IWHIT,JTWPWN)-1 )
            WZ(K) = SIGNZ * (TVDRFT*RTABL(ITWPU,IWHIT,JTWPTE)-ZTPCMX)
            WE(K) = FLOAT(ITABL(ITWPU, IWHIT, 5))/SCALEF
  700    CONTINUE
  710 CONTINUE
C
      NUMW = K
C
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVWIAL
CH
      ENTRY DVWIAL(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   29-MAY-1989
C
C!:
C    Inputs    :NO
C    Outputs   :# OF PULSE ON WIRES
C
C    Called by :DEVSET
C ---------------------------------------------------------------------
      NUM=0
      DO 720 KSECT=1,36
         ITWPU = NLINK('TWPU',KSECT)
         IF(ITWPU.GT.0) NUM=NUM+LROWS(ITWPU)
  720 CONTINUE
      END
*DK DVDFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVDFT
CH
      SUBROUTINE DVDFT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C      INCLUDE 'DGCOMN.INC'
      INCLUDE 'DALI_CF.INC'
C               FACEDV(SIDE,X or Y,PHI,LAYER)
      DIMENSION FACE(2,2,15,2)
      EQUIVALENCE (FACE,CRILDV)
      DIMENSION NL(2),JF(2)
      DATA IDEB/0/
      DATA NL/12,15/
      DATA NP/12/
      DATA IL/1/,JF/2,2/,N2/2/,N8/8/
      DIMENSION IC(4),ICOL(0:7)
      DATA ICOL/9,11,8,13,15,10,12,14/
      DATA IC/0,0,1,1/
      NCOL=9
      DZ=ZALWDV*0.5
      DO NZ=1,4
        Z1=ZOFWDV(NZ)-DZ
        Z2=ZOFWDV(NZ)+DZ
        DO NF=1,NL(IL)
          IF(NWTSDV(NF,IL).NE.0) THEN
            NC=MOD(N2*NF+IC(NZ),N8)
            CALL DGLEVL(ICOL(NC))
            X1=FACE(1,1,NF,IL)
            X2=FACE(1,2,NF,IL)
            Y1=FACE(2,1,NF,IL)
            Y2=FACE(2,2,NF,IL)
            R1=SQRT(X1*X1+Y1*Y1)
            F1=DATN2D(Y1,X1)
            T1=DATN2D(R1,Z1)
            T2=DATN2D(R1,Z2)
            R2=SQRT(X2*X2+Y2*Y2)
            F2=DATN2D(Y2,X2)
            IF(F2.GT.F1) F2=F2-360.
            S1=DATN2D(R2,Z1)
            S2=DATN2D(R2,Z2)
            CALL DQL2E(-T1,F1,-T2,F1)
            CALL DQL2E(-S1,F2,-S2,F2)
            IF(NF.EQ.JF(IL)) THEN
              CALL DQL2E(-T1,F1+360.,-T2,F1+360.)
              CALL DQL2E(-S1,F2+360.,-S2,F2+360.)
            END IF
            IF(IDEB.EQ.1) CALL DQL2E(-T1,F1,-S1,F2)
            IF(IDEB.EQ.1) CALL DQL2E(-T2,F1,-S2,F2)
            Q=1./FLOAT(NP)
            DX=(X2-X1)*Q
            DY=(Y2-Y1)*Q
            X=X1
            Y=Y1
            RO1=SQRT(X*X+Y*Y)
            FI1=DATN2D(Y,X)
            TL1=DATN2D(RO1,Z1)
            TR1=DATN2D(RO1,Z2)
            DO K=1,NP
              X=X+DX
              Y=Y+DY
              RO2=SQRT(X*X+Y*Y)
              FI2=DATN2D(Y,X)
              IF(FI2.GT.FI1) FI2=FI2-360.
              TL2=DATN2D(RO2,Z1)
              TR2=DATN2D(RO2,Z2)
              IF(NF.EQ.JF(IL)) THEN
                CALL DQL2E(-TL1,FI1+360.,-TL2,FI2+360.)
                CALL DQL2E(-TR1,FI1+360.,-TR2,FI2+360.)
              END IF
              CALL DQL2E(-TL1,FI1,-TL2,FI2)
              CALL DQL2E(-TR1,FI1,-TR2,FI2)
              TL1=TL2
              TR1=TR2
              FI1=FI2
            END DO
          END IF
        END DO
      END DO
      END
C**********************************************************************
C#######################################################################
      SUBROUTINE DVEXS0(NTR)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Ilias T. Efthymiopou  18-MAR-1991
C!
C!   Inputs:
C!        -
C!
C!   Outputs:   NTR     /I      Number of tracks if the banks exist
C!                              0 if not
C!        -
C?
C!======================================================================
      INCLUDE 'A_BCS.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INCLUDE 'A_BMACRO.INC'
C------------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NTEXS=NAMIND('TEXS')
        NFRFT=NAMIND('FRFT')
      END IF
      KTEXS=IW(NTEXS)
      KFRFT=IW(NFRFT)
      IF ( KTEXS.EQ.0 ) THEN
        NTR = 0
      ELSE
        NTR=LROWS(KFRFT)
      END IF
  999 RETURN
      END
C#######################################################################
      SUBROUTINE DVTION(RI,NS,TL,N,RIEXP,SIGMA,DION,CHI)
C
C-----------------------------------------
C
C   Author   :- Ilias T. Efthymiopou  14-JUL-1991
C
C=========================================
C
C
C   Inputs    : RI      /R      The dE/dx value for the track
C               NS      /I      The number of dE/dx samples
C               TL      /R      The track length
C               N       /I      The number of hypotheses to try
C               RIEXP(n)/R      The expected dE/dx for each
C               SIGMA(n)/R      The error for each particle hypothesis
C   Outputs   : DION    /R      The dE/dx error
C               CHI(n)  /R      The (ri-riexp)/sigma for each hypothesis
C=========================================================================
      DIMENSION RIEXP(N),SIGMA(N),CHI(N)
C-------------------------------------------------------------------------
      CALL VZERO(CHI,N)
      DO 10  I=  1,  N
        IF ( SIGMA(I).NE.0. ) THEN
          DIFF = RI-RIEXP(I)
          XX = ABS(DIFF)/SIGMA(I)
          XX = MIN(XX,9.99)
          CHI(I) = SIGN(XX,DIFF)
        ELSE
          CHI(I) = 999.
        END IF
   10 CONTINUE
      RSIG = TDXERR(NS,TL,IER)
      IF ( IER.EQ.0 ) THEN
        DION = RSIG*RI
      ELSE
        DION = 99.
      ENDIF
  999 RETURN
      END
C**********************************************************************
      SUBROUTINE DVTHYP(ITK,FIELD,N,RMASS,Q,RI,NS,TL,RIEXP,SIGMA,IER)
C      SUBROUTINE TIDHYP(ITK,FIELD,N,RMASS,Q,RI,NS,TL,RIEXP,SIGMA,IER)
C
C----------------------------------------------------------------------
C! Particle ID hypothesis from TPC dE/dx
C! This is a user interface for dE/dx analysis.
C!
C!    Author:  R. Johnson    23-09-88
C!    Modified:S. Haywood    07-06-90
C!    Modified :- Ilias T. Efthymiopou  14-JUL-1991
C!
C!    Input:   ITK      /I       Track number in FRFT or DTRA bank
C!             FIELD    /R       Magnetic field (sign doesn't matter)
C!             N        /I       Number of hypothesis to try
C!             RMASS(n) /R       Mass hypotheses
C!             Q(n)     /R       Charge hypotheses (sign doesn't matter)
C!    Output:  RI       /R       Measured ionization (1.O=minion, Q=1)
C!             NS       /R       Number of useful wire samples on track
C!             TL       /R       Useful length of the track (cm)
C!             RIEXP(n) /R       Expected ionization for the given
C!                               mass hypothesis (1.0=minion, Q=1)
C!             SIGMA(n) /R       Sigma of dE/dx measurement error,
C!                               including the momentum error.
C!                               Note that one can calculate a
C!                               chi-squared with 1 d.o.f. as:
C!                               chi2 = ((RI-RIEXP)/SIGMA)**2
C!             IER      /I       Error return= 0 for success.
C!                               1= can't find track bank
C!                               2= can't find dE/dx bank
C!                               3= track has no dE/dx information
C!                               4= can't find calibration banks
C!                                  TC1X, TC2X, and/or TC3X
C!                               5= cannot find RUNH or EVEH bank
C!                                  from which to get the run number
C!                               6= no valid dE/dx calibration exists
C!                                  for this run
C!  Input data banks:
C!         FRFT or DTRA    for tracking information
C!         TEXS or DTRA    for dE/dx information
C!                         NOTE:  if all 3 of these banks exist, the
C!                                program will use FRFT and TEXS.
C!  Input calibration banks:
C!         TC1X, TC2X, and TC3X
C!
C!  Comments
C!  ========
C!  This interface is to be used for analysis of dE/dx information
C!  contained on POT, DST, or MDST data files.  One provides the
C!  track number and one or more particle hypotheses, and the
C!  program returns the corresponding measured dE/dx, expected
C!  dE/dx, and the estimated 1 sigma measurement error.  All
C!  calibration factors valid for the current run are applied.
C!  The program also returns the number of dE/dx samples of the track
C!  and the effective length of the track.
C!
C!  I.E. the modification is to add a common block with the dE/dx per sector
C!  of the track with the correct normalization.
C!
C-------------------------------------------------------------------
C
C!========= MODIFICATION(  I.E. ) ===============================
      INCLUDE 'DALI_CF.INC'
C!===============================================================
      INCLUDE 'A_RUNHJJ.INC'
      INCLUDE 'A_EVEHJJ.INC'
      INCLUDE 'A_REVHJJ.INC'
      INCLUDE 'A_ALCONS.INC'
      INCLUDE 'A_BCS.INC'
C*CA DTRAJJ
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTREM=12,JDTRTF=16,JDTRHO=17,JDTRHM=18,
     +          JDTRDE=19,JDTRNS=20,JDTRTL=21,JDTRVB=22,JDTRDC=23,
     +          LDTRAA=23)
C*CC DTRAJJ
      PARAMETER (EFACTM = 1000.)
      DIMENSION RMASS(N),Q(N),RIEXP(N),SIGMA(N)
      INCLUDE 'A_FRFTJJ.INC'
      INCLUDE 'A_TC3XJJ.INC'
      INTEGER ALGTDB
      LOGICAL FIRST,FMDST
      DATA FIRST/.TRUE./
C
C++   Spacing between sense wires in the TPC in cm
C
      DATA TWRSP/0.4/
C
      INCLUDE 'A_BMACRO.INC'
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NFRFT=NAMIND('FRFT')
        NDTRA=NAMIND('DTRA')
        NTEXS=NAMIND('TEXS')
        NRUNH=NAMIND('RUNH')
        NEVEH=NAMIND('EVEH')
        NREVH=NAMIND('REVH')
        NTC3X=NAMIND('TC3X')
        NEVEH=NAMIND('EVEH')
        NRUNH=NAMIND('RUNH')
      ENDIF
C
C++   Get the run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        KRUNH=IW(NRUNH)
        IF (KRUNH.EQ.0) THEN
          IER=5
          GO TO 999
        ENDIF
        IRUN=IW(KRUNH+JRUNRN)
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Look for the necessary input banks
C
      KFRFT=IW(NFRFT)
      KTEXS=IW(NTEXS)
      IF (KFRFT.NE.0 .AND. KTEXS.NE.0) THEN
        FMDST=.FALSE.
      ELSE
        KDTRA=IW(NDTRA)
        IF (KDTRA.EQ.0) THEN
          IER=1
          GO TO 999
        ENDIF
        FMDST=.TRUE.
      ENDIF
C
C++   Get the TC3X calibration bank
C
      KTC3X=IW(NTC3X)
      IF (KTC3X.EQ.0) THEN
        IRET= ALGTDB(JUNIDB(0),'TC3X',IRUN)
        IF (IRET.EQ.0) THEN
          IER=4
          GO TO 999
        ENDIF
        KTC3X=IW(NTC3X)
      ELSE
        IF (IRUN.LT.ITABL(KTC3X,1,JTC3VR)
     &              .OR. IRUN.GT.ITABL(KTC3X,1,JTC3VR+1)) THEN
          IRET=ALGTDB(JUNIDB(0),'TC3X',IRUN)
          IF (IRET.EQ.0) THEN
            IER=4
            GO TO 999
          ENDIF
          KTC3X=IW(NTC3X)
        ENDIF
      ENDIF
C
C++   Get the track momentum and error on momentum
C
      IF (FMDST) THEN
        P=FLOAT(ITABL(KDTRA,ITK,JDTRP0))/EFACTM
        SGP=FLOAT(ITABL(KDTRA,ITK,JDTRER))/EFACTM
      ELSE
        RI=RTABL(KFRFT,ITK,JFRFIR)
        IF (RI.NE.0.) THEN
          RAD=1./RI
        ELSE
          RAD=1.0E20
        ENDIF
        PT=RAD*CLGHT*FIELD/100000.
        TANL=RTABL(KFRFT,ITK,JFRFTL)
        SECL=SQRT(1.0+TANL**2)
        SINL=TANL/SECL
        P=ABS(PT)*SECL
        DPDRI= -P*RAD
        DPDTL= PT*SINL
        SRI=RTABL(KFRFT,ITK,JFRFEM)
        STL=RTABL(KFRFT,ITK,JFRFEM+2)
        SCOR=RTABL(KFRFT,ITK,JFRFEM+1)
        SGP=SQRT(AMAX1(0.,(DPDRI**2)*SRI + (DPDTL**2)*STL
     &                        + 2.0*(DPDRI*DPDTL)*SCOR))
      ENDIF
C
C++   Get the particle's measured dE/dx and its relative error
C
      IF (FMDST) THEN
C       FMDST SHOULD BE ALWAYS FALSE.
        CALL DWRT('TMDSTX was removed from ALEPHLIB')
        CALL DWRT('If you come here, something is wrong. Please')
        CALL DWRT('report to HAYWOOD@CERNVM or H.Drevermann.')
C        CALL TMDSTX(ITK,RI,RSIG,NS,TL,IER)
CC!======== MODIFICATION ( I.E. ) =====================================
CC       NO SECTOR DE/DX INFORMATION IN MDST.
C        NSIODS = 0
C        CALL VZERO(NSECDS,NSEC)
C        CALL VZERO(DEDXDS,NSEC)
C        CALL VZERO(ERORDS,NSEC)
C        CALL VZERO(NSAMDS,NSEC)
C        CALL VZERO(TRLGDS,NSEC)
CC!====================================================================
C        IF (IER.NE.0) GO TO 999
        GO TO 999
      ELSE
        CALL DVTMDE(ITK,RI,RSIG,NS,TL,IER)
        IF (IER.NE.0) GO TO 999
C
C++     Apply the TC3X normalization correction here.  This is not done
C++     in TMDEDX, because TMDEDX is called to create the MINI-DST, and
C++     TC3X is applied in TMDSTX when reading the MINI.  We must avoid
C++     applying the TC3X correction twice.
C
C!======== MODIFICATION ( I.E. ) =====================================
        IF ( NSIODS.NE.0 ) THEN
          DO II =  1, NSIODS
            DEDXDS(II) = DEDXDS(II)*RTABL(KTC3X,1,JTC3NR)
          ENDDO
        ENDIF
C!====================================================================
        RI=RI*RTABL(KTC3X,1,JTC3NR)
      ENDIF
      IF (NS.GT.0 .AND. TL.GT.0.) THEN
        SMP=TL/FLOAT(NS)
        SMPL=ALOG(SMP/TWRSP)
      ELSE
        SMPL=0.
      ENDIF
C
C++   Loop over the N mass hypotheses
C
      DO 300 I=1,N
C
C++     Get beta*gamma of the particle and its error
C
        BG=P/RMASS(I)
        EBG=SGP/RMASS(I)
C
C++     Get the expected dE/dx and resolution, including momentum
C++     contribution.
C
        CALL TXDEDX(BG,EBG,Q(I),RSIG,SMPL,RIEXP(I),SIGMA(I),IER)
        IF (IER.NE.0) THEN
          GO TO 999
        ENDIF
  300 CONTINUE
      IER=0
C
  999 CONTINUE
      RETURN
      END
C*******************************************************************
      SUBROUTINE DVTMDE(ITK,RI,RSIG,NS,TL,IER)
C      SUBROUTINE TMDEDX(ITK,RI,RSIG,NS,TL,IER)
C
C-------------------------------------------------------------------
C! Return reduced and calibrated dE/dx for a single track.
C!
CKEY DEDX TPC MDST
C!
C!    Author:  R. Johnson    31-05-89
C!   Modified :- Ilias T. Efthymiopou  14-JUL-1991
C!              use a common block to get the ionization per sector
C!
C! Input:   ITK         /I       Track number in FRFT bank
C! Output:  RI          /R       Measured ionization (1.0=minion, Q=1)
C!          RSIG        /R       Relative error on the dE/dx
C!                               The error to be used in analysis
C!                               should be calculated from:
C!                               SIGMA**2= (RSIG*Iexp)**2 + SIG_P**2
C!                               where Iexp is the expected ionization
C!                               for a given hypothesis, and SIG_P
C!                               is the contribution from momentum
C!                               error.
C!          NS          /R       Number of useful wire samples on track
C!          TL          /T       Useful length of the track (cm)
C!          IER         /I       Error return= 0 for success
C!                               2= can't find dE/dx bank
C!                               3= track has no dE/dx information
C!                               4= cannot find calibration banks
C!                               5= cannot find RUNH or EVEH bank
C!                                  from which to get the run number
C!                               6= no valid dE/dx calibration exists
C!                                  for this run
C!
C----------------------------------------------------------------------
C*IF .NOT.DOC
C
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_TEXSJJ.INC'
      INCLUDE 'A_TC1XJJ.INC'
      INCLUDE 'A_TC2XJJ.INC'
      INCLUDE 'A_RUNHJJ.INC'
      INCLUDE 'A_EVEHJJ.INC'
C
      INTEGER ALGTDB
      LOGICAL FIRST,FOUND
      DATA FIRST/.TRUE./
C
C++   Distance between TPC sense wires
C
      DATA DWIR/0.400/
C
C!======== MODIFICATION ( I.E. ) =====================================
      INCLUDE 'DALI_CF.INC'
C!====================================================================
      INCLUDE 'A_BMACRO.INC'
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTEXS=NAMIND('TEXS')
        NTC1X=NAMIND('TC1X')
        NTC2X=NAMIND('TC2X')
        NRUNH=NAMIND('RUNH')
        NEVEH=NAMIND('EVEH')
      ENDIF
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        KRUNH=IW(NRUNH)
        IF (KRUNH.EQ.0) THEN
          IER=5
          GO TO 999
        ENDIF
        IRUN=IW(KRUNH+JRUNRN)
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Look for the calibration constants
C
      KTC1X=IW(NTC1X)
      KTC2X=IW(NTC2X)
      IF (KTC2X.EQ.0 .OR. KTC1X.EQ.0) THEN
        IRET= ALGTDB(JUNIDB(0),'TC1XTC2X',IRUN)
        IF (IRET.EQ.0) THEN
          IER=4
          GO TO 999
        ENDIF
        KTC1X=IW(NTC1X)
        KTC2X=IW(NTC2X)
      ELSE
        IF (IRUN.LT.ITABL(KTC1X,1,JTC1VR)
     &              .OR. IRUN.GT.ITABL(KTC1X,1,JTC1VR+1)) THEN
          IRET=ALGTDB(JUNIDB(0),'TC1X',IRUN)
          IF (IRET.EQ.0) THEN
            IER=4
            GO TO 999
          ENDIF
          KTC1X=IW(NTC1X)
        ENDIF
        IF (IRUN.LT.ITABL(KTC2X,1,JTC2VR)
     &              .OR. IRUN.GT.ITABL(KTC2X,1,JTC2VR+1)) THEN
          IRET=ALGTDB(JUNIDB(0),'TC2X',IRUN)
          IF (IRET.EQ.0) THEN
            IER=4
            GO TO 999
          ENDIF
          KTC2X=IW(NTC2X)
        ENDIF
      ENDIF
C
C++   Link to the dE/dx reconstructed information
C
      KTEXS=IW(NTEXS)
      IF (KTEXS.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
C++   Adsorption parameter
C
      ZCOR= RTABL(KTC2X,1,JTC2AP)
C
C++   Overall normalization.  If this is zero, then there is no
C++   valid dE/dx calibration for this run.
C
      RNRMA= RTABL(KTC2X,1,JTC2NR)
      IF (RNRMA.LE.0.) THEN
        IER=6
        RETURN
      ENDIF
C
C++   Get the particle's measured dE/dx, track length, and # samples
C
      NS=0
      TRMN=0.
      TL=0.
C!======== MODIFICATION ( I.E. ) =====================================
C       INITIALIZATION HERE
      NSIODS = 0
      CALL VZERO(NSECDS,NSEC)
      CALL VZERO(DEDXDS,NSEC)
      CALL VZERO(ERORDS,NSEC)
      CALL VZERO(NSAMDS,NSEC)
      CALL VZERO(TRLGDS,NSEC)
C!====================================================================
      FOUND=.FALSE.
      DO 100 ISG=1,LROWS(KTEXS)
        IPNT=ITABL(KTEXS,ISG,JTEXTN)
        IF (IPNT.NE.ITK) THEN
          IF (FOUND) GO TO 101
          GO TO 100
        ENDIF
        FOUND=.TRUE.
C
C++     Skip sectors with more than 40% of hits saturated
C
        IF (ITABL(KTEXS,ISG,JTEXSF).EQ.1) GO TO 100
C
C++     Skip sectors which could not be calibrated (RNRMS=0)
C
        ISLOT=ITABL(KTEXS,ISG,JTEXSI)
        RNRMS= RTABL(KTC2X,1,JTC2SN+ISLOT-1)
        IF (RNRMS.LE.0.) GO TO 100
C
C++     Correct the measured ionization for adsorption in the gas
C
        TMS=RTABL(KTEXS,ISG,JTEXTM)*(1.0+ZCOR*RTABL(KTEXS,ISG,JTEXAD))
C
C++     Correction for sample length
C
        NSMP=ITABL(KTEXS,ISG,JTEXNS)
        IF (NSMP.LE.1) GO TO 100
        RNSMP=FLOAT(NSMP)
        ASL=RTABL(KTEXS,ISG,JTEXTL)/RNSMP
        SMPL=ALOG(ASL/DWIR)
        TMS=TMS/(1.+RTABL(KTC2X,1,JTC2SL)*SMPL)
C
C++     Correct the sector-to-sector normalization
C
        TMS= TMS*RNRMS
C!======== MODIFICATION ( I.E. ) =====================================
        NSIODS = NSIODS+1
        NSECDS(NSIODS) = ISLOT
        TRMEAN = TMS*RNRMA
        DEDXDS(NSIODS) = TRMEAN
        NSAMPL = (NSMP*100)/ITABL(KTC1X,1,JTC1TP)
        NSAMDS(NSIODS) = NSAMPL
        TRKLEN = RTABL(KTEXS,ISG,JTEXTL)*100./FLOAT(ITABL(KTC1X,1,
     &    JTC1TP))
        TRLGDS(NSIODS) = TRKLEN
        ERORDS(NSIODS) = TDXERR(NSAMPL,TRKLEN,IERROR)*TRMEAN
C!====================================================================
C
C++     Add the contributions from different sectors, weighted by the
C++     number of wire pulses in the sector
C
        TRMN=TRMN + TMS * RNSMP
C
C++     Add up the total track length and number of samples
C
        NS=NS + NSMP
        TL=TL+ RTABL(KTEXS,ISG,JTEXTL)
  100 CONTINUE
  101 CONTINUE
      IF (NS.EQ.0) THEN
        IER=3
        GO TO 999
      ENDIF
      RI=TRMN/FLOAT(NS)
C
C++   Correct the truncated mean with overall normalization factor
C
      RI= RI*RNRMA
C
C++   We want NS to represent the number of samples BEFORE truncation,
C++   so we divide by JTRUNK.  The same applies to TL, the length.
C
      JTRUNK=ITABL(KTC1X,1,JTC1TP)
      NS=(NS*100)/JTRUNK
      TL=TL*100./FLOAT(JTRUNK)
C
C++   Get the resolution corresponding to this track length and number
C++   of samples.
C
      RSIG=TDXERR(NS,TL,IER)
C
      IER=0
  999 CONTINUE
      RETURN
      END
*DK DVLBKN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVLBKN
CH
      SUBROUTINE DVLBKN(TBNK,NRLEN,NUBA,NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU, adapted by H.D. 1996             23-JUN-1992
C    This is a copy of DZLBKN, where the Bank name is assumed to be right
C!: List all of bank numbers
C    Inputs    :
C         TBNK : The bank name
C        NRLEN : length of NUBA
C   Outputs    :
C         NUBA : Array contain all NR of the bank
C          NUM : The number of bank NR
C    Called by : DALB6
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      CHARACTER *(*) TBNK
      DIMENSION NUBA(*)
C     ...................................... NAMI is the name index of the bank
      NAMI=NAMIND(TBNK)
      IBNK=IW(NAMI)
      NUM=0
      IF (IBNK.EQ.0) RETURN
C     ........................................................ Get bank numbers
      IBNK=NAMI+1
  666 IBNK=IW(IBNK-1)
      IF(IBNK.GT.0)THEN
        NR=IW(IBNK-2)
        IF(NUM.LE.NRLEN) THEN
          NUM=NUM+1
          NUBA(NUM)=NR
          GO TO 666
        END IF
      END IF
      END
*DK DVKNK0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVKNV0
CH
      SUBROUTINE DVKNV0(NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:                Read kink vertices and nuclear vertices
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3),PXYZ(3)
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
      IER=0
      NUM=0
      CALL VZERO(KINVDT,2*MTRKDT)
      IYKNK = IW(NAMIND('YKNK'))
      IF (IYKNK.NE.0) THEN
        NUM=LROWS(IYKNK)
        DO N=1,NUM
          DO I=1,2
C           ....................... I=1: inner track: vertex = end point
C           ....................... I=2: outer track: vertex = start point
            ITR=ITABL(IYKNK,N,I)
            IF(ITR.GT.0.AND.ITR.LE.MTRKDT) THEN
              IF(KINVDT(I,ITR).NE.0) IER=1
              KINVDT(I,ITR)=N
            END IF
          END DO
        END DO
      END IF
      NKNK=NUM
      IYNLI = IW(NAMIND('YNLI'))
      IF (IYNLI.NE.0) THEN
        NUM2=LROWS(IYNLI)
        NUM=NUM+NUM2
        IYNVH = IW(NAMIND('YNVH'))
        IF (IYNVH.NE.0) THEN
          NTRNV=LROWS(IYNVH)
          DO N=1,NTRNV
            ITR=ITABL(IYNVH,N,1)
            IVX=ITABL(IYNVH,N,2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC NI START
            IF(IVX.GT.0) THEN
C             .......................................... OUTGOING TRACK
C             ....................... I=1: inner track: vertex = end point
C             ....................... I=2: outer track: vertex = start point
              I=2
            ELSE
C             .......................................... INCOMING TRACK
              I=1
              IVX=-IVX
            END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC NI END
            IF(ITR.GT.0.AND.ITR.LE.MTRKDT) THEN
              IF(KINVDT(I,ITR).NE.0) IER=2
C             KINVDT(2,ITR)=NKNK+IVX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC LINE BELOW
              KINVDT(I,ITR)=NKNK+IVX
            END IF
          END DO
        END IF  
      END IF
      NKNI=NUM
      IYLV0 = IW(NAMIND('YLV0'))
      IF (IYLV0.NE.0) THEN
        NUMV0=LROWS(IYLV0)
        NUM=NUM+NUMV0
        DO N=1,NUMV0
          DO I=1,2
C           ....................... I=1: positive track
C           ....................... I=2: negative track
            ITR=ITABL(IYLV0,N,I)
            IF(ITR.GT.0.AND.ITR.LE.MTRKDT) THEN
C             ........... NI tracks are not overwritten, because V0 tracks
C             ........... in VX list are received directly from the bank.
              IF(KINVDT(2,ITR).NE.0) THEN
                IER=3
              ELSE
                KINVDT(2,ITR)=NKNI+N
              END IF
            END IF
          END DO
        END DO
      ELSE
        NUMV0=0
      END IF
      NUMTOT=NUM
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVKNV1
CH
      ENTRY DVKNV1(NUM,NER)
CH
CH --------------------------------------------------------------------
CH
      NUM=NUMTOT
      NER=IER
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVKNV
CH
      ENTRY DVKNV(K,XYZ)
CH
CH -------------------------------------------------------------------- 
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(     K.LE.NKNK) THEN
        XYZ(1)=RTABL(IYKNK,K,3)
        XYZ(2)=RTABL(IYKNK,K,4)
        XYZ(3)=RTABL(IYKNK,K,5)
      ELSE IF(K.LE.NKNI) THEN
        KK=K-NKNK
        XYZ(1)=RTABL(IYNLI,KK,1)
        XYZ(2)=RTABL(IYNLI,KK,2)
        XYZ(3)=RTABL(IYNLI,KK,3)
      ELSE
        KK=K-NKNI
        XYZ(1)=RTABL(IYLV0,KK,3)
        XYZ(2)=RTABL(IYLV0,KK,4)
        XYZ(3)=RTABL(IYLV0,KK,5)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVKNV
CH
      ENTRY DVKNV_TR(K,NTYPE,IFR,ITO)
CH
CH --------------------------------------------------------------------
CH
      IF(K.LE.NKNK) THEN
        NTYPE=1
        IFR=ITABL(IYKNK,K,1)
        ITO=ITABL(IYKNK,K,2)
      ELSE IF(K.LE.NKNI) THEN
        NTYPE=2
        IFR=-1
        ITO=-1
      ELSE
        NTYPE=3
        KK=K-NKNI
        IFR=ITABL(IYLV0,KK,1)
        ITO=ITABL(IYLV0,KK,2)
      END IF
      IF(IFR.LT.1.OR.IFR.GT.99) IFR=99
      IF(ITO.LT.1.OR.ITO.GT.99) ITO=99
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------  DVKN_V0_TR_0
CH
      ENTRY DVKN_V0_TR_0(NUM)
CH
CH --------------------------------------------------------------------
CH
      NUM=NUMV0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------  DVKN_V0_TR_0
CH
      ENTRY DVKN_V0_TR(K,XYZ,PXYZ)
CH
CH --------------------------------------------------------------------
CH
      XYZ(1) =RTABL(IYLV0,K,3)
      XYZ(2) =RTABL(IYLV0,K,4)
      XYZ(3) =RTABL(IYLV0,K,5)
      PXYZ(1)=RTABL(IYLV0,K,12)
      PXYZ(2)=RTABL(IYLV0,K,13)
      PXYZ(3)=RTABL(IYLV0,K,14)
      END
*DK DV_LC_CRACK
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DV_LC_CRACK
CH
      SUBROUTINE DV_LC_CRACK(PM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH

C     Z>0 Y>0  PM(4) +A
C     Z>0 Y<0  PM(3) -A
C     Z<0 Y<0  PM(2) -B
C     Z<0 Y>0  PM(1) +B

      INCLUDE 'DALI_CF.INC'
      DIMENSION PM(0:4)
      DATA PCUT/100./
      DATA SPM/1./,LDEB/0/
      CHARACTER *2 TEST(4)
      DIMENSION   PTEST(4)

      DATA  TEST/'+B','-B','-A','+A'/
      DATA PTEST/120.,220.,210.,110./

      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'

      CALL VZERO(PM,5)
      KLCRA = IW(NAMIND('LCRA'))
      IF(KLCRA.GT.0) THEN
        DO I=1,4
          CR1 = FLOAT(ITABL(KLCRA,I,1))
          CR2 = FLOAT(ITABL(KLCRA,I,2))
          CRS = MAX(CR1,CR2)

          PM(I)=CRS*SPM
          IF(CRS.GE.0.) PM(0)=1.
C         ................................. draw_signal_at_(x,y,z)=CRXYZ(*,I)
        END DO
      ELSE IF(LDEB.GT.0) THEN
        IF(LDEB.GT.4) THEN
          CALL UFILL(PM,2,5,123456.)
          CALL DWRT('All')
          LDEB=0
        ELSE
          PM(LDEB)=PTEST(LDEB)
          WRITE(TXTADW,1000) (TEST(I),PM(I),I=1,4)
 1000     FORMAT(4(2X,A,F4.0))
          CALL DWRC
        END IF
        PM(0)=2.
        LDEB=LDEB+1
      END IF
      
      END

*DK DV_SET_LABEL
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DV_SET_LABEL
CH
      SUBROUTINE DV_SET_LABEL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     used in DPI_DO for ATLAS      
      END
*DK DV_PICK_TEXT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DV_PICK_TEXT
CH
      SUBROUTINE DV_PICK_TEXT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     used in DPI_DO for ATLAS      
      END


*DK DV_STORE_IW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DV_STORE_IW
CH
      SUBROUTINE DV_STORE_IW
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'A_BCS.INC'
      INCLUDE 'DALI_CF.INC'
      CHARACTER *9 TIW
      DATA TIW/'IWBIN.DAT'/

      IND1=IW(14)
      OPEN(UNIT=NUNIDU,STATUS='UNKNOWN',FORM='UNFORMATTED',FILE=TIW)
      WRITE(NUNIDU) IND2,(IW(K),K=IND1,IND2)
      CLOSE(UNIT=NUNIDU)
      TXTADW=TIW//'stored'
      CALL DWRC
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------- DV_LENGTH_IW
CH
      ENTRY DV_LENGTH_IW
CH
CH --------------------------------------------------------------------
CH
      IND2=NBANK('DDDD',1,1)-1
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------- DV_READ_IW
CH
      ENTRY DV_READ_IW
CH
CH --------------------------------------------------------------------
CH
      IND1=IW(14)
      OPEN(UNIT=NUNIDU,STATUS='UNKNOWN',FORM='UNFORMATTED',FILE=TIW)
      READ(NUNIDU) IND2,(IW(K),K=IND1,IND2)
      CLOSE(UNIT=NUNIDU)
      TXTADW=TIW//'read'
      CALL DWRC
      END
