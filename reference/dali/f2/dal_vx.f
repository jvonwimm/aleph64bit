CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DV0TYP
CH
      SUBROUTINE DV0TYP(NHIT,MODUL,TR)
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
        RETURN
      END IF
      IF(MODUL.EQ.2001.OR.MODUL.EQ.2002) THEN
        CALL DHZPHP(NHIT)
        RETURN
      END IF
      IF(MODUL.EQ.2003.OR.MODUL.EQ.2004) THEN
        CALL DRZHEP(NHIT)
        RETURN
      END IF
      IF(MODUL.EQ.2005.OR.MODUL.EQ.2006) THEN
        CALL DRZHHP(NHIT)
        RETURN
       END IF
      IF(MODUL.EQ.2010) THEN
        CALL DYXHEP(NHIT)
        RETURN
      END IF
      IF(MODUL.GE.2011.AND.MODUL.LE.2023) THEN
        CALL DHZPEP(NHIT)
        RETURN
      END IF
      IF(MODUL.GT.1000.AND.MODUL.LT.2000) THEN
        IF(NHIT.GT.31) GO TO 9
        CALL DRSPTY
        RETURN
      END IF
      IF(MODUL.EQ.2500) THEN
        CALL DTDPIK(MODUL,NHIT)
        TR=NHIT
        RETURN
      END IF
      IF(MODUL.EQ.7000.OR.MODUL.EQ.7001) THEN
        CALL DAC_PICK(NHIT,MODUL)
        RETURN
      END IF
      IF(MODUL.EQ.PEWDDB) THEN
        CALL DYXWE_PICK(NHIT)
        RETURN
      END IF
C      IF(MODUL.GE.6001.AND.MODUL.LE.6009) THEN
C        CALL DDL_ALPHA_PI(MODUL,NHIT)
C        RETURN
C      END IF
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
         RETURN
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
         RETURN
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
        RETURN
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
         RETURN
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
         RETURN
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
      RETURN
    9 CALL DWRT('Bank not available or row out of range.')
      END
*DK DVCO
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DVCO
CH
      FUNCTION DVCO(NV,NHIT,NBANC)
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
      IF(NBANC.GT.NUMBDB) GO TO 9
      IF(NHIT.LE.0.OR.NHIT.GT.IFIX(BNUMDB(2,NBANC))) GO TO 9
      IF     (NBANC.EQ.ITCODB) THEN
         CALL=DVIT0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=0.5*(DVIT(NV,NHIT,1)+DVIT(NV,NHIT,2))
      ELSE IF(NBANC.EQ.TPCODB) THEN
         CALL=DVTQ0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVTP(NV,NHIT)
      ELSE IF(NBANC.EQ.ESDADB) THEN
         CALL=DVEC0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVEC(NV,NHIT)
      ELSE IF(NBANC.EQ.TPADDB) THEN
         CALL DPARGV(20,'HSE',2,HSE2)
         NSECT=HSE2
         CALL=DVPAD0(NSECT,NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVPADA(NV,NHIT)
      ELSE IF(NBANC.EQ.TBCODB) THEN
         CALL=DVTB0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVTP(NV,NHIT)
      ELSE IF(NBANC.EQ.HSDADB) THEN
         CALL=DVHC0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVHC(NV,NHIT)
      ELSE IF(NBANC.EQ.PLSDDB) THEN
         CALL=DVLC0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVLC(NV,NHIT)
      ELSE IF(NBANC.EQ.HTUBDB) THEN
         CALL=DVHT0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVHT(NV,NHIT)
      ELSE IF(NBANC.EQ.MHITDB) THEN
         CALL=DVMD0(NN)
         IF(NHIT.GT.NN) GO TO 9
         DVCO=DVMD(NV,NHIT)
      ELSE
         DVCO=-999999.
      END IF
      RETURN
    9 DVCO=-999999.
      END
*DK DVMCT0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVMCT0
CH
      SUBROUTINE DVMCT0(NTRK)
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
*CD BCS
      INCLUDE 'A_BCS.INC'
C
      NTRK = 0
      IF(IW(30).NE.12345) RETURN
      NAMI = NAMIND('KINE')
      IND = NAMI + 1
   10 IND = IW(IND-1)
      IF (IND.NE.0.AND.IW(IND+9).EQ.1) THEN
         NTRK = NTRK + 1
         GOTO 10
      ENDIF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVMCTP
CH
      ENTRY DVMCTP(N,PX,PY,PZ,ID)
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
      JKINE = NLINK('KINE',N)
C
   20 IF (IW(JKINE+9).NE.1) THEN
         JKINE = IW(JKINE-1)
         GOTO 20
      ENDIF
      PX = RW(JKINE+4)
      PY = RW(JKINE+5)
      PZ = RW(JKINE+6)
      ID = IW(JKINE+8)
      RETURN
      END
CH
CH
CH
CH

C      LOGICAL FUNCTION DVMCNS(NAPAR,KINE)
CC!      select MC particles which are in TPC or enter ECAL
CC       SLIGHTLY MODIFIED FROM D.SCHLATTERS SUBROUTINE SELPAR
CC      INPUT:
CC              KINE  = index of KINE bank
CC
C      DATA RMAX/177./,ZMAX/220./
C      INCLUDE 'BCS.INC'
C      INCLUDE 'BMACRO.INC'
C      INCLUDE 'KMACRO.INC'
C      RHO(IND)=SQRT(RW(IND+4)**2+RW(IND+5)**2)
C
C      DVMCNS=.TRUE.
CC      NAPAR=NAMIND('PART')
C      IPART=IW(KINE+8)
C
CC           LIFETIME
C      TIM=TIMLIF(IPART)
C      IF(TIM.LT.1.E-9.AND.TIM.GT.0.) RETURN
C      IF(IPART.GT.44.and.IPART.LT.49) RETURN
C      IF(IW(KINE+9).EQ.1.and.IW(KINE+3).EQ.1) GOTO 99
CC           PHOTONS
C      IF(IPART.EQ.1) THEN
C         KVER1=NLINK('VERT',IW(KINE+9))
CC                   REJECT PHOTONS FROM SHOWER
C         IF(RHO(KVER1).GT.RMAX) RETURN
C         IF(RHO(KVER1).LT.RMAX.AND.ABS(RW(KVER1+6)).GT.ZMAX) RETURN
C         IF(IW(KINE+3).EQ.1) GOTO 99
C         KVER2=NLINK('VERT',IW(KINE+10))
CC                 REJECT CONVERSIONS
C         IF(RHO(KVER2).GT.RMAX/2..OR.ABS(RW(KVER2+6)).GT.ZMAX) GOTO 99
C         RETURN
C      ENDIF
CC          HADRONS
CC            REJECT NUCLEAR PROCESSES but keep lambdas
C      IF(IPART.LE.15.AND.IPART.GE.13) THEN
C         IF(IW(KINE+9).NE.1) THEN
C            KVER1=NLINK('VERT',IW(KINE+9))
C            IN=IW(KVER1+8)
C            KINE1=NLINK('KINE',IN )
C            IF(IW(KINE1+8).EQ.18) GOTO 25
C            IF(RHO(KVER1).LT.RMAX) RETURN
C         ENDIF
C      ENDIF
CC
C   25 CONTINUE
C      IF(IPART.EQ.25.OR.(IPART.GE.7.AND.IPART.LE.16)) THEN
C         KVER1=NLINK('VERT',IW(KINE+9))
CC                REJECT HADRONS FROM SHOWER
C         IF(RHO(KVER1).GT.RMAX) RETURN
C         IF(RHO(KVER1).LT.RMAX.AND.ABS(RW(KVER1+6)).GT.ZMAX) RETURN
CC
C         IF(IW(KINE+3).EQ.1) GOTO 99
CC
C         KVER2=NLINK('VERT',IW(KINE+10))
CC            KEEP PART. WITH NUCL. INTERACTION AT CENTER
C         KINE2=NLINK('KINE',IW(KVER2+9) )
C         IF(IW(KINE2+8).GE.13.and.IW(KINE2+8).LE.15) GOTO 99
C         IF(RHO(KVER2).LT.RMAX.AND.ABS(RW(KVER2+6)).LT.ZMAX) RETURN
C         GOTO 99
C      ENDIF
CC          MUONS
C      IF(IPART.LE.6.AND.IPART.GE.5) THEN
C         KVER1=NLINK('VERT',IW(KINE+9))
C         IF(RHO(KVER1).GT.RMAX) RETURN
C         IF(RHO(KVER1).LT.RMAX.AND.ABS(RW(KVER1+6)).GT.ZMAX) RETURN
C         GOTO 99
C      ENDIF
CC          ELECTRON
C      IF(IPART.LE.3.AND.IPART.GE.2) THEN
C         KVER1=NLINK('VERT',IW(KINE+9))
C         IF(RHO(KVER1).GT.RMAX/2.) RETURN
C         IF(RHO(KVER1).LT.RMAX/2..AND.ABS(RW(KVER1+6)).GT.ZMAX) RETURN
C         GOTO 99
C      ENDIF
CC         NEUTRINO
C      IF(IPART.EQ.4.OR.(IPART.GE.54.AND.IPART.LE.59)) THEN
C         KVER1=NLINK('VERT',IW(KINE+9))
C         IF(RHO(KVER1).GT.RMAX) RETURN
C         IF(RHO(KVER1).LT.RMAX.AND.ABS(RW(KVER1+6)).GT.ZMAX) RETURN
C         GOTO 99
C      ENDIF
C   99 CONTINUE
C      DVMCNS=.FALSE.
C      RETURN
C      END
*DK DVPART
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVPART
CH
      SUBROUTINE DVPART(ITYPE,N,TNAM)
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
      CHARACTER *(*) TNAM
      CHARACTER *6 T(47)
      DATA T/
     &  'Gamma ','Posit.','Electr','NY    ','Muon  ',
     &  'Muon  ','Pion  ','Pion  ','Pion  ','Kl    ',
     &  'K     ','K     ','Neutr.','Proton','AProt.',
     &  'Ks    ','Eta   ','Lambda','Sigma ','Sigma ',
     &  'Sigma ','Xi    ','Xi    ','Omega ','ANeut.',
     &  'ALamb.','ASigma','ASigma','ASigma','AXi   ',
     &  'AXi   ','AOmega','Tau   ','Tau   ','D     ',
     &  'D     ','D     ','AD    ','sD    ','sD    ',
     &  'cLamb.','W     ','W     ','Z0    ','Deute.',
     &  'Trito.','Alpha '/
      TNAM(1:N)=T(ITYPE)(1:N)
      END
*DK DVX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVX
CH
      SUBROUTINE DVX
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
      DIMENSION LPTRK(9)
      EQUIVALENCE (LPTRK,NTRKDP)
      DIMENSION PPI0(3),ERRP0(4,4)
      PARAMETER (MSYM=5,MCOL=15,MON=1,MSEL=60,MCTR=60,MR=3,MP1=3,MP2=9)
      PARAMETER (               MSEL3=MSEL*3 )
      DIMENSION ICOCN(MCTR)
      DIMENSION ISEL(MSEL,3)
      DATA      ISEL/MSEL3*0/
      DIMENSION ICOL(MSEL,2),ISYM(MSEL,2),IONF(MSEL,2),ITOV(MSEL)
      DATA  ICOL/MSEL*8,MSEL*9/,
     &      ISYM/MSEL*5,MSEL*3/,
     &      IONF/MSEL*0,MSEL*1/,IS5/5/
      DIMENSION IFRV(MSEL,5),ITRK(MSEL)  ,ITTY(MSEL)  ,IWRK(MSEL)
      DATA                   ITRK/MSEL*0/,ITTY/MSEL*0/
      DIMENSION XYZ(3),XYZR(3),XYZV(3),RFT(3),PR1(4,MP1),PR2(4,MP2,2)
      DIMENSION FIR(MR),TER(MR),ALR(MR)
      DIMENSION XYZP(3,2),PXYZ(3)
      DATA PR1/.1,1.,99.,0.,   1.,6.,99.,0. ,1.,1.,9.,0./
      DATA PR2/
     &        1.,0.,MSEL,0.,
     &      -24, 0.,MSEL,0.,
     &        1.,0.,MSEL,0.,
     &        1.,0.,MSEL,0.,
     &        1.,0.,MSEL,0.,
     &        1.,0.,MSEL,0.,
     &        1.,0.,MSEL,0.,
     &        1.,0.,MSEL,0.,
     &        1.,0.,MSEL,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0.,
     &        -99.,0.,99.,0./
      DATA R2MAX/9./
      DATA IDEB/0/
      CHARACTER *2 TSYM(MSYM),TCO1(-2:MCOL),TCO2(MCOL),TCOL
      CHARACTER *2 TON(0:MON),TFL(0:1),TF,TKNV
      CHARACTER *2 TP1(MP1),TP2(2*MP2),TANSW,DT2
      CHARACTER *3 TSELT(0:1),TBL
      CHARACTER *5 TTYPE(0:9)
      DATA TSYM/'EL','XX','SQ','CF','AR'/,TBL/'   '/
      DATA TON/'OF','ON'/,TFL/'x ','=>'/
      DATA TP1/'SI','SZ','PV'/
      DATA TP2/'AD','SE','s2','s3','s4','s5','s6','s7','s8',
     &         'TR','t2','t3','t4','t5','t6','t7','t8','t9'/
      DATA TCO1/'MX','IV',
     &  'DF','C1','C2','C3','C4','C5','C6','GY',
     &  'WH','GN','YE','BR','RD','MA','CY','BL'/
      DATA TCO2/
     &       'c1','c2','c3','c4','c5','c6','gy',
     &  'wh','gn','ye','br','rd','ma','cy','bl'/
C                 12345678
      DATA TTYPE/'B Pos',
     &           'pr VX',
     &           ' V0  ',
     &           'Bhaba',
     &           'Gamma',
     &           '  ?? ',
     &           'new P',
     &           'sec V',
     &           'int P',
     &           '     '/
      DATA TSELT/'   ','not'/
      DIMENSION IQS(2:MSYM)
      DATA IQS/3,6,7,8/,IPC/1/,ILIST/1/,IDLST/1/,NTRK/0/
      LOGICAL FOUT,FYES,F1,FOTH,FSTRT
      DATA FOTH/.FALSE./,FSTRT/.FALSE./
      CHARACTER *49 T,T1(4),T0,T2,T3,TC,TV(2),TK(2)
C     DATA T1/'P?:W?:  SI_12  SZ_12   PV_12'/
C              123456789 123456789 123456789 123456789 123456789
C
      DATA TC/'P?:W?: define colors for individual tracks.'/
      DATA T1/'P?:W?: Vertex list           PV_12 SZ_12 SI_12345',
     &        'P?:W?: Vertex + track list   PV_12 SZ_12 SI_12345',
     &        'P?:W?: track list          PN_123',
     &        'P?:W?: Kink + Nuclear interaction list  SZ_12'/
      DATA T2/'1D: sigma=1=68% 1.515=87% 2=95% 3=99.7% Conf.Lev.'/
      DATA T3/'2 Dimens. 1=39% 1.515=68% 2=86% 3=98.8% use here!'/
C
C              123456789 123456789 123456789 123456789 123456789
C              => 1  1 pr VX    -0.009    0.006    0.282 WH AR OF |
      DATA TV/'   # Ty Vertex     X        Y        Z    CO SY',
     &        '   # Ty Vertex     R       phi     theta  CO SY'/
C
C              => 1 Ki 18->23 -985.301 -910.738 -914.764 GN SQ ON|:
      DATA TK/'   # Ty tracks     X        Y        Z    CO SY',
     &        '   # Ty tracks     R       phi     theta  CO SY'/
C
      DATA T0/'               Color of all Other tracks  = IV'/
C             '=> 1 T ##    P   dP  phi theta  D0   Z0  VX RD   '
C
      DATA R999/0./
C     .............. LATER FILL BUFFERS IF TRACK ASSIGNMENT IS KNOWN. PUT ALL
C     ............... SECONDARY TRACKS AUTOMATICALLY INT THE LIST
      IF(TPICDO.EQ.'TC') THEN
        ILIST=2
        IDLST=3
      END IF
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HIN,J_HCA'
      CALL DPARAM(20
     &  ,J_HIN,J_HCA)
      TPARDA=
     &  'J_RVN'
      CALL DPARAM(43
     &  ,J_RVN)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FMONDT.AND.FSTRT) THEN
        FSTRT=.FALSE.
        DO K=0,6
          TCO1(K)='IV'
          TCO2(K)='iv'
        END DO
        DO K=7,15
          TCO1(K)='VS'
          TCO2(K)='vs'
        END DO
      END IF
      FDTRDC=.FALSE.
      CALL DSCTR
      IF(TPICDO.EQ.'VX') THEN
        CALL DVKNV1(NKN,IER)
        CALL DVVTX0(NVX)
        IF(NVX.EQ.0) CALL DWRT('PYER is empty !')
C       ........................................... Get selected vertex from RO
        IF(PARADA(4,J_RVN).EQ.1.) THEN
          CALL VZERO(ISEL,MSEL)
          ISEL(IFIX(PARADA(2,J_RVN)),1)=1
        END IF
      END IF
C     ............................................................. DRAW HEADER
  930 CALL DO_BAR_STATUS_0

      IF(TPICDO.EQ.'TC') THEN
        CALL DTYPT('TYPE',TPICDO,1,0,0,' ',TC)
      ELSE
        CALL DTYPT('TYPE',TPICDO,1,MP1,PR1,TP1,T1(IDLST))
      END IF
      IF(IDLST.EQ.4) THEN
        CALL DWRT(TK(IPC))
        DO N=1,NKN
          CALL DVKNV(N,XYZ)
          TF=TFL(ISEL(N,3))
          CALL DVKNV_TR(N,LTYP,IFRKN,ITOKN)
          IF(      LTYP.EQ.1) THEN
            TKNV='Ki'
          ELSE IF(LTYP.EQ.2) THEN
            TKNV='NI'
            L=9
            NFRF=MIN(MTRKDT,IFIX(BNUMDB(2,FRFTDB)))
            DO KNVT=1,NFRF
              IF(KINVDT(2,KNVT).EQ.N) THEN
                ITOKN=KNVT
                T='         ->'
                DO K=KNVT+1,NFRF
                  IF(KINVDT(2,K).EQ.N) THEN
                    L=L+4
                    IF(L.LT.49) WRITE(T(L:L+2),1931) K
 1931               FORMAT(I3)
                  END IF
                END DO
                GO TO 931
              END IF
            END DO
          ELSE
            T='         ->'
            TKNV='V0'
            L=12
            WRITE(T(L:L+2),1931) ITOKN
            ITOKN=IFRKN
          END IF
  931     IF(IPC.EQ.2) THEN
            CALL DPOLCRV(XYZ,RFT)
            WRITE(TXTADW,1002) TF,N,TKNV,IFRKN,ITOKN,RFT,
     &        TCO1(ICOL(N,2)),TSYM(ISYM(N,2)),TON(IONF(N,2))
          ELSE
            WRITE(TXTADW,1002) TF,N,TKNV,IFRKN,ITOKN,XYZ,
     &        TCO1(ICOL(N,2)),TSYM(ISYM(N,2)),TON(IONF(N,2))
          END IF
 1002     FORMAT(A,I2,1X,A,I3,'>',I3,3F9.3,3(1X,A))
          IF(LTYP.EQ.1) THEN
            CALL DWRC
          ELSE
            TXTADW(9:10)='  '
            CALL DWRC
            IF(L.GT.10) CALL DWRT(T)
          END IF
        END DO
        IF(IER.NE.0) CALL DWRT('Tracks are entered twice.#')
        GO TO 936
      END IF
      IF(IDLST.LE.2) THEN
        CALL DWRT(T2)
        CALL DWRT(T3)
      END IF
      IF(TPICDO.EQ.'VX'.AND.NVX.GT.9) THEN
        CALL DWRT('Not more than 9 vertices are accepted')
        NVX=9
      END IF
C     ........................................................ DRAW VERTEX LIST
      IF(IDLST.LE.2) THEN
C                     => 1 12 prima. V -123.56 -123.56 -123.56 12 EL ON
C                     123456789 123456789 123456789 123456789 123456789
        CALL DWRT(TV(IPC))
        DO N=1,NVX
          CALL DVVTX(N,NTYPE,XYZ)
          IF(ISYM(N,1).EQ.1) THEN
            CALL DVVTXD(N,NDGFR)
            IF(NDGFR.EQ.0) ISYM(N,1)=IS5
          END IF
          TF=TFL(ISEL(N,1))
          IF(ILIST.EQ.2) TF(1:1)=' '
          IF(IPC.EQ.2) THEN
            CALL DPOLCRV(XYZ,RFT)
            WRITE(TXTADW,1000) TF,N,NTYPE,TTYPE(NTYPE),RFT,
     &        TCO1(ICOL(N,1)),TSYM(ISYM(N,1)),TON(IONF(N,1))
          ELSE
            WRITE(TXTADW,1000) TF,N,NTYPE,TTYPE(NTYPE),XYZ,
     &        TCO1(ICOL(N,1)),TSYM(ISYM(N,1)),TON(IONF(N,1))
          END IF
 1000     FORMAT(A,I2,I3,1X,A,1X,3F9.3,3(1X,A))
          CALL DWRC
        END DO
      END IF
C     ............................................................ LIST TRACKS
      IF(IDLST.EQ.2) CALL DWRT(
     &  '-------------------------------------------------')
      IF(IDLST.GE.2) THEN
C          123456789 123456789 123456789 123456789 123456789
C       T='=> 1 T ##    P   dP  phi theta  D0   Z0 frV toVRD'
        IF(NTRK.GT.0) THEN
          T='Tracks'
          CALL DVTCT1(0,T(6:49))
          IF(TPICDO.EQ.'TC') T(41:49)=' '
          CALL DWRT(T)
          DO N=1,NTRK
            KT=ITRK(N)
            IF(ITTY(N).EQ.1) THEN
              CALL DVTRT1(KT,T(6:49))
              IF(T(6:6).EQ.' ') T(6:6)='F'
              IF(COLRDT(KT).EQ.0.) THEN
                TCOL=TCO2(NCTRDC(KT))
              ELSE
                NCOL=COLRDT(KT)
                TCOL=TCO1(NCOL)
              END IF
            ELSE IF(ITTY(N).EQ.2) THEN
              CALL DVTCT1(KT,T(6:49))
              T(6:6)='C'
              TCOL=TCO1(ICOCN(N))
            ELSE IF(ITTY(N).EQ.3) THEN
              CALL DVTNT1(KT,T(6:49))
              T(6:6)='N'
              TCOL=TCO1(ICOCN(N))
            ELSE
              T(6:6)='P'
              TCOL=' '
              CALL DPOLCR(PPI0(1),PPI0(2),PPI0(3),PP0,TP0,FP0)
              ITP0=TP0
              IFP0=FP0
              WRITE(T,1008) ITRK(N),PP0,IFP0,ITP0
 1008         FORMAT(5X,'P',I3,F6.1,I8,I5)
            END IF
            WRITE(T( 1: 5),1000) TFL(ISEL(N,2)),N
            IF(ILIST.EQ.1) T(1:1)=' '
            WRITE(T(40:49),1001) IFRV(N,1),ITOV(N),TCOL
 1001       FORMAT(I3,I4,1X,A)
            IF(T(40:42).EQ.'  0') T(40:42)=TBL
            IF(T(44:46).EQ.'  0') T(44:46)=TBL
            IF(TPICDO.EQ.'TC') T(41:47)=' '
            CALL DWRT(T)
          END DO
        END IF
C       .................................................... DRAW OTHER TRACKS
        NALL=BNUMDB(2,FRFTDB)
        F1=.FALSE.
        DO K=1,NALL
          IF(NTRK.GT.0) THEN
            DO N=1,NTRK
              IF(K.EQ.ITRK(N)) GO TO 934
            END DO
          END IF
          NCOL=COLRDT(K)
          IF(F1) THEN
            IF(NC.NE.NCOL) THEN
              NC=-2
              GO TO 935
            END IF
          ELSE
            NC=NCOL
            F1=.TRUE.
          END IF
  934   END DO
  935   IF(F1) THEN
          T0(45:46)=TCO1(NC)
          CALL DWRT(T0)
        END IF
      END IF
C     ................................................................ OPERATOR
  936 DO I=1,2
        DO K=1,MP2
          PR2(2,K,I)=R999
        END DO
      END DO
      PR1(3,3)=NVX
      CALL DOPERS(2,2,0.)
      CALL DOPER(1,0,
     &  1,MP1  ,TP1,PR1,
     &  1,MP2*2,TP2,PR2,
     &  NEXEC,CHG,TANSW)
C     .............................................. SET COLOR FOR OTHER TRACKS
      IF(FOTH.AND.NEXEC.EQ.2) THEN
        FOTH=.FALSE.
        DO M=-1,MCOL
          IF(TANSW.EQ.TCO1(M)) THEN
            DO 838 K=1,NALL
              DO N=1,NTRK
                IF(ITRK(N).EQ.K.AND.ITTY(N).EQ.1) GO TO 838
              END DO
              COLRDT(K)=M
  838       CONTINUE
            CALL DSC0
          END IF
        END DO
        GO TO 930
      END IF
C     .................................................. STORE SELECTED TRACKS
      DO N=1,9
        IF(PR2(2,N,2).EQ.R999) GO TO 937
        LTRK=PR2(2,N,2)
        IF(NTRK.GT.0) THEN
          JTRK=ABS(LTRK)
          DO K=1,NTRK
            IF(ITTY(K).EQ.1.AND.JTRK.EQ.ITRK(K)) THEN
              IF(LTRK.GT.0) THEN
                CALL DWRT('List contains already track ',
     &            DT2(PR2(2,N,2)))
              ELSE IF(LTRK.LT.0) THEN
                DO L=K,NTRK
                  ITRK(L  )=ITRK(L+1)
                  ISEL(L,2)=ISEL(L+1,2)
                  DO I=1,5
                    IFRV(L,I)=IFRV(L+1,I)
                  END DO
                  ITOV(L  )=ITOV(L+1)
                  ITTY(L  )=ITTY(L+1)
                END DO
                NTRK=NTRK-1
              END IF
              GO TO 938
            END IF
          END DO
        END IF
        IF(LTRK.GT.0) THEN
          IF(NTRK.GE.MSEL) THEN
            CALL DWRT('Too many tracks, delet first.')
          ELSE
            NTRK=NTRK+1
            ITRK(NTRK  )=LTRK
            ISEL(NTRK,2)=1
            DO I=1,5
              IFRV(NTRK,I)=0
            END DO
            ITOV(NTRK  )=0
            ITTY(NTRK  )=1
          END IF
        END IF
  938 END DO
C     ........................................................ STORE SELECTION
  937 IF(PR2(2,1,1).NE.R999) THEN
        NUSEL=1
        GO TO 939
      END IF
      IF(PR2(2,2,1).NE.R999) THEN
        IF(PR2(2,2,1).GE.0.) THEN
          CALL VZERO(ISEL(1,ILIST),MSEL)
          NUSEL=1
        ELSE
          NUSEL=0
        END IF
        GO TO 939
      END IF
      GO TO 839
        
  939 DO K=1,MP2
        RNU=ABS(PR2(2,K,1))
        IF(RNU.NE.R999) THEN
          NU=RNU
          IF(NU.NE.0) THEN
            ISEL(NU,ILIST)=NUSEL
          ELSE
            CALL VZERO(ISEL(1,ILIST),MSEL)
          END IF
        END IF
      END DO
C     ................................................................... PICK
  839 IF(TANSW.EQ.'PI'.AND.NEXEC.EQ.2) THEN
        CALL DPOS(TANSW,FDUM,TDUM,NEXEC,CHG)
        IF(NEXEC.EQ.3) GO TO 930
      END IF
      GO TO (910,920,930,940), NEXEC
C     ........................................... Store selected vertex for RO
  910 IF(TPICDO.EQ.'TC') RETURN
      DO L=NVX,1,-1
        IF(ISEL(L,1).EQ.1) THEN
          PARADA(2,J_RVN)=L
          GO TO 911
        END IF
      END DO
  911 PARADA(4,J_RVN)=-1.
      RETURN
  920 IF(TPICDO.EQ.'VX') THEN
C       .......................................................... LIST VERTICES
        IF(TANSW.EQ.'LV') THEN
          ILIST=1
          IDLST=1
          GO TO 930
        END IF
C       .......................................................... LIST TRACKS
        IF(TANSW.EQ.'LT') THEN
          ILIST=2
          IDLST=3
          GO TO 930
        END IF
C       ............................................................ LIST BOTH
        IF(TANSW.EQ.'LB') THEN
          IDLST=2
          IF(ILIST.EQ.0) ILIST=1
          GO TO 930
        END IF
C       ........................................................ LIST KINKS
        IF(TANSW.EQ.'LK') THEN
          ILIST=3
          IDLST=4
          GO TO 930
        END IF
C       ................................................... store kink tracks
        IF(TANSW.EQ.'SK') THEN
          ILIST=2
          IDLST=3
          NFRF=MIN(MTRKDT,IFIX(BNUMDB(2,FRFTDB)))
          DO I=1,2
            DO J=1,NFRF
              IF(KINVDT(I,J).NE.0) THEN
                DO K=1,NTRK
                  IF(ITTY(K).EQ.1.AND.J.EQ.ITRK(K)) GO TO 932
                END DO
                IF(NTRK.GE.MSEL) THEN
                  CALL DWRT('Too many tracks, delet first.')
                ELSE
                  NTRK=NTRK+1
                  ITRK(NTRK  )=J
                  ISEL(NTRK,2)=1
                  DO L=1,5
                    IFRV(NTRK,L)=0
                  END DO
                  ITOV(NTRK  )=0
                  ITTY(NTRK  )=1
                END IF
              END IF
  932       END DO
          END DO
          GO TO 930
        END IF
C       ...................................... CHANGE BETWEEN XYZ AND R,FI,TETA
        IF(TANSW.EQ.'CC')  THEN
          IPC=MOD(IPC,2)+1
          GO TO 930
        END IF
        IF(ILIST.EQ.3) THEN
C         ..................................................... KINK LIST
C         ............................................................... ON/OF
          DO K=0,MON
            IF(TANSW.EQ.TON(K)) THEN
              DO N=1,NKN
                IF(ISEL(N,3).EQ.1) IONF(N,2)=K
              END DO
              GO TO 930
            END IF
          END DO
C         ............................................................. SYMBOLS
          DO K=2,MSYM
            IF(TANSW.EQ.TSYM(K)) THEN
              DO N=1,NKN
                IF(ISEL(N,3).EQ.1) ISYM(N,2)=K
              END DO
              GO TO 930
            END IF
          END DO
C         .............................................................. COLORS
          DO K=0,MCOL
            IF(TANSW.EQ.TCO1(K)) THEN
              DO N=1,NVX
                IF(ISEL(N,3).EQ.1) ICOL(N,2)=K
              END DO
              GO TO 930
            END IF
          END DO
          IF(TANSW.EQ.'AL') THEN
            CALL UFILL(ISEL(1,ILIST),1,NKN,1)
            GO TO 930
          END IF
          GO TO 927
C         ................................................. END OF KINK LIST
        END IF
        IF(TANSW.EQ.'MV'.AND.ILIST.NE.3) THEN
          NRV=0
          DO K=1,NTRK
            IF(ISEL(K,2).NE.0) THEN
              NRV=NRV+1
              IWRK(NRV)=ITRK(K)
            END IF
          END DO
          CALL DVPVCA(NRV,IWRK)
          CALL DVVTX0(NVX)
          BNUMDB(2,PYERDB)=NVX
          BNUMDB(3,PYERDB)=NVX
          PR1(2,3)=NVX
          ISEL(NVX,1)=1
          IONF(NVX,1)=1
          IF(IDLST.EQ.3) IDLST=2
          GO TO 930
        END IF
C       ........................................... RECALCULATE PRIMARY VERTEX
        IF(TANSW.EQ.'RV') THEN
          CALL DVPVCA(0,IWRK)
          NVX0=NVX
          CALL DVVTX0(NVX)
          BNUMDB(2,PYERDB)=NVX
          BNUMDB(3,PYERDB)=NVX
          PR1(2,3)=NVX
          CALL VZERO(ISEL,MSEL)
          DO K=NVX0,NVX
            ISEL(K,1)=1
            IONF(K,1)=1
          END DO
          IF(IDLST.EQ.4) IDLST=2
          GO TO 930
        END IF
        CALL DO_STR('P0: GET P0')
        IF(TANSW.EQ.'P0') THEN
          CALL DWRT('Which # on the ALPHA list? ')
          CALL DGETLN(TXTADW,LTXT,3)
          IF(LTXT.LE.0) GO TO 930
          READ(TXTADW(1:LTXT),1007,ERR=930) IPI0
 1007     FORMAT(I3)
          IPI0=ABS(IPI0)
          CALL DAC_NUM_OF_PI0(-IPI0,IAP0)
          IF(IAP0.GT.0) THEN
            CALL DV_QVEC_P0(IAP0,PPI0,ERRP0)
            DO K=1,NTRK
              IF(ITTY(K).LT.0) GO TO 48
            END DO
            IF(NTRK.GE.MSEL) THEN
              CALL DWRT('Too many tracks, delet first.')
              GO TO 930
            END IF
            NTRK=NTRK+1
            K=NTRK
   48       ITRK(K)=IPI0
            ISEL(NTRK,2)=1
            DO I=1,5
              IFRV(K,I)=0
            END DO
            ITOV(K)=0
            ITTY(K)=-1
          ELSE
            CALL DWRT('There is no p0 in the alpha list.#')
            GO TO 936
          END IF
          GO TO 930
        END IF
        CALL DO_STR('SV: Calculate secondary vertex')
        IF(TANSW.EQ.'SV'.AND.NTRK.GT.0) THEN
          NSV=0
          NP0=0
          DO K=1,NTRK
            IF(     ISEL(K,2).EQ.0) THEN
              IWRK(K)=0
            ELSE
              IF(ITTY(K).GT.0) THEN
                IWRK(K)=ITTY(K)
                NSV=NSV+1
              ELSE
                IWRK(K)=0
                NP0=NP0+1
              END IF
            END IF
          END DO
          IF(NSV.GE.2) THEN
            NC1=BNUMDB(2,CRFTDB)
            NN1=BNUMDB(2,NRFTDB)
            IF(NP0.GT.1) THEN
              CALL DWRT('Select only 1 PI0E.#')
              GO TO 936
            END IF
            CALL DVSVCA(NTRK,ITRK,IWRK,NRSEV,ITTYP,NTROW,NP0,PPI0,ERRP0)
            IF(NRSEV.EQ.0) THEN
              CALL DWRT('Reconstruction of secondary vertex failed!')
            ELSE
              CALL VZERO(ISEL(1,1),MSEL)
              CALL DVVTX0(NVX)
              ISEL(NVX,1)=1
              IONF(NVX,1)=1
              BNUMDB(2,PYERDB)=NVX
              BNUMDB(3,PYERDB)=NVX
              DO K=1,NTRK
                IF(ISEL(K,2).EQ.1) THEN
                  DO I=4,1,-1
                    IFRV(K,I+1)=IFRV(K,I)
                  END DO
                  IFRV(K,1)=NRSEV
                END IF
              END DO
            END IF
            IF(IDLST.EQ.3) IDLST=2
            IF(IDEB.NE.0) GO TO 930
            CALL DVTC0(NC2)
            IF(NC2.GT.NC1) THEN
              NTRK=NTRK+1
              ITTY(NTRK)=2
              IFRV(NTRK,1)=0
              ITOV(NTRK  )=NRSEV
              ISEL(NTRK,2)=0
              ITRK(NTRK)=NC2
              ICOCN(NTRK)=8
            END IF
            CALL DVTN0(NN2)
            IF(NN2.GT.NN1) THEN
              NTRK=NTRK+1
              ITTY(NTRK)=3
              IFRV(NTRK,1)=0
              ITOV(NTRK  )=NRSEV
              ISEL(NTRK,2)=0
              ITRK(NTRK)=NN2
              ICOCN(NTRK)=8
            END IF
            GO TO 930
          ELSE
            CALL DWRT('Not enough tracks ( # > 1 ) selected?')
            GO TO 936
          END IF
        END IF
C       ........................................... ERASE LAST SECONDARY VERTEX
        IF(TANSW.EQ.'EV') THEN
          CALL DVSVDL(LPYER,LCRFT,LNRFT)
          IF(LPYER.LE.0) THEN
            CALL DWRT('No vertex deleted.')
            GO TO 936
          END IF
          NVR=0
          LVX=NVX
          CALL DVVTX0(NVX)
          BNUMDB(2,PYERDB)=NVX
          BNUMDB(3,PYERDB)=NVX
          IF(NTRK.GT.0) THEN
            L=0
            DO K=1,NTRK
              IF(ITTY(K).EQ.1.OR.ITOV(K).NE.LVX) THEN
                L=L+1
                ITRK(L  )=ITRK(K)
                ISEL(L,2)=ISEL(K,2)
                DO I=1,5
                  IFRV(L,I)=IFRV(K,I)
                END DO
                ITOV(L  )=ITOV(K)
                ITTY(L  )=ITTY(K)
              END IF
            END DO
            NTRK=L
          END IF
          DO K=1,NTRK
            IF(IFRV(K,1).EQ.LVX) THEN
              DO I=1,4
                IFRV(K,I)=IFRV(K,I+1)
              END DO
              IFRV(K,5)=0
            END IF
          END DO
          CALL DVTN0(NUM)
          CALL DVTC0(NUM)
          GO TO 930
        END IF
      END IF
C     ........................................................... SELECT ALL
      IF(TANSW.EQ.'AL') THEN
        IF(ILIST.EQ.1) THEN
          CALL UFILL(ISEL(1,ILIST),1, NVX,1)
        ELSE
          CALL UFILL(ISEL(1,ILIST),1,NTRK,1)
        END IF
        GO TO 930
      END IF
C     .......................... VERTEX + TC PROCESSOR COMMANDS (NOT FO KINKS)
      IF(TANSW.EQ.'CO') THEN
C       ............................................... COLOR FOR OTHER TRACKS
        FOTH=.TRUE.
        GO TO 936
      END IF
      IF(TANSW.EQ.'CA') THEN
C       .................................. INVISIBILITY CUT ON HITS AND TRACKS
        PARADA(2,J_HCA)=1.
        GO TO 930
      END IF
      IF(TANSW.EQ.'CT') THEN
C       ........................................... INVISIBILITY CUT ON TRACKS
        PARADA(2,J_HCA)=0.
        GO TO 930
      END IF
C     ............................................................. VERTEX LIST
      IF(ILIST.EQ.1) THEN
C       ................................................................. ON/OF
        DO K=0,MON
          IF(TANSW.EQ.TON(K)) THEN
            DO N=1,NVX
              IF(ISEL(N,1).EQ.1) IONF(N,1)=K
            END DO
            GO TO 930
          END IF
        END DO
C       ............................................................... ELLIPSE
        IF(TANSW.EQ.TSYM(1)) THEN
          T=' '
C         ...................... ? changed 10.9.97  .... DO N=NKN+1,NVX
          DO N=1,NVX
            IF(ISEL(N,1).EQ.1) THEN
              CALL DVVTXD(N,NDGFR)
              IF(NDGFR.NE.0) THEN
                ISYM(N,1)=1
              ELSE
                T='No ellips can be drawn for some of the tracks.'
              END IF
            END IF
          END DO
          IF(T.NE.' ') CALL DWRT(T)
          GO TO 930
        END IF
C       ............................................................... SYMBOLS
        DO K=2,MSYM
          IF(TANSW.EQ.TSYM(K)) THEN
            DO N=1,NVX
              IF(ISEL(N,1).EQ.1) ISYM(N,1)=K
            END DO
            GO TO 930
          END IF
        END DO
C       ................................................................ COLORS
        DO K=0,MCOL
          IF(TANSW.EQ.TCO1(K)) THEN
            DO N=1,NVX
              IF(ISEL(N,1).EQ.1) ICOL(N,1)=K
            END DO
            GO TO 930
          END IF
        END DO
C       ............................................................. KINK LIST
      ELSE
C       ............................................................ TRACK LIST
C       ............................................ ERASE TRACKS FROM THE LIST
        IF(TANSW.EQ.'ES'.OR.TANSW.EQ.'EN') THEN
          IKEEP=1
          IF(TANSW.EQ.'ES') IKEEP=0
          CALL DTYANS('Erase '//TSELT(IKEEP)//' selected tracks? Y/N',
     &      'NY',IANSW)
          IF(IANSW.NE.2) GO TO 930
          IF(NTRK.GT.0) THEN
            L=0
            DO K=1,NTRK
              IF(ISEL(K,ILIST).EQ.IKEEP.OR.ITTY(K).GT.1) THEN
                L=L+1
                ITRK(L  )=ITRK(K)
                ISEL(L,2)=ISEL(K,2)
                DO I=1,5
                  IFRV(L,I)=IFRV(K,I)
                END DO
                ITOV(L  )=ITOV(K)
                ITTY(L  )=ITTY(K)
              END IF
            END DO
            NTRK=L
          END IF
          GO TO 930
        END IF
C       .......................................... SET ALL SELECTED COLORS TO 0
        IF(TANSW.EQ.'Z0') THEN
          CALL VZERO(COLRDT,NALL)
          CALL DSC0
          GO TO 930
        END IF
C       ................................................................ COLORS
        DO K=-1,MCOL
          IF(TANSW.EQ.TCO1(K)) THEN
            DO N=1,NTRK
              IF(ISEL(N,2).EQ.1) THEN
                IF(     ITTY(N).EQ.1) THEN
                  COLRDT(ITRK(N))=K
                ELSE IF(ITTY(N).GT.1) THEN
                  ICOCN(N)=K
                END IF
              END IF
            END DO
            CALL DSC0
            GO TO 930
          END IF
        END DO
      END IF
      CALL DO_STR('LP"L1 to L9: add last picked tracks')
      IF(TANSW(1:1).EQ.'L') THEN
        IF(NUMPDP.LE.0) THEN
          CALL DWRT('Pick tracks first')
          GO TO 930
        ELSE IF(TANSW(2:2).EQ.'P') THEN
          L=NUMPDP
        ELSE
          READ(TANSW(2:2),1029,ERR=929) L
 1029     FORMAT(I1)
          IF(L.LE.0) GO TO 929
          L=MIN(NUMPDP,L)
        END IF
        CALL VZERO(ISEL(1,2),MSEL)
        DO N=L,1,-1
          DO K=1,NTRK
            IF(ITTY(K).EQ.1.AND.LPTRK(N).EQ.ITRK(K)) THEN
              CALL DWRT('List contains already track '//
     &          DT2(FLOAT(LPTRK(N))))
              GO TO 928
            END IF
          END DO
          IF(NTRK.GE.MSEL) THEN
            CALL DWRT('Too many tracks, delet first.')
          ELSE
            NTRK=NTRK+1
            ITRK(NTRK  )=LPTRK(N)
            ISEL(NTRK,2)=1
            DO I=1,5
              IFRV(NTRK,I)=0
            END DO
            ITOV(NTRK  )=0
            ITTY(NTRK  )=1
          END IF
  928   END DO
        ILIST=2
        IDLST=3
        GO TO 930
      END IF
C     ............................................... REDISPLAY SELECTED WINDOW
  927 CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 940
  929 CALL DWR_IC(TANSW)
      GO TO 936
C     ................................................................ DISPLAY
  940 CALL DSCTR
      NAR=IAREDO
      DO M=0,MPNWDW
        IF(NWINDW(M,NAR).EQ.-2.OR.NWINDW(M,NAR).EQ.1) THEN
          PSTODS(1,J_HCA,M,IWUSDO)=PARADA(2,J_HCA)
        END IF
      END DO
      CALL DPCEAR(NAR)
      IAREDO=NAR
      GO TO 936
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------------------  DVXROV
CH
      ENTRY DVXROV
CH
CH --------------------------------------------------------------------
CH
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_RXX,J_RYY,J_RZZ'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_RXX,J_RYY,J_RZZ)
      TPARDA=
     &  'J_RVN'
      CALL DPARAM(43
     &  ,J_RVN)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      NSV=PARADA(2,J_RVN)
      CALL DVVTX(NSV,NTYPE,XYZ )
      PARADA(2,J_RXX)=XYZ(1)
      PARADA(2,J_RYY)=XYZ(2)
      PARADA(2,J_RZZ)=XYZ(3)
      IF(NTYPE.LE.1.OR.NTYPE.EQ.6) THEN
        CALL DWRT('Phi, theta not changed for this type of vertex.')
      ELSE
        DO N=1,NTRK
          IF(NSV.EQ.ITOV(N)) THEN
            KT=ITRK(N)
            IF(ITTY(N).EQ.2) THEN
              CALL DVTCP(KT,PX,PY,PZ,IDUM)
            ELSE IF(ITTY(N).GT.0) THEN
              CALL DVTNP(KT,PX,PY,PZ)
            END IF
            CALL DPOLCO(PX,PY,PZ,RDUM,PARADA(2,J_PTE),PARADA(2,J_PFI))
            RETURN
          END IF
        END DO
        CALL DWRT('No track to this vertex.')
      END IF
      RETURN
CH --------------------------------------------------------------------  DVXPV
CH
      ENTRY DVXPV
CH
CH --------------------------------------------------------------------
CH
C     ......................................... SEND POSITION OF PRIMARY VERTEX
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_RXX,J_RYY,J_RZZ'
      CALL DPARAM(15
     &  ,J_RXX,J_RYY,J_RZZ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(BNUMDB(2,PYERDB).GT.0) THEN
        CALL DVVTX0(NVT)
        IF(NVT.EQ.0) THEN
          CALL DWRT('PYER is empty.')
          RETURN
        END IF
        N=PR1(2,3)
        IF(N.GT.NVT) THEN
          DO N=NVT,1,-1
            CALL DVVTX(K,NTYPE,XYZ)
            IF(NTYPE.EQ.1.OR.NTYPE.EQ.6) THEN
              PR1(2,3)=N
              GO TO 511
            END IF
          END DO
          PR1(2,3)=1.
          N=1
        END IF
        CALL DVVTX(N,NTYPE,XYZ)
        IF(NTYPE.NE.1.AND.NTYPE.NE.6)
     &    CALL DWRT('Check type of primaty vertex = '
     &    //DT2(FLOAT(NTYPE)))
  511   PARADA(2,J_RXX)=XYZ(1)
        PARADA(2,J_RYY)=XYZ(2)
        PARADA(2,J_RZZ)=XYZ(3)
      ELSE
        CALL DWRT('No primary vertex available.')
        PARADA(2,J_RXX)=0.
        PARADA(2,J_RYY)=0.
        PARADA(2,J_RZZ)=0.
      END IF
      RETURN
CH --------------------------------------------------------------------  DVX0
CH
      ENTRY DVX0
CH
CH --------------------------------------------------------------------
CH
C     ..................................................... RUN INITIALISATION
      DO N=1,MP2
        PR2(1,N,2)=-BNUMDB(2,FRFTDB)
        PR2(3,N,2)= BNUMDB(2,FRFTDB)
      END DO
      CALL VZERO(IFRV,MSEL*5)
      CALL VZERO(ITOV,MSEL)
      ILIST=1
      CALL UFILL(ISEL,1,MSEL,1)
      PR1(2,3)=1.
      CALL UFILL(XYZR,1,3,999.)
      CALL UFILL(FIR,1,MR,999.)
      CALL UFILL(TER,1,MR,999.)
      CALL UFILL(ALR,1,MR,999.)
      NVR=0
      NTRK=0
      NKN=0
      CALL UFILL(ISYM(1,2),2,MSEL,ISYM(1,2))
      CALL UFILL(ICOL(1,2),2,MSEL,ICOL(1,2))
      CALL UFILL(IONF(1,2),2,MSEL,IONF(1,2))
      CALL DVTC0(NUM)
      DO K=1,NUM
        NTRK=NTRK+1
        ITTY(NTRK)=2
        ITRK(NTRK)=K
        IFRV(NTRK,1)=0
        CALL DVTCF(K,ITOV(NTRK))
      END DO
      CALL DVTN0(NUM)
      DO K=1,NUM
        NTRK=NTRK+1
        ITTY(NTRK)=2
        ITRK(NTRK)=K
        IFRV(NTRK,1)=0
        CALL DVTNF(K,ITOV(NTRK))
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVX1
CH
      ENTRY DVX1(NV1,NP1,FOUT,SIGMA)
CH
CH --------------------------------------------------------------------
CH
C     ................................... RETURN VERTEX STATUS,SYMBOL AND COLOR
      IF(IONF(NV1,NP1).EQ.0) THEN
        FOUT=.TRUE.
      ELSE
        FOUT=.FALSE.
        CALL DGLEVL(ICOL(NV1,NP1))
        IF(ISYM(NV1,NP1).EQ.1) THEN
          SIGMA=PR1(2,1)
        ELSE
          SIGMA=0.
          CALL DQPD0(IQS(ISYM(NV1,NP1)),PR1(2,2),0.)
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
CH --------------------------------------------------------------------  DVX1
CH
      ENTRY DVXC(NV1,JCOL)
CH
CH --------------------------------------------------------------------
CH
C     ................................... RETURN VERTEX STATUS,SYMBOL AND COLOR
      IF(IONF(NV1,1).EQ.0) THEN
        JCOL=-1
      ELSE
        JCOL=ICOL(NV1,1)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVXT
CH
      ENTRY DVXT(NHTR,XYZV)
CH
CH --------------------------------------------------------------------
CH
C     ................................................... INPUT = TRACK NUMBER
C
C     .......................................... KINK OR NUCL. INTERACTION
C     ......................................... end point must be set first
      NSVXDT=0
      IF(NHTR.GT.MTRKDT) THEN
        CALL VZERO(XYZV,3)
        RETURN
      END IF
      IF(KINVDT(1,NHTR).NE.0) THEN
C       ..................................... end point
        N3DIDT=1
        NSVXDT=KINVDT(1,NHTR)
      END IF
      IF(KINVDT(2,NHTR).NE.0) THEN
C       ..................................... start point
        CALL DVKNV(KINVDT(2,NHTR),XYZV)
        N3DIDT=1
        RETURN
      END IF
      IF(NHTR.GT.0) THEN
C       ................. SET FLAG IF TRACK = 2D : IT HAS ONLY ITC COORDINATES
        N3DIDT=1
        CALL=DVCHT(NHTR,NCORD)
        IF(NCORD.LE.0) THEN
          CALL=DVCHV(NHTR,NCORD)
          IF(NCORD.LE.0) N3DIDT=0
        END IF
      END IF
C     ................ OUTPUT = POSITION OF VERTEX TO WHICH THE TRACK BELONGS.
      IF(BNUMDB(2,PYERDB).LE.0..OR.NHTR.LE.0) THEN
        CALL VZERO(XYZV,3)
        RETURN
      END IF
      KVXT=PR1(2,3)
      DO K=1,NTRK
        IF(NHTR.EQ.ITRK(K).AND.ITTY(K).EQ.1) THEN
          IF(IFRV(K,1).NE.0) KVXT=IFRV(K,1)
          GO TO 400
        END IF
      END DO
  400 CALL DVVTX(KVXT,NTYPE,XYZV)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------  DVXTN
CH
      ENTRY DVX_PR_VERTEX(XYZV)
CH
CH --------------------------------------------------------------------
CH
C     .......................... XYZV = coordinates of primary vertex
      IF(BNUMDB(2,PYERDB).LE.0) THEN
        CALL VZERO(XYZV,3)
      ELSE
        KVXT=PR1(2,3)
        CALL DVVTX(KVXT,NDUM,XYZV)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVXTN
CH
      ENTRY DVXTN
CH
CH --------------------------------------------------------------------
CH
C     ............................... STORE TWO POINTS FOR CONNECTING TRACKS
      CALL DVTN0(NUM)
      CALL DVTC0(NUM)
      NTRKDN=0
      CALL DVVTX0(NVT)
      T=' '
      IF(NVT.EQ.0) THEN
        CALL VZERO(XYZP(1,1),3)
        T='No primary vertex.'
      ELSE
        N=PR1(2,3)
        IF(N.GT.NVT) THEN
          CALL VZERO(XYZP(1,1),3)
          T='Wrong primary vertex selected in VX ("GT:VX").'
        ELSE
          CALL DVVTX(N,NTYPE,XYZP(1,1))
        END IF
      END IF
      DO K=1,NTRK
        IF(ITTY(K).GT.1) THEN
          CALL DVVTX(ITOV(K),NTYPE,XYZ)
          IF(ITTY(K).EQ.2) THEN
            R=XYZ(1)*XYZ(1)+XYZ(2)*XYZ(2)
          ELSE
            R=0.
          END IF
          IF(R.LT.R2MAX) THEN
            IF(     ITTY(K).EQ.2) THEN
              CALL DVTCP(ITRK(K),PXYZ(1),PXYZ(2),PXYZ(3),IC)
            ELSE IF(ITTY(K).GT.0) THEN
              CALL DVTNP(ITRK(K),PXYZ(1),PXYZ(2),PXYZ(3),IC)
            END IF
            IF(IFRV(K,1).EQ.0) THEN
              INVX=1
            ELSE
              INVX=2
              CALL DVVTX(IFRV(K,1),NTYPE,XYZP(1,2))
            END IF
            SS=0
            S2=0
            DO J=1,3
              SS=SS+PXYZ(J)*(XYZP(J,INVX)-XYZ(J))
              S2=S2+PXYZ(J)*PXYZ(J)
            END DO
            IF(S2.GT.0.) THEN
              NTRKDN=NTRKDN+1
              CALL UCOPY(XYZ,XYZTDN(1,1,NTRKDN),3)
              U=SS/S2
              DO J=1,3
                XYZTDN(J,2,NTRKDN)=XYZ(J)+PXYZ(J)*U
              END DO
              ICOLDN(NTRKDN)=ICOCN(K)
              IDSHDN(NTRKDN)=ITTY(K)-1
            END IF
          ELSE
            CALL DWRT(
     &        'Charged sec. track too long to be drawn.#')
          END IF
        END IF
      END DO
      IF(NTRKDN.GT.0.AND.T.NE.' ') CALL DWRT(T)
      END
