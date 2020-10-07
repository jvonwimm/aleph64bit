*DK DV_QVEC_IN
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DV_QVEC_IN
CH
      SUBROUTINE DV_QVEC_IN(NF,NL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION NF(*),NL(*),LIST(*),IOBJ(*),NCTR(*),IOCO(*),CTR(*)
      DIMENSION ITEHC(*)
      CHARACTER *(*) TOP,TDAU,TTYP,TANSW
      CHARACTER *3 DT3
      CHARACTER *2 TOPS
      CHARACTER *1 TM,T1
      DATA T1/'_'/
      DIMENSION LCOL(8)
      PARAMETER (MLIS=9)
      DIMENSION KL(MLIS),KF(MLIS)
      DIMENSION NUBA(4)
      DATA MDEB/0/
      CHARACTER *8 TNAM
      CHARACTER *49 TXT
      LOGICAL FIRST,FNEV,FL(MLIS),FLST(99)
      DATA FLST/99*.TRUE./
      DATA FIRST/.TRUE./,FNEV/.TRUE./,FL/MLIS*.TRUE./
      DATA N4/4/,R12/12./
      CHARACTER *1 TNUM(0:8)
      DATA TNUM/'0','1','2','3','4','5','6','7','8'/

      PARAMETER(MAXSP=200,MAXJT=20)
      DIMENSION ITRK(MAXJT),IFRF2(MAXSP)
      DIMENSION PRTRK(MAXSP),PRJET(MAXJT),PRHEM(2)
      DIMENSION LTAG(MAXJT)

      INCLUDE 'P_QCDE.INC'
C     .............................................. QMACRO.INC needs QCDE.INC
      INCLUDE 'P_QMACRO.INC'
      VALU(KI)=RW(KOQVEC+KI*KCQVEC+JQVAL)
      ICO1(KI)=IW(KOQVEC+KI*KCQVEC+JQCO1)
      ICO2(KI)=IW(KOQVEC+KI*KCQVEC+JQCO2)
      RMAS(KI)=RW(KOQVEC+KI*KCQVEC+JQMAS)
C
      IF(FNEV) THEN
C
        FNEV=.FALSE.
C       ................................. CHarged tracks
        KF(1)=KFCHT
        KL(1)=KLCHT
C       ................................. isolated cal-objects
        KF(2)=KFIST
        KL(2)=KLIST
C       ................................. associated cal-objects
        KF(3)=KFAST
        KL(3)=KLAST
C       ................................. reconstructed V0's
        KF(4)=KFV0T
        KL(4)=KLV0T
C       ................................. energy flow from Patrick Junot
        KF(5)=KFEFT
        KL(5)=KLEFT
C       ................................. neutral cal-objects
        KF(6)=KFNET
        KL(6)=KLNET
C       ................................. Photons from GAMPEC
        KF(7)=KFGAT
        KL(7)=KLGAT
C       ................................. jets from EJET
        KF(8)=KFJET
        KL(8)=KLJET
C       ................................. MC TRUTH
        KF(9)=KFMCT
        KL(9)=KLMCT
        DO K=1,9
          NF(K)=KF(K)
          NL(K)=KL(K)
        END DO
      END IF
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_DEF_COL_IN
CH
      ENTRY DV_QVEC_DEF_COL_IN
CH
CH --------------------------------------------------------------------
CH
CH
      DO K=1,9
        FL(K)=.TRUE.
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DV_QVEC_NEW_EV
CH
      ENTRY DV_QVEC_NEW_EV(EF)
CH
CH --------------------------------------------------------------------
CH
CH
      IF(FIRST) THEN
        KUCONS=N4
        CALL QMINIT
        KUPTER=IW(6)
        KUPRNT=IW(6)
C
        JQVAL=JQVEUS
        JQCO1=JQVEUS+1
        JQCO2=JQVEUS+2
        JQMAS=JQVEUS+3
C
        FIRST=.FALSE.
      END IF
      CALL QMRDSB(IEND)
      CALL QMEVNT
      IF(MDEB.EQ.0) THEN
        CALL DVLBKN('FRFT',4,NUBA,NUMB)
        DO K=1,NUMB
          IF(NUBA(K).EQ.3) THEN
            CALL MSWAP(IW,'FRFT',0,'FRFT',3)
            GO TO 300
          END IF
        END DO
      END IF
  300 EF=0.
      DO K=KFEFT,KLEFT
        EF=EF+QE(K)
      END DO
      FNEV=.TRUE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DV_QVEC_PTOT
CH
      ENTRY DV_QVEC_PTOT(NAL,PTOT)
CH
CH --------------------------------------------------------------------
CH
      PTOT=QP(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_VECTOR
CH
      ENTRY DV_QVEC_VECTOR(NAL,PX,PY,PZ,P,IC,NCOL1,NCOL2)
CH
CH --------------------------------------------------------------------
CH
CH
C     ........................................ get object variables from QVEC
C     .............................................. SEE ALPHA MANUAL PAGE 83
      PX=QX(NAL)
      PY=QY(NAL)
      PZ=QZ(NAL)
      P =QP(NAL)
      IC=KCH(NAL)
      VAL=VALU(NAL)
      IF(VAL.GT.0.) THEN
        IC=999
      ELSE
        IC=KCH(NAL)
      END IF
      NCOL1=ICO1(NAL)
      NCOL2=ICO2(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_VECTOR_POL
CH
      ENTRY DV_QVEC_VECTOR_POL(NAL,P,FI,TE,IC,NCOL1,NCOL2)
CH
CH --------------------------------------------------------------------
CH
CH
      SX=QX(NAL)
      SY=QY(NAL)
      SZ=QZ(NAL)
      CALL DPOLCR(SX,SY,SZ,P,TE,FI)
      VAL=VALU(NAL)
      IF(VAL.GT.0.) THEN
        IC=999
      ELSE
        IC=KCH(NAL)
      END IF
      NCOL1=ICO1(NAL)
      NCOL2=ICO2(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_VECTOR_COL
CH
      ENTRY DV_QVEC_VECTOR_COL(NAL,NCOL1,NCOL2)
CH
CH --------------------------------------------------------------------
CH
CH
      NCOL1=ICO1(NAL)
      NCOL2=ICO2(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------- DV_QVEC_OBJECT
CH
      ENTRY DV_QVEC_OBJECT(NAL,P,FI,TE,IC,RM,TM,NCOL1,NCOL2)
CH
CH --------------------------------------------------------------------
CH
CH
      SX=QX(NAL)
      SY=QY(NAL)
      SZ=QZ(NAL)
      CALL DPOLCR(SX,SY,SZ,P,TE,FI)
C      IF(IDEB.GT.0.AND.NAL.GT.IDEB) THEN
C        WRITE(TXT,1234) NAL,P,FI,TE,SX,SY,SZ
C 1234   FORMAT(I6,F10.3,5F6.0)
C        CALL DWRT(TXT)
C      END IF
      TM=' '
      VAL=VALU(NAL)
      IF(VAL.GT.0.) THEN
        P=VAL
        IC=999
      ELSE
        RM=QM(NAL)
        RMU=RMAS(NAL)
        IF(RMU.GT.0.) TM=T1
        IC=KCH(NAL)
      END IF
      NCOL1=ICO1(NAL)
      NCOL2=ICO2(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_V0_PV
CH
      ENTRY DV_QVEC_V0_PV(IVR)
CH
CH --------------------------------------------------------------------
CH
CH
      NTY=KVTYPE(KFREV)
      IF(NTY.EQ.1) THEN
        VXP=QVX(KFREV)
        VYP=QVY(KFREV)
      ELSE
        VXP=0.
        VYP=0.
C       CALL DWRT('X,Y of primary vertex set to 0.')
      END IF
      CALL DSCTR
      CALL DPARGI(98,'VNP',NTPIT)
      CALL=DVTQ0(NTPC)
      IF(NTPC.NE.0) CALL=DVCHT0(NTPC)
      CALL=DVIT0(NITC)
      IF(NITC.NE.0) CALL=DVCHI0(NITC)
      IVRO=IVR
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_V0
CH
      ENTRY DV_QVEC_V0(NAL,NCTR,LT2,TDAU)
CH
CH --------------------------------------------------------------------
CH
CH
      L=LENOCC(TDAU)+2
      LD=LT2-L+1
      IF(LD.LT.3) RETURN
      NSV=KENDV(NAL)
      VXS=QVX(NSV)
      VYS=QVY(NSV)
      RHO=SQRT((VXS-VXP)**2+(VYS-VYP)**2)
      TDAU(L:L+2)=DT3(RHO)
      L=L+4
      NDAU=KNDAU(NAL)
      KFCH1=KFCHT-1
      DO K=1,NDAU
        I1DAU=KDAU(NAL,K)
        I2DAU=KCHT(I1DAU)
c       I3DAU=KEFOLT(I2DAU)
        I3DAU=I2DAU-KFCH1
        IF(RHO.GT.R12) THEN
          NP=0
          IF(NTPC.GT.0) THEN
            CALL DVCHT(I3DAU,NTP)
            DO KIND=1,NTP
              N=IDVCHT(KIND)
              IF(DVTP(IVRO,N).LT.RHO) NP=NP+1
            END DO
          END IF
          IF(NITC.GT.0) THEN
            CALL DVCHI(I3DAU,NTP)
            DO KIND=1,NTP
              N=ABS(IDVCHI(KIND))
              IF(DVIT(IVRO,N).LT.RHO) NP=NP+1
            END DO
          END IF
          IF(NP.GE.NTPIT) THEN
            DO J=1,8
              IF(TDAU(J:J+1).EQ.'V0') THEN
                TDAU(J:J+1)='V?'
                GO TO 211
              END IF
            END DO
          END IF
        END IF
  211   IF(L.LT.LT2) THEN
          IF(I3DAU.LT.99) THEN
            WRITE(TDAU(L:L+1),2000) I3DAU
 2000       FORMAT(I2)
          ELSE
            TDAU(L+1:L+1)='*'
          END IF
        ELSE
          TDAU(LT2-1:LT2)='+.'
          RETURN
        END IF
        L=L+3
        IF(LD.GE.16) THEN
          IF(I3DAU.GT.0) THEN
            TDAU(L-1:L-1)='_'
            CALL DACOL(NCTR(I3DAU),TDAU(L:L+1))
          END IF
          L=L+3
        END IF
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_DEF_COL
CH
      ENTRY DV_QVEC_DEF_COL(NLIST,KCF,NCTR)
CH
CH --------------------------------------------------------------------
CH
CH
      IF(FL(NLIST)) THEN
        FL(NLIST)=.FALSE.
        KK=KOQVEC+JQCO1+KF(NLIST)*KCQVEC
        IF(     KCF.GT.0.AND.NLIST.EQ.1) THEN
          NFRFT=0
          DO K=KF(NLIST),KL(NLIST)
            NFRFT=NFRFT+1
            IW(KK)=NCTR(NFRFT)
            KK=KK+KCQVEC
          END DO
        ELSE IF(KCF.GT.0.AND.NLIST.EQ.5) THEN
          CALL DPARGI(98,'ANA',ICNAS)
          DO K=KF(NLIST),KL(NLIST)
            NFRFT=KEFOLT(K)
            IF(NFRFT.NE.0) THEN
              IW(KK)=NCTR(NFRFT)
            ELSE
              IW(KK)=ICNAS
            END IF
            KK=KK+KCQVEC
          END DO
        ELSE
          CALL DPAR_GET_ARRAY(47,'CC1',8,LCOL)
          CALL DPARGV(70,'FFC',2,EFAC)
          DO K=KF(NLIST),KL(NLIST)
            PP=QP(K)
            CALL DSCLOG(PP,EFAC,LCOL,IW(KK))
            KK=KK+KCQVEC
          END DO
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
CH --------------------------------------------------- DV_QVEC_SET_COL
CH
      ENTRY DV_QVEC_SET_COL(NAL,NCOL)
CH
CH --------------------------------------------------------------------
CH
CH
      IF(NCOL.EQ.-9) THEN
        IW(KOQVEC+JQCO2+NAL*KCQVEC)=ICO1(NAL)
      ELSE
        IW(KOQVEC+JQCO2+NAL*KCQVEC)=NCOL
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_SET_COL_AR
CH
      ENTRY DV_QVEC_SET_COL_AR(NAL1,NAL2,NCOL)
CH
CH --------------------------------------------------------------------
CH
CH
      IF(NAL2.GE.NAL1) THEN
        KK=KOQVEC+JQCO2+NAL1*KCQVEC
        DO N=NAL1,NAL2
          IW(KK)=NCOL
          KK=KK+KCQVEC
        END DO
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_GET_COL
CH
      ENTRY DV_QVEC_GET_COL(NAL,NCOL)
CH
CH --------------------------------------------------------------------
CH
CH
      NCOL=ICO2(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_COL_FRFT
CH
      ENTRY DV_QVEC_COL_FRFT(NLIST,IOCO,CNAS,NUM,CTR)
CH
CH --------------------------------------------------------------------
CH
CH
C     ..... FRFT tracks get color of CH or EF objects
C     ..... INPUT:   NLIST  =         1 or  5
C     .............  IOCO = color array of other objects
C     .............  CNAS = color of non associated tracks
C     .............  NUM  = # of FRFT tracks
C     ..... Output:  CTR  = manual color array of FRFT: COLRDT
C
      IF(NLIST.EQ.1) THEN
        NFRFT=0
        DO K=KF(NLIST),KL(NLIST)
          NFRFT=NFRFT+1
          ICOL=ICO2(K)
          IF(ICOL.EQ.0) THEN
            ICOL=IOCO(NLIST)
            IF(ICOL.EQ.0) ICOL=ICO1(K)
          END IF
          CTR(NFRFT)=ICOL
        END DO
      ELSE IF(NLIST.EQ.5) THEN
        CALL UFILL(CTR,1,NUM,CNAS)
        DO K=KF(NLIST),KL(NLIST)
          NFRFT=KEFOLT(K)
          IF(NFRFT.NE.0) THEN
            ICOL=ICO2(K)
            IF(ICOL.EQ.0) THEN
              ICOL=IOCO(NLIST)
              IF(ICOL.EQ.0) ICOL=ICO1(K)
            END IF
            CTR(NFRFT)=ICOL
          END IF
        END DO
      ELSE
        CALL DWRT('Only color from CH or EF is accepted.')
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_DEF_COL
CH
      ENTRY DV_QVEC_COL_FROM_FRFT(NLIST,NCTR)
CH
CH --------------------------------------------------------------------
CH
CH
      KK=KOQVEC+JQCO2+KF(NLIST)*KCQVEC
      IF(     NLIST.EQ.1) THEN
        NFRFT=0
        DO K=KF(NLIST),KL(NLIST)
          NFRFT=NFRFT+1
          IW(KK)=NCTR(NFRFT)
          KK=KK+KCQVEC
        END DO
      ELSE IF(NLIST.EQ.5) THEN
        CALL DPARGI(98,'ANA',ICNAS)
        DO K=KF(NLIST),KL(NLIST)
          NFRFT=KEFOLT(K)
          IF(NFRFT.NE.0) THEN
            IW(KK)=NCTR(NFRFT)
          ELSE
            IW(KK)=ICNAS
          END IF
          KK=KK+KCQVEC
        END DO
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------
CH
      ENTRY DV_QVEC_COL_CH_TO_EF(IOCO)
CH
CH --------------------------------------------------------------------
CH
CH
      CALL DPARGI(98,'ANA',ICNAS)
      KK=KOQVEC+JQCO2+KF(5)*KCQVEC
      DO K=KF(5),KL(5)
        NCHTR=KEFOLT(K)
        IF(NCHTR.NE.0) THEN
          NALP=NCHTR+KF(1)-1
          ICOL2=ICO2(NALP)
          IW(KK)=ICOL2
        ELSE
          IW(KK)=ICNAS
        END IF
        KK=KK+KCQVEC
      END DO
      IOCO(5)=IOCO(1)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_DEF_COL
CH
      ENTRY DV_QVEC_COL_EF_TO_CH(IOCO)
CH
CH --------------------------------------------------------------------
CH
CH
      CALL DPARGI(98,'ANA',ICNAS)
      KK=KOQVEC+JQCO2+KF(1)*KCQVEC
      NCHTR=0
      DO K=KF(1),KL(1)
        NCHTR=NCHTR+1
        DO L=KF(5),KL(5)
          IF(NCHTR.EQ.KEFOLT(L)) THEN
            ICOL2=ICO2(L)
            IW(KK)=ICOL2
            GO TO 71
          END IF
        END DO
        IW(KK)=ICNAS
   71   KK=KK+KCQVEC
      END DO
      IOCO(1)=IOCO(5)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DV_QVEC_EF_TYPE
CH
      ENTRY DV_QVEC_EF_TYPE(NAL,TTYP)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      N=KEFOTY(NAL)
      TTYP(2:2)=TNUM(N)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DV_QVEC_EF_ITYPE
CH
      ENTRY DV_QVEC_EF_ITYPE(NAL,ITYPE)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      ITYPE=KEFOTY(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DV_QVEC_EF_FRFT_PECO
CH
      ENTRY DV_QVEC_EF_TEH(NAL,ITEHC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      ITEHC(1)=KEFOLT(NAL)
      ITEHC(2)=KEFOLE(NAL)
      ITEHC(3)=KEFOLH(NAL)
      ITEHC(4)=KEFOLC(NAL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DV_QVEC_GET_INDEX
CH
      ENTRY DV_QVEC_GET_INDEX(ILIST,IFRFT,NAL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      IF(     ILIST.EQ.1) THEN
        NAL=KF(1)-1+IFRFT
        IF(NAL.GT.KL(1)) NAL=0
      ELSE IF(ILIST.EQ.5) THEN
        DO NAL=KF(5),KL(5)
          IF(IFRFT.EQ.KEFOLT(NAL)) RETURN
        END DO
        NAL=0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DV_QVEC_GET_TRACK
CH
      ENTRY DV_QVEC_GET_TRACK(NAL,ILIST,IFRFT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      IF(     NAL.GE.KF(1).AND.NAL.LE.KL(1)) THEN
        ILIST=1
        IFRFT=NAL-KF(1)+1
      ELSE IF(NAL.GE.KF(5).AND.NAL.LE.KL(5)) THEN
        ILIST=5
        IFRFT=KEFOLT(NAL)
      ELSE
        ILIST=0
        IFRFT=0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DV_QVEC_SPHERICITY
CH
      ENTRY DV_QVEC_SPHERICITY(NAL)
CH
CH --------------------------------------------------------------------
CH
      CALL QJSPHE(VAL,'SPHE',KRECO)
      NSCR=KPDIR('SPHE',KRECO)
      NAL=KVSAVE(NSCR,' ')
      RW(KOQVEC+JQVAL+NAL*KCQVEC)=VAL
      CALL DPARGI(98,'ASP',JCOL)
      IW(KOQVEC+JQCO2+NAL*KCQVEC)=JCOL
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------- DV_QVEC_THRUST
CH
      ENTRY DV_QVEC_THRUST(NAL)
CH
CH --------------------------------------------------------------------
CH
      CALL QJTHRU(VAL,'THRU',KRECO)
      NSCR=KPDIR('THRU',KRECO)
      NAL=KVSAVE(NSCR,' ')
      RW(KOQVEC+JQVAL+NAL*KCQVEC)=VAL
      CALL DPARGI(98,'ATH',JCOL)
      IW(KOQVEC+JQCO2+NAL*KCQVEC)=JCOL
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------- DV_QVEC_VMISS
CH
      ENTRY DV_QVEC_VMISS(NAL)
CH
CH --------------------------------------------------------------------
CH
      CALL QJMISS(VAL,'MISS',KRECO,0)
      NSCR=KPDIR('MISS',KRECO)
      NAL=KVSAVE(NSCR,' ')
      CALL DPARGI(98,'AMI',JCOL)
      IW(KOQVEC+JQCO2+NAL*KCQVEC)=JCOL
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DV_QVEC_ADD_4V
CH
      ENTRY DV_QVEC_ADD_4V(NADD,NAL)
CH
CH --------------------------------------------------------------------
CH
CH
      IF(NADD.EQ.0) THEN
        NAL=KVNEW(DUMMY)
      ELSE
        VAL=VALU(NADD)
        IF(VAL.EQ.0.) THEN
          CALL QVADDN(NAL,NADD)
          CALL DPARGI(98,'AV4',JCOL)
          IW(KOQVEC+JQCO2+NAL*KCQVEC)=JCOL
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
CH ----------------------------------------------- DV_QVEC_SEC_VERTEX
CH
      ENTRY DV_QVEC_SEC_VERTEX(TANSW,LSV,LIST,NAL,PMASS)
CH
CH --------------------------------------------------------------------
CH
CH
      JSV=0
      DO K=1,LSV
        VAL=VALU(LIST(K))
        IF(VAL.EQ.0.) THEN
          JSV=JSV+1
          LIST(JSV)=LIST(K)
        END IF
      END DO
      IF(TANSW.EQ.'SV') THEN
        NAL=KVFITN(JSV,LIST,'SEC_V')
      ELSE
        NAL=KVFTMC(JSV,LIST,'SEC_V',FLST,PMASS)
      END IF
      CALL DPARGI(98,'AV4',JCOL)
      IW(KOQVEC+JQCO2+NAL*KCQVEC)=JCOL
      LSV=JSV
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DV_QVEC_SET_OPT
CH
      ENTRY DV_QVEC_SET_OPT(TOP)
CH
CH --------------------------------------------------------------------
CH
      CALL QJOPTR(TOP,' ')
      TOPS=TOP
      NTO=0
      IF(     TOP.EQ.'CH') THEN
        NFR=KFCHT
        NTO=KLCHT
      ELSE IF(TOP.EQ.'EF') THEN
        NFR=KFEFT
        NTO=KLEFT
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DV_QVEC_JADE
CH
      ENTRY DV_QVEC_JADE(MAXJ,NJETS,LIST,IOBJ)
CH
CH --------------------------------------------------------------------
CH
      CALL DPARGV(100,'AYJ',2,YCUT)
      CALL DPARGV(100,'AEV',2,EVIS)
      TNAM='QIPBJETS'
C     TNAM='JJETS'
      CALL QJMMCL(NJETS,TNAM,KRECO,YCUT,EVIS)
      CALL DPARGI(98,'AJJ',JCOL)
      GO TO 700
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DV_QVEC_DURHAM
CH
      ENTRY DV_QVEC_DURHAM(MAXJ,NJETS,LIST,IOBJ)
CH
CH --------------------------------------------------------------------
CH
      CALL DPARGV(100,'AYD',2,YCUT)
      CALL DPARGV(100,'AEV',2,EVIS)
      TNAM='QIPBJETS'
C     TNAM='DJETS'
      CALL QDMMCL(NJETS,TNAM,KRECO,YCUT,EVIS)
      CALL DPARGI(98,'AJD',JCOL)
  700 IF(NJETS.LE.0) RETURN
      IF(NJETS.GT.MAXJ) THEN
        CALL DWRT('Too many jets!#')
        NJETS=MAXJ
      END IF
      NSCR=KPDIR(TNAM,KRECO)
      LIST(1)=KVSAVE(NSCR,' ')
      IW(KOQVEC+JQCO2+LIST(1)*KCQVEC)=JCOL
      DO N=2,NJETS
C       LIST(N)=KFOLLO(LIST(N-1))
        NSCR=KFOLLO(NSCR)
        LIST(N)=KVSAVE(NSCR,' ')
        IW(KOQVEC+JQCO2+LIST(N)*KCQVEC)=JCOL
      END DO
      NUMJT=NJETS
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DV_QVEC_JET_CORR
CH
      ENTRY DV_QVEC_BTAG(NJETS,LIST)
CH
CH --------------------------------------------------------------------
CH
CH
      NUMJ=IW(NAMIND('NUMJ'))
      IW(NUMJ+1)=NJETS
      CALL QIPBTAG(IRET,NTRK,NJET,ITRK,IFRF2,PRTRK,PRJET,PRHEM,PREVT)
      IF(IRET.EQ.0) THEN
        IF(NJETS.NE.NJET) THEN
          CALL DWRT('BTAG mismatch')
          RETURN
        ELSE
          CALL VZERO(LTAG,MAXJT)
          DO N=1,NJET
            IT=KSAME(ITRK(N))
            DO K=1,NJETS
              IF(LIST(K).EQ.IT) LTAG(K)=N
            END DO
          END DO
          DO K=1,NJETS
            IT=LIST(K)
            IF(IT.NE.0) THEN
              THETA=ACOS(QCT(IT))*PIFCDK
              PHI=QPH(IT)*PIFCDK
              IF(PHI.LT.0.) PHI=PHI+360.
              PROZ=100.*PRJET(LTAG(K))
              WRITE(TXTADW,1285) K,QP(IT),PHI,THETA,PROZ
 1285         FORMAT('jet',I2,' P=',F5.1,' phi=',F4.0,' th.=',F4.0,
     &          F7.2,'% uds')
              CALL DWRC
            END IF
          END DO
C
C          DO N=1,NJET
C            IT=KSAME(ITRK(N))
C            THETA=ACOS(QCT(IT))*PIFCDK
C            PHI=QPH(IT)*PIFCDK
C            IF(PHI.LT.0.) PHI=PHI+360.
C            PROZ=100.*PRJET(N)
C            WRITE(TXTADW,1285) N,QP(IT),PHI,THETA,PROZ
C 1285       FORMAT('jet',I2,' P=',F5.1,' phi=',F4.0,' th.=',F4.0,
C     &        F7.2,'% uds')
C            CALL DWRC
C
        END IF  
      ELSE
        WRITE(TXTADW,1286) IRET
 1286   FORMAT('QIPBTAG failed see ALPHA manual. IRET=',I2,'#')
        CALL DWRC
      END IF  
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DV_QVEC_JET_CORR
CH
      ENTRY DV_QVEC_JET_CORR(NAL,NOBJ,IOBJ)
CH
CH --------------------------------------------------------------------
CH
CH
      NOBJ=0
      IF(NTO.GT.0) THEN
        DO I=NFR,NTO
          IF(XSAME(NAL,I)) THEN
            NOBJ=NOBJ+1
            IOBJ(NOBJ)=I
          END IF
        END DO
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DV_QVEC_SET_MASS
CH
      ENTRY DV_QVEC_SET_MASS(NAL,RM)
CH
CH --------------------------------------------------------------------
CH
CH
      IF(VALU(NAL).GT.0.) RETURN
      RMU=RMAS(NAL)
      RMN=RM
      IF(RMU.NE.0.) THEN
        IF(RM.EQ.-1.) THEN
          RMN=RMU-1.
          RW(KOQVEC+JQMAS+NAL*KCQVEC)=0.
        ELSE IF(RM.EQ.RMU) THEN
          RW(KOQVEC+JQMAS+NAL*KCQVEC)=0.
        END IF
      ELSE
C       ....... 1 is added to flag that original mass even if 0 was changed.
        RW(KOQVEC+JQMAS+NAL*KCQVEC)=1.+QM(NAL)
      END IF
      CALL QVSETM(NAL,RMN)
      RETURN
      END
*DK DV_QVEC_P0
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DV_QVEC_DEF_COL_IN
CH
      SUBROUTINE DV_QVEC_P0(NAL,PPI0,ERRP0)
CH
CH --------------------------------------------------------------------
CH
      DIMENSION PPI0(*),ERRP0(*)
      CALL QVGET3(PPI0,NAL)
      CALL QVGETS(ERRP0,NAL)
      END
*DK DV_QVEC_LOCK
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DV_QVEC_LOCK
CH
      SUBROUTINE DV_QVEC_LOCK
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DO L=1,NLINDA
        IF(FSELDA(L)) CALL QLTRK(IALPDA(L))
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DV_QVEC_UNLOCK
CH
      ENTRY DV_QVEC_UNLOCK
CH
CH --------------------------------------------------------------------
CH
CH
      DO L=NCALDA,NLINDA
        IF(FSELDA(L)) CALL QLUTRK(IALPDA(L))
      END DO
      END
*DK DV_QVEC_PART_NAME
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------- DV_QVEC_PART_NAME
CH
      SUBROUTINE DV_QVEC_PART_NAME(INAM,TNAM)
CH
CH --------------------------------------------------------------------
CH
      CHARACTER *(*) TNAM
      CHARACTER *12 CQPART
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        I=KPART('E+')
      END IF
      TNAM=CQPART(INAM)
      END
C
C
C
C
C
C
C
C
C
C
      SUBROUTINE QMINIT
CKEY INIT /INTERNAL
C----------------------------------------------------------------------
C! initialize analysis job
C----------------------------------------------------------------------
*IF .NOT.DOC
      SAVE IOLD,INEW
      INCLUDE 'P_QCDE.INC'
      DATA IOLD,INEW/0,-1/
C----------------------------------------------------------------------
C
C           quasi BLOCK DATA
C

      CALL QDATA

C           interactive job ?
C
      CALL QAINTA
C
C
C           define NAMe Indices
C
      CALL QMNAMI
C
C           read data cards
C
Ccc      CALL QMCARD
C
C          initialize ALPHA
C
      CALL QMALPH
C
Ccccc      call qdbase

C
C          initialize particle table
C
      CALL QMPART
C
C          initialize formats for mini
C
      CALL MINFMT
C
C          initialize NanoDst if requested
C
      IF (XWNANO) CALL QNINIT
C
C          Initialization for YTOP
C
      CALL YTIJOB
C Initialise EBNEUT package when ENFLW required :
      IF (XFILEF) CALL EBINIT(IER)
C
      KSTATU = 0
C
      END

      SUBROUTINE QDBASE
      INCLUDE 'P_QCDE.INC'

C           read short-term database
C
      CALL QMDBAS
C
C           initialise database
C
      IF (KDEBUG .GT. 0)  CALL QWMESS ('_QMINIT_ Open data base')
      KUCONS = JUNIDB (0)
      CALL AOPDBS (' ',IER)
      WRITE(6,*) ' kucons=',KUCONS,IER
      IF (IER .NE. 0)
     +  CALL QWSYNT ('0_QMINIT_ Data base cannot be opened')
C    Swap database bank given on data cards with NR=IROLD to IRNEW.
C
      CALL ADBSWP(IOLD,INEW)
C
      CALL MDARD (IW, KUCONS, 'ADBS', 0)
      CALL MDARD (IW, KUCONS, 'ADBL', 0)
      IS = NLINK ('ADBS', 0)
      IL = NLINK ('ADBL', 0)
      IF (IS .GT. 0 .AND. IL .GT. 0)  THEN
        WRITE (KUPRNT, 1001)
     +    IW(IS+3), IW(IS+4), IW(IL+3), IW(IL+4)
      ELSE
        CALL QWMESS
     +    ('_QMINIT_ Data base : ADBS/ADBL is missing')
      ENDIF
 1001 FORMAT (' _QMINIT_ Data base version ADBS',2I7,
     +        ', ADBL',2I7)
      END


      SUBROUTINE QMRDSB(IEND)
CKEY EVENT /INTERNAL
C----------------------------------------------------------------------
C! read next event.
C  called from QMREAD
C----------------------------------------------------------------------
*IF .NOT.DOC
      SAVE LNEWR,IROLD,IEOLD
C
      DIMENSION LINDAT(2),LUTDAT(5)
      DIMENSION IPV(10),IAV(10)
      INCLUDE 'P_QCDE.INC'
      INCLUDE 'A_EVEHJJ.INC'
      INCLUDE 'A_RUNHJJ.INC'
      INTEGER BKINCA
      LOGICAL LNEWR
      LOGICAL CHKCLAS
      CHARACTER * 50 MESS
C
      DATA LNEWR /.TRUE./, IROLD /0/, IEOLD /0/
C----------------------------------------------------------------------
      IEND=0
C
C       get next record
C
      KCLASW = 0
C       get record class word if read from EDIR
      CALL ABGTWCL (KCLASW)
C
C       determine whether we are reading a Mini or a Nano , or not :
C
      CALL ALVSN(ITYP,IPV,IAV,IYR)
      INDATA=ITYP
      IF (ITYP.EQ.5) THEN
        XMINI = .TRUE.
      ELSE
        XMINI = .FALSE.
      ENDIF
      IF (ITYP.EQ.7) THEN
        XNANO = .TRUE.
      ELSE
        XNANO = .FALSE.
      ENDIF
C
C       clear ALPHA banks
C
      IF (.NOT. XCOPYJ)  CALL QMCLR
C
C----------------------------------------------------------------------
C       event record
C
   10 CONTINUE
C
      KNREIN = KNREIN + 1
      KFFILL = 0
      KNEVT = KNEVT + 1
      KNEFIL = KNEFIL + 1
      XWREVT = .FALSE.
      CALL ABRUEV (KRUN ,KEVT)
      IF(KRUN.NE.IROLD) CALL QMNEWR(IROLD,KRUN)
C     LOUTRL=IW(6)
C     LDEBRL=IW(6)
      IROLD=KRUN

C            flags from QMCARD
      XFILL=.TRUE.
      XFILCH=.TRUE.
      XFILCO=.TRUE.
      XFILV0=.TRUE.
      XFILGA=.TRUE.
      XFILJE=.TRUE.
      XFILEF=.TRUE.
      KEFOPT=1
      END

      SUBROUTINE QMEVNT(IRUN,IEVT)
CKEY EVENT /INTERNAL
C----------------------------------------------------------------------
C! event processing
C! called from QMAIN
C!                                                  H.Albrecht 20.09.88
C----------------------------------------------------------------------
      INCLUDE 'P_QCDE.INC'
C----------------------------------------------------------------------
C
C++   Fix some bank structure or content.
C++   This must be done if and only if RHAH is updated in QWRUNR.
C
      IF (.NOT.XCOPYJ .OR. XWMINI) THEN
        CALL FIXPECO
        CALL FIXPCRL
      ENDIF
C
C++ Force the track extrapolation for muon-id if MEXT card exists
      JMEXT=IW(NAMIND('MEXT'))
C
      IF(.NOT.XMINI .AND. (JMEXT.GT.0 .OR.
     &  (IW(NAMUID).LE.0 .AND.
     &  (IW(NAHMAD).GT.0.OR.IW(NAMCAD).GT.0)))) CALL QUMUPR

      CALL QFILL

      END

      SUBROUTINE NANFIL
      ENTRY NADDX
      ENTRY NAIRFD
      ENTRY NGPUT
      ENTRY MINQMU
      ENTRY MINVSN
      END
