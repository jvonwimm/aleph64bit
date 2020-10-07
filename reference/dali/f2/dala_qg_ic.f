CH
CH --------------------------------------------------------------------  DAR
CH
      SUBROUTINE DQFFAR(H1,V1,H2,V2,LA,LI)
CH
CH --------------------------------------------------------------------
CH
C!:  DRAW AREA IN DISPLAY COORDINATES and frame it
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(5),V(5)
      H(1)=H1
      H(3)=H2
      V(1)=V1
      V(3)=V2
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      CALL DGLEVL(LA)
      CALL DGAREA(5,H,V)
      CALL DGLEVL(LI)
      CALL DGDRAW(5,H,V)
      END
*DK DQSCA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQSCA
CH
      SUBROUTINE DQSCA(THV,U1,U2,TSCA,NSCA,TVR,NVR)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DW
      PARAMETER (MPOSDW=30,MPNWDW=12,MNUWDW=14)
      COMMON /DWINDC/NWINDW(0:MPNWDW,0:MPNWDW),
     &  NEXTDW(0:MPNWDW),LASTDW(0:MPNWDW),
     &  POSIDW(MPOSDW)
      COMMON /DWINDO/TWINDW(0:MNUWDW)
      CHARACTER *1 TWINDW
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) TVR
      CHARACTER *8 TVAR
      CHARACTER *1 THV
      CHARACTER *3 TSCA
      CHARACTER *5 T5
      CHARACTER *13 TL,TH
      DIMENSION NGR(4)
      DIMENSION SL(2),SH(2),DS(2)
      DIMENSION NS(2),H(3),V(3)
      DIMENSION MXH(0:12),MXV(0:12)
      DATA MXH/24,8,8,8,8,8,8,16,16, 8, 8, 8,16/
      DATA MXV/16,8,8,8,8,8,8, 8, 8,16,16,16,16/
      DATA P9,P11,P12,P13,P14,P29,P30/8.,6.,18.,10.,13.,100.,112./
C     DATA P9,P11,P12,P13,P14,P29,P30/8.,6.,18.,10.,16., 90.,102./
      DATA DHT2/20./,DVT2/13./
      DATA HPIC/3./,VPIC/3./

      W(S)=A*S+C
C
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(U1.EQ.U2) RETURN
      CALL DPARGI_24(95,'CTM',NCTM,ION)
      IF(ION.LE.0) RETURN
      CALL DGLEVL(NCTM)
C     ................................. text on scales for b3 rubber band
      DLINDD=1
      IF(U2.GT.U1) THEN
        S1=U1
        S2=U2
        TVAR=TVR
        NVAR=NVR
      ELSE
        S1=-U1
        S2=-U2
        TVAR(1:1)='-'
        TVAR(2:)=TVR
        NVAR=NVR+1
      END IF
      IF(THV.EQ.'H') THEN
        A=(HHGHDG(IAREDO)-HLOWDG(IAREDO))/(S2-S1)
        C=HLOWDG(IAREDO)-A*S1
        MX=MXH(IAREDO)
      ELSE
        A=(VHGHDG(IAREDO)-VLOWDG(IAREDO))/(S2-S1)
        C=VLOWDG(IAREDO)-A*S1
        MX=MXV(IAREDO)
      END IF
      CALL DQSCD(TSCA,NSCA,S1,S2,MX,SL,SH,DS,NS,TL,TH,NL,NH)
      IF(TVAR(1:3).EQ.'-&j') THEN
        TVAR=TVAR(2:NVR)
        NVAR=NVR-1
        TL(2:13)=TL(3:13)
        NL=NL-1
        L=14-NH
        TH(L:L)=' '
        NH=NH-1
      END IF
      NT=MAX(NH,NL)
      IF(NS(1).EQ.0.) GO TO 99
      MGR=0
      I=1
      J=1
      T5=' '
   10 IF(TVAR(J:J).EQ.'&') THEN
         MGR=MGR+1
         NGR(MGR)=I
         J=J+1
      END IF
      T5(I:I)=TVAR(J:J)
      I=I+1
      J=J+1
      IF(I.LE.55.AND.J.LE.NVAR) GO TO 10
      IF(THV.EQ.'H') THEN
C                                       ! HORIZONTAL
        WL=W(SL(1))
        WH=W(SH(1))
        V(2)=VLOWDG(IAREDO)
        V(1)=V(2)+POSIDW(27)
        IF(NS(2).EQ.0) THEN
          DW=A*DS(1)
          DO 711 WS=WL,WH+0.5*DW,DW
            H(1)=WS
            H(2)=WS
            CALL DGDRAW(2,H,V)
  711     CONTINUE
        ELSE
          W1=W(SL(2))
          W2=W(SH(2))
          DW=A*DS(2)
          N1=9
          V(3)=V(2)+POSIDW(26)
          DO 712 WS=WL,W1-0.5*DW,-DW
            H(1)=WS
            H(2)=WS
            N1=N1+1
            IF(N1.EQ.10) THEN
              CALL DGDRAW(2,H,V)
              N1=0
            ELSE IF(DW.GE.4.) THEN
              CALL DGDRAW(2,H,V(2))
            END IF
  712     CONTINUE
          N1=9
          DO 713 WS=WL,W2+0.5*DW,DW
            H(1)=WS
            H(2)=WS
            N1=N1+1
            IF(N1.EQ.10) THEN
              CALL DGDRAW(2,H,V)
              N1=0
            ELSE IF(DW.GE.4.) THEN
              CALL DGDRAW(2,H,V(2))
            END IF
  713     CONTINUE
        END IF
        VT=VMINDG(IAREDO)+P14
        W0=W(0.)
        CALL DQSCP(W0,WL,WH,NT,P9,HLOWDG(IAREDO),HHGHDG(IAREDO),HT)
        IF(W0.NE.9999.)               CALL DGTEXT(W0-P11,VT,'0',1)
        IF(WL.NE.9999..AND.TL.NE.' ') THEN
          CALL DQDRAW(WL,VT,WL,VT+DVT2)
          CALL DGTEXT(WL-P11,VT,TL,13)
        END IF
        IF(WH.NE.9999..AND.TH.NE.' ') THEN
          CALL DQDRAW(WH,VT,WH,VT+DVT2)
C         CALL DGTEXT(WH-P11,VT,'|',1)
          CALL DGTEXT(WH-P29,VT,TH,12)
        END IF
        HT=HT-P11
        CALL DPARGI_24(95,'CVA',NCVAR,IONVA)
        IF(IONVA.GT.0) THEN
          CALL DGLEVL(NCVAR)
          IF(MGR.EQ.0) THEN
            CALL DGTEXT(HT,VT,TVAR,NVAR)
          ELSE
            CALL DGTXTG(HT,VT,T5,5,NGR,MGR)
          END IF
          CALL DGLEVL(NCTM)
        END IF
        IF(TFILDC(1:4).EQ.'ATLA')
     &    CALL DGTEXT(HMINDG(IAREDO)+HPIC,VMINDG(IAREDO)+VPIC,TPICDO,2)
      ELSE
C                                       ! VERTICAL
        WL=W(SL(1))
        WH=W(SH(1))
        H(2)=HLOWDG(IAREDO)
        H(1)=H(2)+POSIDW(27)
        IF(NS(2).EQ.0) THEN
          DW=A*DS(1)
          DO 721 WS=WL,WH+0.5*DW,DW
            V(1)=WS
            V(2)=WS
            CALL DGDRAW(2,H,V)
  721     CONTINUE
        ELSE
          W1=W(SL(2))
          W2=W(SH(2))
          DW=A*DS(2)
          H(3)=H(2)+POSIDW(26)
          N1=9
          DO 722 WS=WL,W1-0.5*DW,-DW
            V(1)=WS
            V(2)=WS
            N1=N1+1
            IF(N1.EQ.10) THEN
              CALL DGDRAW(2,H,V)
              N1=0
            ELSE IF(DW.GE.4.) THEN
              CALL DGDRAW(2,H(2),V)
            END IF
  722     CONTINUE
          N1=9
          DO 723 WS=WL,W2+0.5*DW,DW
            V(1)=WS
            V(2)=WS
            N1=N1+1
            IF(N1.EQ.10) THEN
              CALL DGDRAW(2,H,V)
              N1=0
            ELSE IF(DW.GE.4.) THEN
              CALL DGDRAW(2,H(2),V)
            END IF
  723     CONTINUE
        END IF
        CALL DGTDIR(90)
        HT=HMINDG(IAREDO)+P13
        W0=W(0.)
        CALL DQSCP(W0,WL,WH,NT,P9,VLOWDG(IAREDO),VHGHDG(IAREDO),VT)
        IF(W0.NE.9999.)               CALL DGTEXT(HT,W0-P12,'0',1)
        IF(WL.NE.9999..AND.TL.NE.' ') THEN
          CALL DQDRAW(HT,WL,HT+DHT2,WL)
          CALL DGTEXT(HT,WL-P12,TL,12)
        END IF
        IF(WH.NE.9999..AND.TH.NE.' ') THEN
          CALL DQDRAW(HT,WH,HT+DHT2,WH)
C         CALL DGTEXT(HT,WH-P12,'|',1)
          CALL DGTEXT(HT,WH-P30,TH,12)
        END IF
        VT=VT-P12
        IF(IONVA.GT.0) THEN
          CALL DGLEVL(NCVAR)
          IF(MGR.EQ.0) THEN
            CALL DGTEXT(HT,VT,TVAR,NVAR)
          ELSE
            CALL DGTXTG(HT,VT,T5,5,NGR,MGR)
          END IF
          CALL DGLEVL(NCTM)
        END IF
      END IF
   99 DLINDD=PDCODD(2,LITRDD)
      END
*DK DQSCD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQSCD
CH
      SUBROUTINE DQSCD(TIN,NIN,S1,S2,MX,SL,SH,DS,NS,TL,TH,NL,NH)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  calculate scale
C    Inputs    :TIN=cm,deg ... , NIN=# of letters of TIN
C              :S1,S2 = 2 corners
C    Outputs   :SL(1),SH(1) = low and high edge of big   marks, DS(1)=step
C              :SL(2),SH(2) = low and high edge of small marks, DS(2)=step
C              :NS( ) = number of steps
C              :TL = text at low edge TH at high edge.  (12.3cm)
C              :S0 = position for variable name (phi)
C    Called by : DQSCA
C ---------------------------------------------------------------------
      CHARACTER *3 TIN
      CHARACTER *13 TL,TH
      DIMENSION NS(2)
      DIMENSION DD(5)
C      DOUBLE PRECISION SL(2),SH(2),DS(2),WL,WH,WD
      DIMENSION SL(2),SH(2),DS(2)
      DATA DD/1.,0.1,0.01,0.001,0.0001/
      NS(2)=0
      NS(1)=0
      X=ABS(S2-S1)
      IF(X.EQ.0.) RETURN
      DMIN=X/295.
      NF=1
      IF(X.GT.1.) THEN
        D=.1
    1   X=X*0.1
        D=D*10.
        IF(X.GT.1.) GO TO 1
      ELSE
    2   X=X*10.
        NF=NF+1
        IF(NF.GT.5) RETURN
        D=DD(NF)
        IF(X.LE.1.) GO TO 2
      END IF
      IF(S2.LT.S1) THEN
        W2=S1
        W1=S2
      ELSE
        W1=S1
        W2=S2
      END IF
      SMAX=W2-DMIN
      DO 700 J=1,2
        N1=W1/D
        N2=W2/D
        IF(W2.LT.0..AND.MOD(W2,D).NE.0.) N2=N2-1
        IF(W1.GT.0..AND.MOD(W1,D).NE.0.) N1=N1+1
        NS(J)=N2-N1+1
        IF(NS(J).LE.1) THEN
          D=D*0.1
          N1=W1/D
          N2=W2/D
          IF(W2.LT.0..AND.MOD(W2,D).NE.0.) N2=N2-1
          IF(W1.GT.0..AND.MOD(W1,D).NE.0.) N1=N1+1
          NS(J)=N2-N1+1
          IF(D.LE.0.1) NF=NF+1
        END IF
        SL(J)=N1*D
        SH(J)=N2*D
        IF(SH(J).GE.SMAX) THEN
          SH(J)=SH(J)-D
          NS(J)=NS(J)-1
        END IF
        DS(J)=D
        IF(NS(1).GT.MX) GO TO 10
        D=0.1*D
  700 CONTINUE
   10 CALL DQSCF(SL(1),TIN,NIN,'L',NF,TL,NL)
      CALL DQSCF(SH(1),TIN,NIN,'H',NF,TH,NH)
      END
*DK DQSCF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQSCF
CH
      SUBROUTINE DQSCF(S,TIN,NIN,TLH,NF,T,NT)
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
      CHARACTER *3 TIN
      CHARACTER *1 TLH
      CHARACTER *9 TSCR
      DATA TSCR/' '/
      CHARACTER *13 T
      CHARACTER *6 TF(5)
      DATA TF/'( I9 )','(F9.1)','(F9.2)','(F9.3)','(F9.4)'/
      IF(NF.GT.5) THEN
        T=' '
        RETURN
      END IF
      IF(S.EQ.0) THEN
        IF(TLH.EQ.'H') THEN
C            1234567890123
          T='           0'
        ELSE
          T=' 0           '
        END IF
        NT=2
        RETURN
      END IF
      IF(NF.NE.1) THEN
        WRITE(TSCR,TF(NF),ERR=1) S
      ELSE
        IS=S
        WRITE(TSCR,TF(NF),ERR=1) IS
      END IF
      DO 700 J=1,8
        IF(TSCR(J:J).NE.' ') GO TO 2
  700 CONTINUE
      J=9
      GO TO 2
    1 TSCR(9:9)='*'
      J=9
    2 IF(TLH.EQ.'H') THEN
        N2=12-NIN
        N1=N2-9+J
        T(1:N1-1)=' '
        T(N1:N2)=TSCR(J:9)
        T(N2+1:12)=TIN(1:NIN)
        T(13:13)=' '
        NT=14-N1
      ELSE
        N1=11-J
        T(1:1)=' '
        T(2:N1)=TSCR(J:9)
        N2=N1+NIN
        T(N1+1:N2)=TIN(1:NIN)
        T(N2+1:13)=' '
        NT=N2
      END IF
      END
*DK DQSCP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQSCP
CH
      SUBROUTINE DQSCP(S0,S1,S2,NT,D1,SLU,SHD,ST)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DIMENSION D(4),U(4)
      DATA Q/.3/
      DN=NT*D1
      S0D=S0-D1
      S0U=S0+D1
      S1D=S1-D1
      S1U=S1+DN
      S2D=S2-DN
      S2U=S2+D1
      D(1)=SLU
      U(1)=S1D
      D(2)=S1U
      U(2)=S2D
      D(3)=S2U
      U(3)=SHD
      D(4)=0.
      U(4)=0.
      IF     (S0D.LT.SLU) THEN
        S0=9999.
      ELSE IF(S0U.LT.S1D) THEN
        U(1)=S0D
        D(4)=S0U
        U(4)=S1D
      ELSE IF(S0D.LT.S1U) THEN
        S1=9999.
        U(1)=S0D
        D(2)=S0U
      ELSE IF(S0U.LT.S2D) THEN
        U(2)=S0D
        D(4)=S0U
        U(4)=S2D
      ELSE IF(S0D.LT.S2U) THEN
        S2=9999.
        U(2)=S0D
        D(3)=S0U
      ELSE IF(S0U.LT.SHD) THEN
        U(3)=S0D
        D(4)=S0U
        U(4)=SHD
      ELSE
        S0=9999.
      END IF
      SM=0.
      DO 700 K=1,4
        SD=U(K)-D(K)
        IF(SD.GE.SM) THEN
          KK=K
          SM=SD
        END IF
  700 CONTINUE
      ST=D(KK)+Q*(U(KK)-D(KK))
      END
