      SUBROUTINE  VTCLAS(ITK,IUORW,NCOMB)
C-------------------------------------------------------------
C!Do the track VDET cluster association in the U or W plan.
CKEY VDET TRACK
C
C  Author      : B. Mours   901001
C  modified by : B. Mours   910918
C    add hit error to the extrapolation error matrix
C    use new matrix invertor
C    cut on residual difference for overlap
C  modified by : B. Mours   911023
C    recompute error matrix when we have missing cluster
C
C  This routine may return several possible combinaisons
C  including those with missing clusters.
C
C  ITK  (in)  Track number
C  IUORW(in)  = 0 if in xy plan; 1 if in z
C  NCOMB(out) number of returned combinaisons
C-------------------------------------------------------------
      SAVE
C
      REAL    R(4),W(10),V(10),WEIG(16),CHI2,SU(4),SW(4),ACOR(0:1,4)
     +        ,RES(4),TANL
      INTEGER IL(4),NCL(4),ISCUT(4),IL1,IL2,IL3,IL4,I,NCLST,
     +        NVTUC,NVTWC,KVTXT,KVTER,KVTXC,KVTSC,JVTUWR,JVTSC,
     +        LAYN(4),KFRFT,PTR(4)
      LOGICAL FIRST,ERRMF(16)
      DOUBLE PRECISION DWEIG(4,4),DWORK(5,5),ERRM(4,4,16)
      INTEGER JVTXWI,JVTXHF,JVTXUC,JVTXWC,JVTXSU,JVTXSW,
     +          JVTXUW,JVTXXC,JVTXYC,JVTXZC,JVTXPV,
     +          JVTXPU,JVTXPW,JVTXUR,JVTXUT,JVTXUP,
     +          JVTXUD,JVTXUZ,JVTXWR,JVTXWT,JVTXWP,
     +          JVTXWD,JVTXWZ,LVTXTA
      PARAMETER(JVTXWI=1,JVTXHF=2,JVTXUC=3,JVTXWC=4,JVTXSU=5,JVTXSW=6,
     +          JVTXUW=7,JVTXXC=8,JVTXYC=9,JVTXZC=10,JVTXPV=11,
     +          JVTXPU=12,JVTXPW=13,JVTXUR=14,JVTXUT=15,JVTXUP=16,
     +          JVTXUD=17,JVTXUZ=18,JVTXWR=19,JVTXWT=20,JVTXWP=21,
     +          JVTXWD=22,JVTXWZ=23,LVTXTA=23)
      COMMON /VTKREC/  NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL
     +                ,NARCVD
      INTEGER NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL,NARCVD
      COMMON /VTRPAR/ MAXCLS,MAXCOM,IVFRFT,C2PRCL,SEACUT,CI2CUT,
     +                BIGERR,PULMIN,USNOIS,WSNOIS,HBIGER,NLYRMX
     +    ,           ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
      INTEGER MAXCOM,MAXCLS,IVFRFT,NLYRMX
      REAL C2PRCL,SEACUT,CI2CUT,BIGERR,PULMIN,HBIGER
      REAL ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
      PARAMETER(JVTSC2=1,JVTSCI=2,LVTSCA=5)
      PARAMETER(JVTUWI=1,JVTUCI=5,JVTUUC=9,JVTUSU=13,JVTURC=17,
     +          JVTUPH=21,JVTURE=25,LVTUCA=28)
      PARAMETER(JVTWWI=1,JVTWCI=5,JVTWWC=9,JVTWSW=13,JVTWZC=17,
     +          JVTWRE=21,LVTWCA=24)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
C!    local common to store work bank indices
      INTEGER KWSRT,KVTUC,KVTWC,KVTS0,KVTS1
      COMMON /VTBOS/ KWSRT, KVTUC, KVTWC, KVTS0, KVTS1
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DATA IL/1,2,3,4/,ISCUT/1,3,6,10/
      DATA FIRST/.TRUE./
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
      NCOMB = 0
      KVTXT = NLINK('VTXT',ITK)
      KVTER = NLINK('VTER',ITK)
      IF(KVTXT.EQ.0 .OR. KVTER.EQ.0) GO TO 999
      KFRFT = NLINK('FRFT',0)
      TANL  = RTABL(KFRFT,ITK,JFRFTL)
      NLAYER = LROWS(KVTXT)
C
      DO 10 I=1,16
        ERRMF(I) = .FALSE.
  10  CONTINUE
C
C-- position error
C
      DO 19 I=1,4
         SU(I) = 0.
         SW(I) = 0.
 19   CONTINUE
      DO 20 I=1,NLAYER
         JVTXT = KROW(KVTXT,I)
         CALL VDHTER(IW(JVTXT+JVTXWI),RW(JVTXT+JVTXPV),
     +               RW(JVTXT+JVTXPU),RW(JVTXT+JVTXPW),
     +               RW(JVTXT+JVTXUC),RW(JVTXT+JVTXWC),
     +               USNOIS,WSNOIS,SU(I),SW(I))
         LAYN(I) = IW(JVTXT+JVTXWI)/10000
         PT   = SQRT(RW(JVTXT+JVTXPU)**2+RW(JVTXT+JVTXPV)**2)
         PTOT = SQRT(RW(JVTXT+JVTXPW)**2+PT**2)
         ACOR(0,I) = RW(JVTXT+JVTXPV)/PT
         ACOR(1,I) = PT/PTOT
  20  CONTINUE
C
C-- compute weight matrix: invert of the error matrix in U plane
C
      IF(IUORW.EQ.0) THEN
        W(1) = RW(KVTER+3) + SU(1)
        W(2) = RW(KVTER+6)
        W(3) = RW(KVTER+8) + SU(2)
        W(4) = RW(KVTER+13)
        W(5) = RW(KVTER+15)
        W(6) = RW(KVTER+17) + SU(3)
        W(7) = RW(KVTER+24)
        W(8) = RW(KVTER+26)
        W(9) = RW(KVTER+28)
        W(10)= RW(KVTER+30) + SU(4)
        JVTUWR = JVTURE
      ELSE
        W(1) = RW(KVTER+5) + SW(1)
        W(2) = RW(KVTER+10)
        W(3) = RW(KVTER+12) + SW(2)
        W(4) = RW(KVTER+19)
        W(5) = RW(KVTER+21)
        W(6) = RW(KVTER+23) + SW(3)
        W(7) = RW(KVTER+32)
        W(8) = RW(KVTER+34)
        W(9) = RW(KVTER+36)
        W(10)= RW(KVTER+38) + SW(4)
        JVTUWR = JVTWRE
      ENDIF
C
      CALL TRUPCK(W,WEIG,NLAYER)
C
C-- loop over all layers with track extrapolation and prepare info
C
      NCL(2) = 1
      NCL(3) = 1
      NCL(4) = 1
C
      DO 50 I=1,NLAYER
        CALL VTCLLD(ITK,I,SEACUT*SQRT(W(ISCUT(I))),IUORW,NCL(I))
  50  CONTINUE
C
C Ser local work bank indices depending on the view
C
      IF(IUORW.EQ.0) THEN
        KVTXC = KVTUC
        KVTSC = KVTS0
      ELSE
        KVTXC = KVTWC
        KVTSC = KVTS1
      ENDIF
C
C-- here we have 4 layers
C
      DO 500 IL4 = 1,NCL(4)
        R(4) = RW(KROW(KVTXC,IL4)+JVTUWR+3)
        IL(4) = IL4
C
C-- here we have 3 layers
C
        DO 500 IL3 = 1,NCL(3)
          R(3) = RW(KROW(KVTXC,IL3)+JVTUWR+2)
          IL(3) = IL3
C
C-- here we have 2 layers
C
          DO 500 IL2 = 1,NCL(2)
            R(2) = RW(KROW(KVTXC,IL2)+JVTUWR+1)
            IL(2) = IL2
C
C-- here we have 1 layer
C
            DO 500 IL1 = 1,NCL(1)
              R(1) = RW(KROW(KVTXC,IL1)+JVTUWR)
              IL(1) = IL1
C
C-- cut on residual difference for overlaping wafers
C
              IF(NLAYER.GE.3) THEN
                DO 370 J=2,NLAYER
                  IF(LAYN(J-1).NE.LAYN(J)) GO TO 370
                  IF(R(J).EQ.0.)           GO TO 370
                  IF(R(J-1).EQ.0.)         GO TO 370
                  DRES = R(J)*ACOR(IUORW,J) -
     +                   R(J-1)*ACOR(IUORW,J-1)
                  IF(IUORW.EQ.0) THEN
                    IF(LAYN(J).EQ.0) THEN
                      DRESC = DRESIU
                    ELSE
                      DRESC = DRESOU
                    ENDIF
                  ELSE
                    DRESC = DRESOW + DRESLW * ABS(TANL)
                  ENDIF
                  IF(ABS(DRES).GT.DRESC) GO TO 500
  370           CONTINUE
              ENDIF
C
C-- find the inverse error matrix needed
C
              INDEX = 0
              JM = 0
              DO 380 I=1,NLAYER
                INDEX = INDEX*2
                IF (R(I).NE.0) THEN
                  INDEX = INDEX+1
                  JM = JM+1
                  PTR(JM) = I
                ENDIF
 380          CONTINUE
C
C-- compute inverse error matrix if needed
C
              IF(INDEX.NE.0) THEN
               IF(.NOT.ERRMF(INDEX)) THEN
                DO 385 I=1,JM
                  DO 385 J=1,JM
                    DWEIG(I,J) = WEIG((PTR(I)-1)*NLAYER+PTR(J))
 385            CONTINUE
                CALL DINV(JM,DWEIG,4,DWORK,IFAIL)
                DO 390 I=1,JM
                  DO 390 J=1,JM
                    ERRM(I,J,INDEX) = DWEIG(I,J)
 390            CONTINUE
                IF (IFAIL.NE.0) GO TO 999
                ERRMF(INDEX) = .TRUE.
               ENDIF
              ENDIF
C
C-- compute chisquare
C
              NCLST = 0
              CHI2  = 0.
              DO 400 J=1,JM
                IF(R(PTR(J)).NE.0.) NCLST = NCLST+1
                DO 400 K=1,JM
                  CHI2=CHI2+R(PTR(J))*R(PTR(K))*ERRM(J,K,INDEX)
  400         CONTINUE
              IF(CHI2.GT.NCLST*CI2CUT) GO TO 500
C
C-- find a free row in VTSC. If the bank is full we look for the
C-- worst stroed combinaison and we replace it by the current one
C-- if it is better. Combinaisons are order by number of cluster
C-- and for those with same number of cluster by chisquare.
C
            NCCUR = 0
            DO 420 J=1,NLAYER
              IF(IL(J).NE.NCL(J)) NCCUR = NCCUR+1
  420       CONTINUE
            IF(NCOMB.LT.MAXCOM) THEN
              NCOMB = NCOMB+1
              IW(KVTSC+LMHROW) = NCOMB
              IWRST = NCOMB
            ELSE
C
              IWRST = 0
              NWRST = NCLST
              CWRST = CHI2
              DO 440 ICOMB = 1,MAXCOM
                NC = 0
                JVTSC = KROW(KVTSC,ICOMB)
                DO 430 J=1,NLAYER
                  IF(IW(JVTSC+JVTSCI+J-1).NE.NCL(J)) NC = NC+1
  430           CONTINUE
                IF(NC.LT.NWRST) THEN
                  IWRST = ICOMB
                  NWRST = NC
                  CWRST = RW(JVTSC+JVTSC2)
                ELSE IF(NC.EQ.NWRST) THEN
                  IF(RW(JVTSC+JVTSC2).GT. CWRST) THEN
                    IWRST = ICOMB
                    CWRST = RW(JVTSC+JVTSC2)
                  ENDIF
                ENDIF
  440         CONTINUE
            ENDIF
C
            IF(IWRST.EQ.0) GO TO 999
            JVTSC = KROW(KVTSC,IWRST)
C
C-- store current combinaisons
C
            DO 450 J=1,NLAYER
              IW(JVTSC+JVTSCI+J-1) = IL(J)
  450       CONTINUE
            RW(JVTSC+JVTSC2) = CHI2
C
  500       CONTINUE
C
  999 RETURN
      END