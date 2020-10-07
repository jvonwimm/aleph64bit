*DK DCD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCD
CH
      SUBROUTINE DCD
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
C INVOKED BY TANSW.EQ.'CD'  (DCD)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_UIS.INC'
      LOGICAL CHG
      CHARACTER *51 T,TEX,TEX1
      CHARACTER *12 T12
      DATA TEX/'START_DSP'/
      DATA LEX/9/
      CHARACTER *2 DT2,TANSW
      PARAMETER (MCDEF=7)
      CHARACTER *2 TCDEF(MCDEF)
      DATA TCDEF/'CO','MA','UN','TO',  'FR','FG','FB'/
      DIMENSION PCDEF(4,MCDEF)
      DATA LCDEF/4/
      DATA PCDEF/
     &   0., 0.,127.,0.,
     &   0., 8.,127.,0.,
     &  10.,40.,999.,0.,
     &   0., 0.,  9.,0.,
     &   0., 0.,1.,0.,   0., 0.,1.,0.,   0., 0.,1.,0./
      CHARACTER *2 TCOL(0:127)
      DATA TCOL/'C0','C1','C2','C3','C4','C5','C6','C7',
     &          'CW','CG','CY','CB','CR','CM','CL','CB',
     &        112*' '/
      CHARACTER *9 TCOM(128)
C               USED FOR                 ! DEC COLOR NUMBER
      DATA TCOM/'DAL backg',             ! 1
     &     6*   '         ',             ! 2-7
     &          'Deb forgr',             ! 8
     &     4*   '         ',             ! 9-12
     &          'I/O backg',             ! 13
     &          'I/O forgr',             ! 14
     &          'Dsp backg',             ! 15
     &          '         ',             ! 16
     &    112*  '         '/             ! 17-128
      DATA C/0.0001/
      LOGICAL FYES
      DIMENSION NCTAB(0:127)
C     DATA DS/8./
C     CHARACTER *1 TLET(0:7)
C     DATA TLET/'A','B','C','D','E','F','G','H'/
C     CHARACTER *2 TNUM(0:127)
C     DATA TNUM/' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9',
C    &          '10','11','12','13','14','15',112*'  '/
C      CHARACTER *2 TDET(8:21)
C      DATA TDET/
C     &  'SI','LU','VD','IT','TP','EC','MA',
C     &  'HC','M1','M2','GL','FR','TX','BG'/
      CHARACTER *15 TT
C     DATA DTXH/20./,DTXL/2./
      DATA L1/-1/,L2/-2/,LD/2/
C     DATA NSL/15/,DHBL/4./,DVSL/7./,L31/31/,L16/16/
C     DIMENSION RDCO(32),GRCO(32),BLCO(32)
C     DATA MASK0/0/,MASK1/31/,DRAN/0.1/
C     DIMENSION LTAB(0:15)
C     DATA LTAB/ 1,13, 2, 6, 3, 5, 7, 8,14, 4,11,10,16,12,15, 9/
      CHARACTER *2 TLM(8)
      DATA TLM/'L1','L2','L3','L4','L5','L6','L7','L8'/
      FRNL=FRNLDU
      FRNLDU=0.
      LARE=AREADU
      CALL DCOPTL
      DO 710 N=0,31
        LCD=33-LTABDD(N)
        DO NCD=1,32
          IF(LTABDD(NCD).EQ.LCD) THEN
            NCTAB(N)=NCD
            GO TO 710
          END IF
        END DO
  710 CONTINUE
  900 T12=TFILDC//'COL_'
      L1=-1
      L2=-2
      TAN1DH='  '
C        123456789 123456789 123456789 123456789 123456789
  930 T='CD:W?:  set up      colors     , no metafile  '
      IF(TEX(LEX-2:LEX).EQ.'DSP'.OR.
     &   TEX(LEX-2:LEX).EQ.'PRI') T(17:20)=TEX(LEX-2:LEX)
      IF(FPRIDT) T(34:35)='on'
      CALL DWR_HL_AR(T)
      IF(L1.EQ.0) THEN
        LCDEF=MCDEF
        NCOL=PCDEF(2,1)
        LCOL=LTABDD(NCOL)
        WRITE(TXTADW,1001)   NCOL,
     &    'FR=',RDCODD(NCOL),
     &    'FG=',GRCODD(NCOL),
     &    'FB=',BLCODD(NCOL),LCOL,TCOM(LCOL)
 1001   FORMAT('CO=',I3,':',3(1X,A,1X,F4.2),' (',I2,') ',A)
        CALL DWRC
      ELSE IF(L1.GT.0) THEN
        LCDEF=MCDEF-3
        CALL DCDTYP(TT,LT,L1,L2,LD)
      END IF
      CHG=.FALSE.
  936 NCOL=PCDEF(2,1)
      LCOL=NCOL
      PCDEF(2,MCDEF-2)=RDCODD(LCOL)
      PCDEF(2,MCDEF-1)=GRCODD(LCOL)
      PCDEF(2,MCDEF  )=BLCODD(LCOL)
      CALL DOPER(1,0,
     &  1,LCDEF,TCDEF,PCDEF,
     &  1,L2-L1+1,TDCODD(L1),PDCODD(1,L1),
     &  NEXEC,CHG,TANSW)
      DLINDD=PDCODD(2,LITRDD)
      NCOL=PCDEF(2,1)
      IF(NCOL.EQ.LCOL) THEN
        RDCODD(LCOL)=PCDEF(2,MCDEF-2)
        GRCODD(LCOL)=PCDEF(2,MCDEF-1)
        BLCODD(LCOL)=PCDEF(2,MCDEF  )
      END IF
      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DCOPFL
      CALL DSC0
      CALL DSCTR1
      FRNLDU=FRNL
      TAN1DH='  '
      RETURN
  920 CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 941
      IF(TANSW.EQ.'PC'.OR.TANSW.EQ.'DC') THEN
        CALL DCDPC(TANSW)
        TAN1DH='  '
        GO TO 936
      END IF
      IF(TANSW.EQ.'MF') THEN
        TAN1DH='  '
        IF(FPRIDT) THEN
          CALL DWRT(' Metafile mode (UIS) already selected')
        ELSE
          CALL DWRT('UIS used now! Go back with "NG". See Help menu.')
          FPRIDT=.TRUE.
          FUISDU=.TRUE.
          CALL DGCLWK
          CALL DQTIT(1)
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'NG') THEN
        TAN1DH='  '
        IF(FPRIDT) THEN
          FPRIDT=.FALSE.
          CALL DQCL(0)
          CALL DGCLWK
          CALL DQTIT(1)
        ELSE
          CALL DWRT('Normal graphic mode (UISDC) already selected.')
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'TA') THEN
         CALL DCDTWF
         TAN1DH='  '
         GO TO 936
      END IF
      IF(TANSW.EQ.'SW') THEN
        CALL DCDBW
        TAN1DH='  '
        GO TO 936
      END IF
      IF(TANSW.EQ.'CC') THEN
        CALL DW_SET_CO
        TAN1DH='LM'
        GO TO 930
      END IF
      IF(TANSW.EQ.'LM') TANSW='L1'
      DO L=1,8
        IF(TANSW.EQ.TLM(L)) THEN
          LM2=L*16-1
          LM1=LM2-15
          WRITE(TXTADW,1101) '#',(      N         ,N=LM1,LM2)
          CALL DWRC
          WRITE(TXTADW,1102) 'R',(DT2(RDCODD(N)+C),N=LM1,LM2)
          CALL DWRC
          WRITE(TXTADW,1102) 'G',(DT2(GRCODD(N)+C),N=LM1,LM2)
          CALL DWRC
          WRITE(TXTADW,1102) 'B',(DT2(BLCODD(N)+C),N=LM1,LM2)
          CALL DWRC
          WRITE(TXTADW,1101) 'C',(  NCTAB (N)     ,N=LM1,LM2)
          CALL DWRC
          WRITE(TXTADW,1101) 'D',(  LTABDD(N)     ,N=LM1,LM2)
          CALL DWRC
 1101     FORMAT(1X,A,16I3)
 1102     FORMAT(17(1X,A))
          L1=0
          L2=-1
          TAN1DH='LM'
          GO TO 930
        END IF
      END DO
      IF(TANSW(1:1).EQ.'B'.AND.LCDEF.GT.MCDEF-3) THEN
        DO K=ICBPDD,ICBADD
          IF(TANSW.EQ.TDCODD(K)) THEN
            NCOL=PDCODD(2,K)
            PCDEF(2,1)=NCOL
            CALL DOPERS(1,NCOL,0.)
            GO TO 930
          END IF
        END DO
      END IF
      IF(TANSW(1:1).EQ.'C'.AND.LCDEF.GT.MCDEF-3) THEN
        DO K=0,31
          IF(TANSW.EQ.TCOL(K)) THEN
            PCDEF(2,1)=K
            CALL DOPERS(1,K,0.)
            GO TO 930
          END IF
        END DO
      END IF
      IF(TANSW.EQ.'DA') THEN
        TT='Detector B.G.'
        LT=13
        L1=MXSADD
        L2=ICCNDD
        LD=-3
        TAN1DH='LD'
        GO TO 930
      END IF
      IF(TANSW.EQ.'LL') THEN
        TT='Lines'
        LT=5
        L1=KCBPDD
        L2=KCLGDD
        LD=2
        TAN1DH='LL'
        GO TO 930
      END IF
      IF(TANSW.EQ.'LS') THEN
        TT='Style,width'
        LT=11
        L1=MODEDD
        L2=ISTHDD
        LD=2
        TAN1DH='LS'
        GO TO 930
      END IF
      IF(TANSW.EQ.'LF') THEN
        TT='Frame,text'
        LT=10
        L1=ICFRDD
        L2=ICVXDD
        LD=-3
        TAN1DH='LF'
        GO TO 930
      END IF
      IF(TANSW.EQ.'SB') THEN
        TT='Shadow 3d'
        LT=9
        L1=KSDBDD
        L2=KSLBDD
        LD=-3
        TAN1DH='SB'
        GO TO 930
      END IF
      IF(TANSW.EQ.'SI') THEN
        TT='SICA'
        LT=4
        L1=LCSHDD
        L2=LCSDDD
        LD=2
        TAN1DH='LA'
        GO TO 930
      END IF
      IF(TANSW.EQ.'VD') THEN
        TT='V-DET'
        L1=LCVDDD
        L2=LCVDDD
        LD=2
        TAN1DH='LV'
        GO TO 930
      END IF
      IF(TANSW.EQ.'IT') THEN
        TT='ITC'
        LT=3
        L1=LCITDD
        L2=LCIUDD
        LD=2
        TAN1DH='LI'
        GO TO 930
      END IF
      IF(TANSW.EQ.'TR') THEN
        TT='Tracks'
        LT=6
        L1=LCNBDD
        L2=LCNIDD
        LD=2
        TAN1DH='LN'
        GO TO 930
      END IF
      IF(TANSW.EQ.'TP') THEN
        TT='TPC'
        LT=3
        L1=LCTPDD
        L2=LCTWDD
        LD=2
        TAN1DH='LP'
        GO TO 930
      END IF
      IF(TANSW.EQ.'LC') THEN
        TT='LCAL'
        LT=4
        L1=LCLGDD
        L2=LCLKDD(3)
        LD=2
        TAN1DH='LC'
        GO TO 930
      END IF
      IF(TANSW.EQ.'EC') THEN
        TT='ECAL'
        LT=4
        L1=LCEODD
        L2=LCEFDD
        LD=2
        TAN1DH='LE'
        GO TO 930
      END IF
      IF(TANSW.EQ.'MU') THEN
        TT='Mu-det'
        LT=6
        L1=LCMUDD(1)
        L2=LCMUDD(2)
        LD=2
        TAN1DH='LU'
        GO TO 930
      END IF
      IF(TANSW.EQ.'HC') THEN
        TT='HCAL'
        LT=4
        L1=LCHGDD
        L2=LCHTDD(4)
        LD=2
        TAN1DH='LH'
        GO TO 930
      END IF
      IF(TANSW.EQ.'LR') THEN
        TT='en/col relation'
        LT=13
        L1=NCOLDD(1)
        L2=NCOLDD(8)
        LD=2
        TAN1DH='LR'
        GO TO 930
      END IF
      IF(TANSW.EQ.'FR') THEN
        TT='Frames'
        LT=6
        L1=NFVHDD
        L2=NFTRDD
        LD=2
        TAN1DH='FR'
        GO TO 930
      END IF
      IF(TANSW.EQ.'LA') THEN
C                    123456789 1234
        CALL DCDTYP('DA:Detector B.G.',16,MXSADD   ,ICCNDD   ,-3)
        CALL DCDTYP('LL:Detect. lines',16,KCBPDD   ,KCLGDD   , 2)
        CALL DCDTYP('LS:Style,width  ',16,MODEDD   ,ISTHDD   , 2)
        CALL DCDTYP('LF:Frm,txt'      ,10,ICFRDD   ,ICVXDD   , 2)
        CALL DCDTYP('LR:E/c. Relation',16,NCOLDD(1),NCOLDD(8), 2)
        CALL DCDTYP('SICA'            , 4,LCSHDD   ,LCSDDD   , 2)
        CALL DCDTYP('VDET'            , 4,LCVDDD   ,LCVDDD   , 2)
        CALL DCDTYP('ITC '            , 4,LCITDD   ,LCIUDD   , 2)
        CALL DCDTYP('TRacks    '      ,16,LCNBDD   ,LCNIDD   , 2)
        CALL DCDTYP('TPC '            , 4,LCTPDD   ,LCTWDD   , 2)
        CALL DCDTYP('LCAL'            , 4,LCLGDD   ,LCLKDD(3), 2)
        CALL DCDTYP('ECAL'            , 4,LCEODD   ,LCEFDD   , 2)
        CALL DCDTYP('HCAL'            , 4,LCHGDD   ,LCHTDD(4), 2)
        CALL DCDTYP('MU-det    '      ,10,LCMUDD(1),LCMUDD(2), 2)
        CALL DCDTYP('Frames    '      ,10,NFVHDD   ,NFTRDD   ,-3)
        CALL DCDTYP('S3:shadow '      ,10,KSDBDD   ,KSLBDD   ,-3)
        L1=-1
        L2=-2
        TAN1DH='  '
        GO TO 936
      END IF
      IF(TANSW.EQ.'NA') THEN
        TAN1DH='  '
 1200   CALL DWRT('old='//T12//TEX(1:LEX)//' new='//T12//'?????')
        CALL DGETLN(TEX1,LEX1,LEN(TEX1))
        IF(LEX1.EQ.1) GO TO 936
        IF(LEX1.EQ.2) THEN
          TANSW=TEX1
          GO TO 920
        END IF
        IF(LEX1.GT.0) THEN
          LEX=LEX1
          TEX=TEX1
        END IF
        CALL DWRT('File = '//T12//TEX(1:LEX))
        GO TO 936
      END IF
      IF(TANSW.EQ.'RD') THEN
        TAN1DH='  '
        CALL DCDRD('ALL',TEX(1:LEX),LEX)
        CALL DW_SET_CO
        GO TO 930
      END IF
      IF(TANSW.EQ.'RC') THEN
        TAN1DH='  '
        CALL DCDRD('COL',TEX(1:LEX),LEX)
        CALL DW_SET_CO
        GO TO 930
      END IF
      IF(TANSW.EQ.'SD') THEN
        TAN1DH='  '
        CALL DWRT('Do you want to write '//T12//TEX(1:LEX)//' ? [N]')
        CALL DGETLN(TEX1,LEX1,LEN(TEX1))
        IF(LEX1.EQ.1.AND.TEX1.EQ.'Y') CALL DCDSD(TEX(1:LEX),LEX)
        GO TO 930
      END IF
      IF(TANSW.EQ.'DH') THEN
        IFULDB=1
        CALL DQTIT(IFULDB)
        TAN1DH='  '
        GO TO 936
      END IF
      IF(TANSW.EQ.'MS') THEN
        CALL DCDMS
        GO TO 936
      END IF
      IF(TANSW.EQ.'MR') THEN
        CALL DCDMR
        GO TO 936
      END IF
      IF(TANSW.EQ.'PR') THEN
        CALL DCDPR
        GO TO 930
      END IF
      IF(TANSW.EQ.'CD') GO TO 900
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CONTINUE
  941 CALL DSC0
      CALL DSCTR1
      NAR=IAREDO
      CALL DPCEAR(NAR)
      IAREDO=NAR
      GO TO 936
      END
*DK DCDBWT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDBWT
CH
      SUBROUTINE DCDBWT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:   Toggle Black and White
C!    SWITCH TO BLACK AND WHITE AND BACK AFTER <CR>
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *49 T
      IF(.NOT.FMONDT) THEN
        CALL DCDBW
        CALL DTYANS('Type <CR> to continue.','<',NANSW)
        CALL DCDBW
      END IF
      END
*DK DCDBW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDBW
CH
      SUBROUTINE DCDBW
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
      DIMENSION RDCO(0:31),GRCO(0:31),BLCO(0:31)
      IF(FMONDT) RETURN
      IF(FBLWDT) THEN
        FBLWDT=.FALSE.
        DO L=0,31
          RDCODD(L)=RDCO(L)
          GRCODD(L)=GRCO(L)
          BLCODD(L)=BLCO(L)
        END DO
      ELSE
        FBLWDT=.TRUE.
        LARE=AREADU
        IF(PDCODD(2,LCBWDD).EQ.0.) THEN
          COBG=1.
          COFG=0.
        ELSE
          COBG=0.
          COFG=1.
        END IF
        DO L=0,LARE
          RDCO(L)=RDCODD(L)
          GRCO(L)=GRCODD(L)
          BLCO(L)=BLCODD(L)
          RDCODD(L)=COBG
          GRCODD(L)=COBG
          BLCODD(L)=COBG
        END DO
        DO L=LARE+1,28
          RDCO(L)=RDCODD(L)
          GRCO(L)=GRCODD(L)
          BLCO(L)=BLCODD(L)
          RDCODD(L)=COFG
          GRCODD(L)=COFG
          BLCODD(L)=COFG
        END DO
        DO L=29,31
          RDCO(L)=RDCODD(L)
          GRCO(L)=GRCODD(L)
          BLCO(L)=BLCODD(L)
          RDCODD(L)=COBG
          GRCODD(L)=COBG
          BLCODD(L)=COBG
        END DO
      END IF
      CALL DW_SET_CO
      END
*DK DCDGRY
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDGRY
CH
      SUBROUTINE DCDGRY
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
      DIMENSION RD(0:15),GR(0:15),BL(0:15)
      LOGICAL FCOL
      DATA FCOL/.TRUE./
      DATA C0/0./,C1/1./
      IF(FCOL) THEN
        FCOL=.FALSE.
        DC=(C1-C0)/7.
        C=C0
        DO L=8,15
          RD(L)=RDCODD(L)
          GR(L)=GRCODD(L)
          BL(L)=BLCODD(L)
          RDCODD(L)=MIN(C,1.)
          GRCODD(L)=MIN(C,1.)
          BLCODD(L)=MIN(C,1.)
          C=C+DC
        END DO
        RD(0)=RDCODD(0)
        GR(0)=GRCODD(0)
        BL(0)=BLCODD(0)
        RDCODD(0)=1.
        GRCODD(0)=1.
        BLCODD(0)=1.
      ELSE
        FCOL=.TRUE.
        DO L=8,15
          RDCODD(L)=RD(L)
          GRCODD(L)=GR(L)
          BLCODD(L)=BL(L)
        END DO
        RDCODD(0)=RD(0)
        GRCODD(0)=GR(0)
        BLCODD(0)=BL(0)
      END IF
      CALL DW_SET_CO
      END
*DK DCDPC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDPC
CH
      SUBROUTINE DCDPC(TANSW)
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
      CHARACTER *2 TANSW
      DIMENSION NC(9),RD(9),GR(9),BL(9)
      DATA NC/ 0  , 8  , 9  , 10 , 11 , 12 , 13 , 14 , 15 /
      DATA RD/ 0. , 1. , 0. , 1. , 1. , 1. , 1. , 0. , 0. /
      DATA GR/ 1. , 1. , 1. , 1. , 0. , 0. , 0. , 1. , 1. /
      DATA BL/ 1. , 1. , 0. , 0. , 0. , 0. , 1. , 1. , 1. /
      IF     (TANSW.EQ.'PC'.OR.
     &        TANSW.EQ.'PP'.OR.
     &        TANSW.EQ.'XC') THEN
        CALL UCOPY(RDCPDD,RDCODD,128)
        CALL UCOPY(GRCPDD,GRCODD,128)
        CALL UCOPY(BLCPDD,BLCODD,128)
        IF(TANSW.EQ.'PP'.OR.
     &     TANSW.EQ.'XC') THEN
          DO K=1,9
            N=NC(K)
            RDCODD(N)=RD(K)
            GRCODD(N)=GR(K)
            BLCODD(N)=BL(K)
          END DO
          IF(TANSW.EQ.'XC') GRCODD(4)=1.
        END IF
      ELSE IF(TANSW.EQ.'DC') THEN
        CALL UCOPY(RDCDDD,RDCODD,128)
        CALL UCOPY(GRCDDD,GRCODD,128)
        CALL UCOPY(BLCDDD,BLCODD,128)
      END IF
      CALL DW_SET_CO
      FBLWDT=.FALSE.
      END
*DK DCDMS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDMS
CH
      SUBROUTINE DCDMS
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
      DIMENSION RDCO(0:127),GRCO(0:127),BLCO(0:127)
      LOGICAL FNST
      DATA FNST/.TRUE./
      FNST=.FALSE.
      CALL UCOPY(RDCODD,RDCO,128)
      CALL UCOPY(GRCODD,GRCO,128)
      CALL UCOPY(BLCODD,BLCO,128)
      CALL DWRT('Color mixing stored.')
      RETURN
      ENTRY DCDMR
      IF(FNST) THEN
        CALL DWRT('Nothing was stored yet.')
      ELSE
        CALL UCOPY(RDCO,RDCODD,128)
        CALL UCOPY(GRCO,GRCODD,128)
        CALL UCOPY(BLCO,BLCODD,128)
        CALL DW_SET_CO
      END IF
      END
*DK DCDPR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDPR
CH
      SUBROUTINE DCDPR
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
      DIMENSION RDCO(0:15),GRCO(0:15),BLCO(0:15)
      LOGICAL FNST
      DATA FNST/.TRUE./
      CHARACTER *1 T
      FNST=.FALSE.
      CALL UCOPY(RDCODD,RDCO,16)
      CALL UCOPY(GRCODD,GRCO,16)
      CALL UCOPY(BLCODD,BLCO,16)
      DO N=0,15
        RDCODD(N)=RDCO(N)
        GRCODD(N)=RDCO(N)
        BLCODD(N)=RDCO(N)
      END DO
      CALL DWRT('  RED')
      CALL DW_SET_CO
CX    READ(5,'(A1)') T
      CALL DGETLN(T,L1,LEN(T))
CX
      DO N=0,15
        RDCODD(N)=GRCO(N)
        GRCODD(N)=GRCO(N)
        BLCODD(N)=GRCO(N)
      END DO
      CALL DWRT('  GREEN')
      CALL DW_SET_CO
CX    READ(5,'(A1)') T
      CALL DGETLN(T,L1,LEN(T))
CX
      DO N=0,15
        GRCODD(N)=BLCO(N)
        RDCODD(N)=BLCO(N)
        BLCODD(N)=BLCO(N)
      END DO
      CALL DWRT('  BLUE')
      CALL DW_SET_CO
CX    READ(5,'(A1)') T
      CALL DGETLN(T,L1,LEN(T))
CX
      DO N=0,15
        RDCODD(N)=1.-RDCO(N)
        GRCODD(N)=1.-RDCO(N)
        BLCODD(N)=1.-RDCO(N)
      END DO
      CALL DWRT('  CYAN')
      CALL DW_SET_CO
CX    READ(5,'(A1)') T
      CALL DGETLN(T,L1,LEN(T))
CX
      DO N=0,15
        RDCODD(N)=1.-GRCO(N)
        GRCODD(N)=1.-GRCO(N)
        BLCODD(N)=1.-GRCO(N)
      END DO
      CALL DWRT('  MAGENTA')
      CALL DW_SET_CO
CX    READ(5,'(A1)') T
      CALL DGETLN(T,L1,LEN(T))
CX
      DO N=0,15
        GRCODD(N)=1.-BLCO(N)
        RDCODD(N)=1.-BLCO(N)
        BLCODD(N)=1.-BLCO(N)
      END DO
      CALL DWRT('  BLUE')
      CALL DW_SET_CO
CX    READ(5,'(A1)') T
      CALL DGETLN(T,L1,LEN(T))
CX
      CALL UCOPY(RDCO,RDCODD,16)
      CALL UCOPY(GRCO,GRCODD,16)
      CALL UCOPY(BLCO,BLCODD,16)
      CALL DW_SET_CO
      END
*DK DCDST0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDST0
CH
      SUBROUTINE DCDST0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
      DIMENSION RDCO(0:31),GRCO(0:31),BLCO(0:31)
      DO L=0,31
        RDCO(L)=RDCODD(L)
        GRCO(L)=GRCODD(L)
        BLCO(L)=BLCODD(L)
      END DO
      RETURN
CH...........--
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCDSTC
CH
      ENTRY DCDSTC
CH
CH --------------------------------------------------------------------
CH
      DO L=0,31
        RDCODD(L)=RDCO(L)
        GRCODD(L)=GRCO(L)
        BLCODD(L)=BLCO(L)
      END DO
      CALL DW_SET_CO
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDRD
CH
      SUBROUTINE DCDRD(TIN,TEX,L)
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
      CHARACTER *(*) TEX
      CHARACTER *1 T1
      CHARACTER *3 TIN
      CHARACTER *30 TXT
C  The following define white/black to match ICOL+16, i.e., they are
C  activated by writing 1 in the 5th bitplane.
C
C  The following define white/black when used as inversed colours in
C  32 colour mode.
CBN   INTEGER ID32(0:31)/16*0,3*1,8,4*1,4*8,1,3*8/
      DIMENSION ICOL(112:127)
      DATA ICOL/3*1,8,4*1,4*8,1,3*8/
      IF(TIN.EQ.'PRI'.AND.TEX(L-2:L).EQ.'DSP') TEX(L-2:L)='PRI'
      IF(TIN.EQ.'DSP'.AND.TEX(L-2:L).EQ.'PRI') TEX(L-2:L)='DSP'
      IF(TIN.NE.'STR') THEN
        TXTADW='Do you want to read '
     &    //TFILDC//'COL_'//TEX(1:L)//' ? [N]'
        CALL DWRC
        CALL DGETLN(T1,L1,LEN(T1))
        CALL CLTOU(T1)
        IF(L1.NE.1.AND.T1.NE.'Y') RETURN
      END IF
      TXT=TFILDC//'COL_'//TEX(1:L)
      CALL DGOPEN(NUNIDU,TXT,2,*99,ISTAT)
C
C  First word contains # of colours, otherwise it is a 31 # of colour file
C
   10 READ(NUNIDU) COLNU
      IF(COLNU.LE.1.) THEN
C       ....................................................... 31 COLOUR FILE
        REWIND NUNIDU
        READ(NUNIDU) (RDCODD(N),GRCODD(N),BLCODD(N),N=0,15)
        DO N=112,127
          RDCODD(N)=RDCODD(ICOL(N))
          GRCODD(N)=GRCODD(ICOL(N))
          BLCODD(N)=BLCODD(ICOL(N))
        END DO
      ELSE
        M=COLNU
        READ(NUNIDU) (RDCODD(N),GRCODD(N),BLCODD(N),N=0,M)
      END IF
   12 IF(TIN.EQ.'ALL'.OR.TIN.EQ.'STR') THEN
        READ(NUNIDU) M
        M=MIN(M,MPNPDD)
        READ(NUNIDU)((PDCODD(I,N),I=1,4),N=1,M)
        IF(FMONDT) THEN
          PDCODD(2,ISTYDD)=0.
          PDCODD(2,ISTHDD)=0.
        END IF
      END IF
      CLOSE(UNIT=NUNIDU)
      DLINDD=PDCODD(2,LITRDD)
      CALL DSCTR1
      RDBGDD=RDCODD(0)
      GRBGDD=GRCODD(0)
      BLBGDD=BLCODD(0)
      IF(PDCODD(2,ISTYDD).LT.2) THEN
        RDCODD(0)=RDCODD(1)
        GRCODD(0)=GRCODD(1)
        BLCODD(0)=BLCODD(1)
      END IF
      CALL DWRT(' ')
      RETURN
   99 TXTADW=TFILDC//'COL_'//TEX(1:L)//' not found.'
      CALL DWRC
      END
*DK DCDSD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDSD
CH
      SUBROUTINE DCDSD(TEX,L)
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
      CHARACTER *(*) TEX
      DATA COLNU/127./
      TXTADW=TFILDC//'COL_'//TEX(1:L)
      OPEN(UNIT=NUNIDU,FILE=TXTADW,
     &  FORM='UNFORMATTED',STATUS='NEW')
      WRITE(NUNIDU) COLNU
      WRITE(NUNIDU) (RDCODD(N),GRCODD(N),BLCODD(N),N=0,127)
      WRITE(NUNIDU) MPNPDD
      WRITE(NUNIDU)((PDCODD(I,N),I=1,4),N=1,MPNPDD)
      CLOSE(UNIT=NUNIDU)
      TXTADW=TFILDC//'COL_'//TEX(1:L)//' saved.'
      CALL DWRC
      END
*DK DCDTYP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDTYP
CH
      SUBROUTINE DCDTYP(TT,LT,L1,L2,LDIG)
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
      CHARACTER *(*) TT
      CHARACTER *1 TSL(-1:1)
      DATA TSL/'/','=',':'/
      CHARACTER *2 DT2
      CHARACTER *3 DT3
      CHARACTER *49 T
      T=TT(1:LT)//':'
      L3=LT+3
      N=L3
      DO L=L1,L2
        IF(TDCODD(L).NE.'..') THEN
          T(N  :N+1)=TDCODD(L)
          T(N+2:N+2)=TSL(IFIX(PDCODD(4,L)))
C          IF(LDIG.LT.0) THEN
C            T(N+2:N+2)=TSL(IFIX(PDCODD(4,L)))
C          ELSE
C            T(N+2:N+2)='='
C          END IF
          IF(ABS(LDIG).EQ.2) THEN
            T(N+3:N+4)=DT2(PDCODD(2,L))
            N=N+6
          ELSE
            T(N+3:N+5)=DT3(PDCODD(2,L))
            N=N+7
          END IF
          IF(N.GT.43) THEN
            CALL DWRT(T)
            T=' '
            N=L3
          END IF
        END IF
      END DO
      IF(N.GT.L3) CALL DWRT(T)
      END
*DK DCDTWF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCDTWF
CH
      SUBROUTINE DCDTWF
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: Toggle between wire frame and areas
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(PDCODD(2,ISTYDD).GE.2.) THEN
        PDCODD(2,ISTYDD)=0.
        RDCO0=RDCODD(0)
        GRCO0=GRCODD(0)
        BLCO0=BLCODD(0)
        RDCODD(0)=RDCODD(1)
        GRCODD(0)=GRCODD(1)
        BLCODD(0)=BLCODD(1)
        CALL DWRT('Picture set to wire frame.')
      ELSE
        IF(FPRIDT) CALL DWRT(
     &    'Dont use area colouring for black/white metafiles!')
        PDCODD(2,ISTYDD)=2.
        RDCODD(0)=RDCO0
        GRCODD(0)=GRCO0
        BLCODD(0)=BLCO0
        CALL DWRT('Picture set to area colouring.')
      END IF
      CALL DW_SET_CO
      CALL DDISPA
      END
*DK DCFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCFT
CH
      SUBROUTINE DCFT(INFL,NTVIN,TI,NCOM,TCOM,TA,TIN,NZIN,TOUT,NZOUT,
     &  IRUN,IEVT)
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
      CHARACTER *2 TCOM(1),TIN,TOUT,TI,TA
      MORDDC=0
      VRTZDV=0.
      NTVIDT=NTVIN
      CALL DSETUP
      IFULDB=0
      CALL DQTIT(0)
1     CALL DBR1
      END
*DK DCHFIL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCHFIL
CH
      SUBROUTINE DCHFIL(AIN,AVEC,N)
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
      CHARACTER *(*) AIN,AVEC(1)
      DO 700 K=1,N
         AVEC(K)=AIN
  700 CONTINUE
      RETURN
      END
*DK DCICI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCICI
CH
      SUBROUTINE DCICI(XC,YC,RC,CH,F0,RDET,FMIN)
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
      DATA PIDEG /57.2957/
      IF(RC.GT.0.) THEN
         XY2=SQRT(XC**2+YC**2)
         R2=RC**2
         Q2R=0.5/RC
         QXY2=1./XY2
         BT=ATAN2(YC,XC)
      END IF
      TB2=RDET**2
      FMIN=180.
      AT=Q2R*(QXY2*(R2-TB2)+XY2)
      IF(ABS(AT).LE.1.) THEN
         D=ACOS(AT)
         F1=(BT+D)*PIDEG
         F2=(BT-D)*PIDEG
         FF1=MOD(CH*(F0-F1)+3600.,360.)
         FF2=MOD(CH*(F0-F2)+3600.,360.)
         FMIN=MIN(FMIN,FF1,FF2)
      END IF
      END
*DK DCIR
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DCIR
CH
      FUNCTION DCIR(FI,NV)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:DCIR = cordinate NV  of helix at position FI
C    Inputs    :FI is the helix parameter = 0 at start
C    Outputs   :NV defines the coordinate
C               1=x,2=y,3=z,4=rho,5=phi,6=theta,7=r,8=b
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA PIDEG /57.2957/
      DATA RTMIN/39.1/,RIIT/12.8/
      DIMENSION TP(5)
      DIMENSION TBX(13),TBY(13),DX(12),DY(12),A(12),Q(12),B(12)
      DIMENSION FIECA(2),FITPC(2)
      LOGICAL FBAR(2)
      IF(FI.NE.FIOLD) THEN
         X=XC-RC*COSD(F0-CH*FI)
         Y=YC-RC*SIND(F0-CH*FI)
         Z=Z0+BZ*FI
         FIOLD=FI
      END IF
      GO TO (1,2,3,4,5,6,7,8),NV
   99 DCIR=0.
      RETURN
    1 DCIR=X
      RETURN
    2 DCIR=Y
      RETURN
    3 DCIR=Z
      RETURN
    4 DCIR=SQRT(X**2+Y**2)
      RETURN
    5 DCIR=ATAN2(Y,X)*PIDEG
      IF(DCIR.LT.0.) DCIR=DCIR+360.
      RETURN
    6 RO=SQRT(X**2+Y**2)
      DCIR=ATAN2(RO,Z-VRTZDV)*PIDEG
      RETURN
    7 DCIR=SQRT(X**2+Y**2+Z**2)
      RETURN
    8 ZA=ABS(Z)
      RO=X**2+Y**2
      D=SQRT(RO+Z**2)
      RO=SQRT(RO)
      IF(RO*ZMAXDK.GT.ZA*RMAXDK) THEN
         DCIR=D*(RMAXDK/RO-1.)
      ELSE
         DCIR=D*(ZMAXDK/ZA-1.)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCIR0
CH
      ENTRY DCIR0(TP,FITC,FITPC,FIECA,FBAR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:set helix parameters for DCIR from vector TP (as defined in bank TGFT
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      DCIR0=0
      RC=1./ABS(TP(1))
      CH=SIGN(1.,TP(1))
      RO=RC-TP(4)
      FC=TP(3)-CH*90./PIDEG
      XC=RO*COS(FC)
      YC=RO*SIN(FC)
      F0=PIDEG*FC
      Z0=TP(5)
      BZ=RC*TP(2)/PIDEG
      IF(ISTART.EQ.0) THEN
         ISTART=1
C          CALL DGTTPC(TTPIR,TTPOR,TTPOZ)
         CALL DGTECA(TBX,TBY,QTR,TECIZ,TECOZ)
         TBX(13)=TBX(1)
         TBY(13)=TBY(1)
         DO 700 K=1,12
            DX(K)=TBX(K+1)-TBX(K)
            DY(K)=TBY(K+1)-TBY(K)
            A(K) =TBX(K)*TBY(K+1)-TBY(K)*TBX(K+1)
            Q(K) =1./SQRT(DX(K)**2+DY(K)**2)
            B(K) =ATAN2(DY(K),DX(K))
  700    CONTINUE
      END IF
      CALL DCICI(XC,YC,RC,CH,F0,RIIT,  FITC    )
      CALL DCICI(0.,0.,0.,CH,F0,RTMIN, FITPC(1))
      CALL DCICI(0.,0.,0.,CH,F0,RMAXDK,FITPC(2))
      IF(FITC.EQ.180.AND.TP(4).NE.0.) FITC=0.
      IF(FITPC(1).EQ.180..AND.TP(4).NE.0.) FITPC(1)=0.
      IF(BZ.NE.0.) THEN
         FZ=(ZMAXDK-Z0)/BZ
         IF(FZ.LT.0.) THEN
            FZ=(-ZMAXDK-Z0)/BZ
         END IF
         FITPC(2)=MIN(FITPC(2),FZ)
      END IF
      QT=1.
      ZECA=TECIZ
      DO 710 L=1,2
         FMIN=180.
         DO 720 K=1,12
            C=Q(K)*(XC*DY(K)-YC*DX(K)-QT*A(K))/RC
            IF(ABS(C).LE.1.) THEN
               D=ASIN(C)
               F1=(B(K)-D)*PIDEG
               F2=(B(K)+D)*PIDEG-180.
               FF1=MOD(CH*(F0-F1)+3600.,360.)
               FF2=MOD(CH*(F0-F2)+3600.,360.)
               FMIN=MIN(FMIN,FF1,FF2)
            END IF
  720    CONTINUE
         FBAR(L)=.TRUE.
         IF(BZ.NE.0.) THEN
            FZ=(ZECA-Z0)/BZ
            IF(FZ.LT.0.) THEN
               FZ=(-ZECA-Z0)/BZ
            END IF
            IF(FZ.LE.FMIN) THEN
               FMIN=FZ
               FBAR(L)=.FALSE.
            END IF
         END IF
         FIECA(L)=FMIN
         QT=QTR
         ZECA=TECOZ
  710 CONTINUE
      FIOLD=-999.
      END
*DK DCLD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCLD
CH
      SUBROUTINE DCLD
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
      CALL DQCL(IAREDO)
      BOTV=DFWIDU(IZOMDO)
      DFWIDU(IZOMDO)=0.
C      IF(WISUDW.LE.0.) THEN
C        CALL DQFR(IAREDO)
C      ELSE
C        WISUDW=2.
C        CALL DQFR(IAREDO)
C        WISUDW=1.
C      END IF
      CALL DQFR(IAREDO)
      IF(IAREDO.EQ.0) THEN
         CALL DGCLWK
         CALL DQTIN
      END IF
      DFWIDU(IZOMDO)=BOTV
      CALL DPCSAR
      RETURN
      END
*DK DCONLO
C----------------------------------------------------------------------
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCONLO
CH
      SUBROUTINE DCONLO
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
C----------------------------------------------------------------------
C!
C!
C!   Purpose   : Convert DALI.LOG file's format into normal.
C!   Inputs    : DALI.LOG
C!   Outputs   : DALI.LOF
C!
C!   Called by :
C!   Calls     :
C!
C----------------------------------------------------------------------
      CHARACTER *79 T,T1
      OPEN(UNIT=87,FILE='DALI.LOG',STATUS='OLD',ERR=999)
      OPEN(UNIT=88,FILE='DALI.LOF',STATUS='NEW')
      DATA K/26/
      CALL DWRT_SETUP('LOGFILE=OFF')
    1 READ(87,1100,END=9) T1
 1100 FORMAT(A)
      IF((T1(1:1).NE.'$').AND.(T1(1:1).NE.' ')) THEN
        LNG=LENOCC(T1)
        IF(LNG.GT.1) THEN
          KK=K+LNG-1
          IF(KK.GE.79) THEN
            T=' '
            IF(LNG.EQ.2) THEN
              K=54
            ELSE
              K=2
            END IF
          END IF
          T(K:)=T1(2:LNG)
          K=K+LNG-1
        END IF
        GO TO 1
      ELSE
        CALL DWR_TO_FILE(88,T)
        T1(1:1)=' '
        T=T1
        K=LENOCC(T)+1
   20 CONTINUE
      GO TO 1
      END IF
    9 CALL DWR_TO_FILE(88,T(1:K-1))
      CALL DWRT('Output on DALI.LOF. Type error information')
      CALL DGETLN(T1,LL1,LEN(T1))
      IF(T1.NE.' ')THEN
        CALL DWR_TO_FILE(88,' ')
        CALL DWR_TO_FILE(88,
     &    '############ Operator comments! ###########')
        CALL DWR_TO_FILE(88,' ')
        CALL DWR_TO_FILE(88,T1)
      END IF
      CLOSE(UNIT=87)
      CLOSE(UNIT=88)
      GO TO 998
  999 CALL DWRT('File DALI.LOG not found !')
  998 CALL DWRT_SETUP('LOGFILE=ON')
      END
*DK DCOPFL
C----------------------------------------------------------------------
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DCOPFL
CH
      SUBROUTINE DCOPFL
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
C----------------------------------------------------------------------
C!
C!
C!   Purpose   : COPY FROM OR TO "LAST ARRAY" = PLSTDA ...
C!
C----------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CALL UCOPY(ILSTDO   ,IMAXDO,3)
      CALL UCOPY(ILSTDO(5),IPICDO,4)
      CALL DPACGF(1,PLSTDA)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCOPTL
CH
      ENTRY DCOPTL
CH
CH --------------------------------------------------------------------
CH
      CALL UCOPY(IMAXDO,ILSTDO   ,3)
      CALL UCOPY(IPICDO,ILSTDO(5),4)
      CALL DPACGT(1,PLSTDA)
      END
