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
*DK DCCTR
      LOGICAL FUNCTION DCCTR(DA1,DA2)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FCUT(2)
      DCCTR=.FALSE.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF((PARADA(4,J_PCF).EQ.1.AND.PARADA(2,J_PCF).GT.0.).OR.
     &   (PARADA(4,J_PCT).EQ.1.AND.PARADA(2,J_PCT).GT.0.)) THEN
         DA=DA1
         DO   700  J=1,2
            FCUT(J)=.FALSE.
            IF(PARADA(4,J_PCF).EQ.1.AND.PARADA(2,J_PCF).GE.0.)THEN
               FI=DFINXT(PARADA(2,J_PFI),DHELIX(DA,IVFIDV))
               D=0.5*PARADA(2,J_PCF)
               IF(FI.LT.PARADA(2,J_PFI)-D.OR.
     &           FI.GT.PARADA(2,J_PFI)+D) THEN
                  FCUT(J)=.TRUE.
                  GO TO 102
               END IF
            END IF
            IF(PARADA(4,J_PCT).EQ.1.AND.PARADA(2,J_PCT).GE.0.)THEN
               TE=DHELIX(DA,IVTEDV)
               D=0.5*PARADA(2,J_PCT)
               IF(TE.LT.PARADA(2,J_PTE)-D.OR.
     &           TE.GT.PARADA(2,J_PTE)+D) THEN
                  FCUT(J)=.TRUE.
                  GO TO 102
               END IF
            END IF
  102       DA=DA2
  700    CONTINUE
         IF(FCUT(1).AND.FCUT(2)) THEN
            DCCTR=.TRUE.
         END IF
      END IF
      END
*DK DCCVX
      LOGICAL FUNCTION DCCVX(XYZ)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PCT).EQ.1.AND.PARADA(2,J_PCT).GE.0.)THEN
         RO=SQRT(XYZ(1)**2+XYZ(2)**2)
         IF(XYZ(3).EQ.0..AND.RO.EQ.0.) GO TO 5
         TE=DATN2D(RO,XYZ(3))
         D=0.5*PARADA(2,J_PCT)
         IF(TE.LT.PARADA(2,J_PTE)-D.OR.TE.GT.PARADA(2,J_PTE)+D)
     &     GO TO 9
      END IF
      IF(PARADA(4,J_PCF).EQ.1.AND.PARADA(2,J_PCF).GE.0.)THEN
         IF(XYZ(1).EQ.0..AND.XYZ(2).EQ.0.) GO TO 5
         FI=DFINXT(PARADA(2,J_PFI),DATN2D(XYZ(2),XYZ(1)))
         D=0.5*PARADA(2,J_PCF)
         IF(FI.LT.PARADA(2,J_PFI)-D.OR.FI.GT.PARADA(2,J_PFI)+D)
     &     GO TO 9
      END IF
    5 DCCVX=.FALSE.
      RETURN
    9 DCCVX=.TRUE.
      END
*DK DCIRCL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCIRCL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:calculate vector TP(as defined in bank TGFT) from 3 points
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (BFIELD=15.0,CLGHT=29.9792458)
      DIMENSION RIN(3),FIN(3),ZIN(3),S2RF(3),S2ZZ(3)
      DATA PIDEG /57.2957/
      DATA NPT/3/
      CHARACTER *3 DT3,TC
      CHARACTER *4 DT4,T4,T5
      CHARACTER *5 TP
      Q=1./PIDEG
      DO   700  N=1,3
         RIN(N)=EVARDP(IVRODV,N)
         FIN(N)=EVARDP(IVFIDV,N)*Q
         ZIN(N)=EVARDP(IVZZDV,N)
  700 CONTINUE
      CALL DCIRHL(RIN,FIN,ZIN,S2RF,S2ZZ,NPT,TPHEDT,CHIZ)
      RHO = 1./ABS(TPHEDT(1))
      PT = BFIELD*RHO*CLGHT/100000.
      THETA = ATAN2(1.,TPHEDT(2))
      PTOT = PT/SIN(THETA)
      TC=DT3(CHIZ)
      T4=DT4(TPHEDT(4))
      T5=DT4(TPHEDT(5))
      CALL DTP(SIGN(1.,TPHEDT(1))*PTOT,4,TP)
      CALL DWRT(' 1c fit dz='//TC//' dist to 0: dr='//
     &  T4//' dz='//T5//' p='//TP)
      PZ=PTOT*COS(THETA)
      PX=PT  *COS(TPHEDT(3))
      PY=PT  *SIN(TPHEDT(3))
      CALL DWRT(
     &  ' px='//DT4(PX)//
     &  ' py='//DT4(PY)//
     &  ' pz='//DT4(PZ) )
      END
*DK DCIRHL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCIRHL(RIN,FIN,ZIN,S2RF,S2ZZ,NPT,VV0,CHIZ)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by N. CHERNOV, G. OSOSKOV & M. POPPE    28-JUL-1988
C    Modified for use of IOPT=0 only BY H.DREVERMANN
C!:Fast fitting routine for helixes
C    Inputs    :RIN  = vector of RADII            /CM
C               FIN  = VECTOR OF PHI VALUES
C               ZIN  = VECTOR OF Z VALUES         /CM
C              S2RF  = VECTOR OF SIGMA**2(R*PHI)  /CM**2
C              S2ZZ  = VECTOR OF SIGMA**2(Z)      /CM**2
C               NPT  = NUMBER OF POINTS TO BE FITTED
C               IOPT = 0 -> DISTANCE**2=X**2+Y**2 MINIMISED
C                      1 -> WEIGHTED WITH 1/SIMA(R*PHI)**2
C                      2 -> ERROR MATRIX CALCULATED
C                      3 -> 3-DIMENSIONAL ITERATION
C    Outputs   : VV0 = 1/R*CHARGE   [1/CM]  POS. IF CLOCKWISE
C                TAN(LAMBDA)  {=DZ/DS}TAN(ANGLE TO X,Y PLANE)
C                PHI0         {0,2PI} ANGLE TO X-AXIS
C                D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,
C                                     POS. IF AXIS ENCIRCLED
C                Z0           [CM]    Z POS AT R=D0
C                CHI2= CHI SQUARED = SUM (DEVIATIONS/ERRORS)**2
C    Note: DEGREES OF FREEDOM = 2*NPT-5
C          BASED ON  SUBROUTINE CIRCLE
C          REFERENCE:  COMPUTER PHYSICS COMMUNICATIONS VOL 33,P329
C
C    Called by :
C ---------------------------------------------------------------------
      PARAMETER (EPS = 1.0E-16, ITMAX =15, MPT=40)
      REAL   PF(MPT),RF(MPT),SP2(MPT),VV0(*),
     1  DEL(MPT),SXY(MPT),ZF(MPT),WZF(MPT),SS0(MPT),EEE(MPT),
     2  GRAD(5)
      DOUBLE PRECISION XF(MPT),YF(MPT),WF(MPT)
         DOUBLE PRECISION ALF,A0,A1,A2,A22,BET,CUR,
     1        DD,DEN,DET,DY,D2,F,FACT,FG,F1,G,GAM,GAM0,G1,
     2        H,H2,P2,Q2,RM,RN,ROOT,
     3        XA,XB,XD,XI,XM,XX,XY,X1,X2,DEN2,
     4        YA,YB,YD,YI,YM,YY,   Y1,Y2,SA2B2,DD0,PHIC
            REAL RIN(*),FIN(*),ZIN(*),S2RF(*),S2ZZ(*)
            LOGICAL FIRST
            DATA FIRST/.TRUE./
            IF(FIRST) THEN
               PI    = 2.0*ASIN(1.0)
               PIO2  = 0.5*PI
               PIT2  = 2.0*PI
               FIRST =.FALSE.
            END IF
            DO 2   I=1,5
               GRAD(I)=0.0
               VV0(I) =0.0
    2       CONTINUE
            CHI2=0.0
            IF(NPT.GT.MPT) RETURN
C-----> INPUT DATA
            DO 10 I=1,NPT
               XF(I)  = RIN(I)*COS(FIN(I))
               YF(I)  = RIN(I)*SIN(FIN(I))
               RF(I)  = RIN(I)
               PF(I)  = FIN(I)
               WF(I)  = (S2RF(I)+0.000000001)**(-1)
               SP2(I) = WF(I)*(RF(I)*RF(I))
               ZF(I)  = ZIN(I)
               WZF(I) = 1.0/(S2ZZ(I)+0.000001)
   10       CONTINUE
*
            WSUM= 0.0
            RSS = 0.0
            PRO = 0.0
            IF (NPT .LE. 2)  RETURN
            N = NPT
            XM = 0.
            YM = 0.
            DO 100 I= 1, N
               WZF(I)= 1.0
               WF(I) = 1.0
               XM    = XM + XF(I)
               YM    = YM + YF(I)
  100       CONTINUE
            RN = 1./FLOAT(N)
C **
            XM = XM * RN
            YM = YM * RN
            X2 = 0.
            Y2 = 0.
            XY = 0.
            XD = 0.
            YD = 0.
            D2 = 0.
            DO 102 I= 1, N
               XI = XF(I) - XM
               YI = YF(I) - YM
               XX = XI**2
               YY = YI**2
               X2 = X2 + XX*WF(I)
               Y2 = Y2 + YY*WF(I)
               XY = XY + XI*YI*WF(I)
               DD = XX + YY
               XD = XD + XI*DD*WF(I)
               YD = YD + YI*DD*WF(I)
               D2 = D2 + DD**2*WF(I)
  102       CONTINUE
C **
            X2 = X2*RN
            Y2 = Y2*RN
            XY = XY*RN
            D2 = D2*RN
            XD = XD*RN
            YD = YD*RN
            F = 3.D0*X2 + Y2
            G = 3.D0*Y2 + X2
            FG = F*G
            H = XY + XY
            H2 = H**2
            P2 = XD**2
            Q2 = YD**2
            GAM0 = X2 + Y2
            FACT = GAM0**2
            A2 = (FG-H2-D2)/FACT
            FACT = FACT*GAM0
            A1 = (D2*(F+G) - 2.D0*(P2+Q2))/FACT
            FACT = FACT*GAM0
            A0 = (D2*(H2-FG) + 2.D0*(P2*G + Q2*F) - 4.D0*XD*YD*H)/FACT
            A22 = A2 + A2
            YB = 1.0E30
            ITER = 0
            XA = 1.D0
C **                MAIN ITERATION
  103       YA = A0 + XA*(A1 + XA*(A2 + XA*(XA-4.D0)))
            IF (ITER .GE. ITMAX)                      GO TO 105
            DY = A1 + XA*(A22 + XA*(4.D0*XA - 12.D0))
            XB = XA - YA/DY
            IF (ABS(YA).GT.ABS(YB)) XB=0.5D0*(XB+XA)
            IF (ABS(XA-XB) .LT. EPS)                  GO TO 105
            XA = XB
            YB = YA
            ITER = ITER + 1
            GO TO 103
C **
  105    CONTINUE
         ROOT = XB
         GAM = GAM0*XB
         F1 = F - GAM
         G1 = G - GAM
         X1 = XD*G1 - YD*H
         Y1 = YD*F1 - XD*H
         DET = F1*G1 - H2
         DEN2= 1.D0/(X1**2 + Y1**2 + GAM*DET**2)
         IF(DEN2.LE.0.D0)                GO TO 999
         DEN = DSQRT(DEN2)
         CUR = DET*DEN                  + 0.0000000001D0
         ALF = -(XM*DET + X1)*DEN
         BET = -(YM*DET + Y1)*DEN
         RM = XM**2 + YM**2
         GAM = ((RM-GAM)*DET + 2.D0*(XM*X1 + YM*Y1))*DEN*0.5D0
C
C--------> CALCULATION OF STANDARD CIRCLE PARAMETERS
C          NB: CUR IS ALWAYS POSITIVE
         RR0=CUR
         ASYM = BET*XM-ALF*YM
         SST = 1.0
         IF(ASYM.LT.0.0) SST=-1.0
         RR0 = SST*CUR
         IF((ALF*ALF+BET*BET).LE.0.D0)              GO TO 999
         SA2B2 = 1.D0/DSQRT(ALF*ALF+BET*BET)
         DD0 = (1.D0-1.D0/SA2B2)/CUR
         PHIC = DASIN(ALF*SA2B2)+PIO2
         IF(BET.GT.0)    PHIC=PIT2-PHIC
         PH0 = PHIC+PIO2
         IF(RR0.LE.0)    PH0=PH0-PI
         IF(PH0.GT.PIT2) PH0=PH0-PIT2
         IF(PH0.LT.0.0)  PH0=PH0+PIT2
         VV0(1) = RR0
         VV0(3) = PH0
         VV0(4) = DD0
C
C-----> CALCULATE PHI DISTANCES TO MEASURED POINTS
C
         AA0 =SST
         OME =RR0
         GG0 = OME*DD0-AA0
         HH0 = 0.0000000001
         IF(ABS(GG0).LT.(1.0/HH0)) HH0=1.0/GG0
         HH0=1.0/GG0
         DO 300 I=1,N
            ASYM   = BET*XF(I)-ALF*YF(I)
            SS0(I) =1.0
            IF(ASYM.LT.0.0) SS0(I)=-1.0
            FF0= OME*(RF(I)*RF(I)-DD0*DD0)/(2.0*RF(I)*GG0)+DD0/RF(I)
C
            IF(FF0.LT.-1.0) FF0 = -1.0
            IF(FF0.GT.1.0)  FF0 = 1.0
C
            DEL(I)= PH0 + (SS0(I)-AA0)*PIO2 + SS0(I)*ASIN(FF0) - PF(I)
            IF(DEL(I).GT.PI) DEL(I)=DEL(I)-PIT2
            IF(DEL(I).LT.-PI)DEL(I)=DEL(I)+PIT2
  300    CONTINUE
C
C-----> FIT STRAIGHT LINE IN S-Z
C
         DO 350 I=1,N
            EEE(I) = 0.5*VV0(1)
     1        *SQRT(ABS( (RF(I)*RF(I)-VV0(4)*VV0(4))
     2        /(1.0-AA0*VV0(1)*VV0(4))     ) )
C
            IF(EEE(I).GT.1.0)  EEE(I)= 1.0
            IF(EEE(I).LT.-1.0) EEE(I)= -1.0
C
            SXY(I)=2.0*ASIN(EEE(I))/OME
  350    CONTINUE
         SUMS  = 0.0
         SUMSS = 0.0
         SUMZ  = 0.0
         SUMZZ = 0.0
         SUMSZ = 0.0
         SUMW  = 0.0
         DO 360 I=1,N
            SUMW  = SUMW  +                 WZF(I)
            SUMS  = SUMS  + SXY(I)        * WZF(I)
            SUMSS = SUMSS + SXY(I)*SXY(I) * WZF(I)
            SUMZ  = SUMZ  + ZF(I)         * WZF(I)
            SUMZZ = SUMZZ + ZF(I)*ZF(I)   * WZF(I)
            SUMSZ = SUMSZ + ZF(I)*SXY(I)  * WZF(I)
  360    CONTINUE
         DENOM = SUMW*SUMSS - SUMS*SUMS
         DZDS  = (SUMW*SUMSZ-SUMS*SUMZ) /DENOM
         ZZ0   = (SUMSS*SUMZ-SUMS*SUMSZ)/DENOM
         VV0(2)= DZDS
         VV0(5)= ZZ0
         CHIZ=0.
  999    DO 370 I=1,N
            DZ    = ZZ0+DZDS*SXY(I)-ZF(I)
            CHIZ=CHIZ+DZ**2
  370    CONTINUE
         CHIZ=SQRT(CHIZ)
      END
*DK DCTYCD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DCTYCD
CH
      SUBROUTINE DCTYCD
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
      CHARACTER *3 DT3
      CHARACTER *41 TCTF
      CHARACTER *124 TCUT
      CHARACTER *(*) THMID
      DATA PH1/-2./,PV1/-12./,PH2/14./,PV2/2./,PV3/3./
      DIMENSION NMAX(0:12)
      DATA NMAX/122,6*40,6*80/
      DIMENSION NPGR(2),NPTF(2)
      DATA N24/24/
      LOGICAL FDOWN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCA'
      CALL DPARAM(20
     &  ,J_HCA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MODTP.EQ.1) THEN
        MODTP=0
        IF(IHTRDO(5).EQ.2) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT-4:NCUT)=' U.C.'
        ELSE IF(IHTRDO(5).EQ.3) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' T.C.'
        ELSE IF(IHTRDO(5).EQ.4) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' F.C.'
        ELSE IF(IHTRDO(5).EQ.5) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' C.C.'
        END IF
        IF(FCUTDT.AND.PARADA(2,J_HCA).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' imp.'
          GO TO 1
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
CH --------------------------------------------------------------------  DCTYTR
CH
      ENTRY DCTYTR
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
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    1 TPARDA=
     &  'J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ'
      CALL DPARAM(20
     &  ,J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ)
      TPARDA=
     &  'J_HST,J_HCQ'
      CALL DPARAM(20
     &  ,J_HST,J_HCQ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MODTR.EQ.1) THEN
        MODTR=0
        IF(PARADA(4,J_HCM).EQ.1..AND.PARADA(2,J_HCM).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+6
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          IF(PARADA(2,J_HCM).GE.0) THEN
            CMO1=PARADA(2,J_HCM)
            TCUT(NCUT1:NCUT)=' P>'//DT3(CMO1)
          ELSE
            CMO2=-PARADA(2,J_HCM)
            TCUT(NCUT1:NCUT)=' P<'//DT3(CMO2)
          END IF
        END IF
        IF(PARADA(4,J_HZ0).EQ.1.AND.PARADA(2,J_HZ0).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          IF(PARADA(2,J_HZ0).LT.0.) THEN
            CDZ1=-PARADA(2,J_HZ0)
            TCUT(NCUT1:NCUT)=' Z0>'//DT3(CDZ1)
          ELSE
            CDZ2=PARADA(2,J_HZ0)
            TCUT(NCUT1:NCUT)=' Z0<'//DT3(CDZ2)
          END IF
        END IF
        IF(PARADA(4,J_HD0).EQ.1.AND.PARADA(2,J_HD0).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          IF(PARADA(2,J_HD0).LT.0.) THEN
            CDR1=-PARADA(2,J_HD0)
            TCUT(NCUT1:NCUT)=' D0>'//DT3(CDR1)
          ELSE
            CDR2=PARADA(2,J_HD0)
            TCUT(NCUT1:NCUT)=' D0<'//DT3(CDR2)
          END IF
        END IF
        IF(PARADA(4,J_HCQ).EQ.1.AND.PARADA(2,J_HCQ).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CCH=PARADA(2,J_HCQ)
          IF(CCH.GT.0.) THEN
            TCUT(NCUT1:NCUT)=' CH=+'
          ELSE
            TCUT(NCUT1:NCUT)=' CH=-'
          END IF
        END IF
        IF(PARADA(4,J_HNI).EQ.1.AND.PARADA(2,J_HNI).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CITC=PARADA(2,J_HNI)
          TCUT(NCUT1:NCUT)=' #i>'//DT3(CITC)
        END IF
        IF(PARADA(4,J_HNT).EQ.1.AND.PARADA(2,J_HNT).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' NT='//DT3(PARADA(2,J_HNT))
        END IF
        CT1=-99999.
        CT2= 99999.
        IF(PARADA(4,J_HT1).EQ.1.) CT1=PARADA(2,J_HT1)
        IF(PARADA(4,J_HT2).EQ.1.) CT2=PARADA(2,J_HT2)
        IF(PARADA(4,J_HZZ).EQ.1.AND.PARADA(2,J_HZZ).NE.0.) THEN
          IF(PARADA(2,J_HZZ).LT.0.) THEN
            CT1=MAX(90.,CT1)
          ELSE
            CT2=MIN(90.,CT2)
          END IF
        END IF
        IF     (CT1.NE.-99999.AND.CT2.NE.99999.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+13
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' '//DT3(CT1)//'<J-tr<'//DT3(CT2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1+4
        ELSE IF(CT1.NE.-99999.AND.CT2.EQ.99999.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+9
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' J-tr>'//DT3(CT1)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        ELSE IF(CT1.EQ.-99999.AND.CT2.NE.99999.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+11
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' J-tr<'//DT3(CT2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        END IF
        IF(PARADA(4,J_HF1).EQ.1..AND.PARADA(4,J_HF2).EQ.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+13
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CF1=PARADA(2,J_HF1)
          CF2=PARADA(2,J_HF2)
          TCUT(NCUT1:NCUT)=' '//DT3(CF1)//'<F_tr<'//DT3(CF2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1+4
C                         +5 IS WRONG: DGTXTG(TCUT(2: ...
        ELSE IF(PARADA(4,J_HF1).EQ.1..AND.PARADA(4,J_HF2).NE.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+9
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CF1=PARADA(2,J_HF1)
          TCUT(NCUT1:NCUT)=' F_tr>'//DT3(CF1)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        ELSE IF(PARADA(4,J_HF1).NE.1..AND.PARADA(4,J_HF2).EQ.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+9
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CF2=PARADA(2,J_HF2)
          TCUT(NCUT1:NCUT)=' F_tr<'//DT3(CF2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        END IF
        IF(IHTRDO(2).NE.3.AND.
     &    PARADA(4,J_HST).EQ.1.AND.PARADA(2,J_HST).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CST=PARADA(2,J_HST)
          IF(CST.GT.0.) THEN
            TCUT(NCUT1:NCUT)=' TR='//DT3(CST)
          ELSE
            TCUT(NCUT1:NCUT)=' TR#'//DT3(-CST)
          END IF
        END IF
        NUMT=BNUMDB(2,FRFTDB)
        IF(BNUMDB(4,FRFTDB).GT.0.) THEN
          DO 700 K=1,NUMT
            IF(COLRDT(K).LT.0.) THEN
              NCUT1=NCUT+1
              NCUT=NCUT+7
              TCUT(NCUT1:NCUT)='man.cut'
              GO TO 8
            END IF
  700     CONTINUE
        END IF
      END IF
    8 RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCTYBA
CH
      ENTRY DCTYBA
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(MODBA.EQ.1) THEN
        MODBA=0
        NCUT1=NCUT+1
        NCUT=NCUT+5
        IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
        TCUT(NCUT1:NCUT)=' B.C.'
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCUTBA
CH
      ENTRY DCTYPA
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
      IF(MODPA.EQ.1) THEN
        MODPA=0
        NCUT1=NCUT+1
        NCUT=NCUT+5
        IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
        TCUT(NCUT1:NCUT)=' Pads'
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCTYCA
CH
      ENTRY DCTYCA
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
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE,J_HCL'
      CALL DPARAM(20
     &  ,J_HCE,J_HCL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MODCA.EQ.1) THEN
        MODCA=0
        IF(PARADA(4,J_HCE).EQ.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' En>'//DT3(PARADA(2,J_HCE))
        END IF
        IF(PARADA(4,J_HCL).EQ.1.) THEN
          IF(PARADA(2,J_HCL).EQ.0.)THEN
            NCUT1=NCUT+1
            NCUT=NCUT+5
            IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
            TCUT(NCUT1:NCUT)=' CL#0'
          ELSE
            NCUT1=NCUT+1
            NCUT=NCUT+7
            IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
            TCUT(NCUT1:NCUT)=' CL='//DT3(PARADA(2,J_HCL))
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
CH --------------------------------------------------------------------  DCUTWR
CH
      ENTRY DCTYEX(THMID,NMID)
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
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      CALL DQLEVL(ICTXDD)
      H=HMINDG(IAREDO)
      V=VMINDG(IAREDO)
      FDOWN=.FALSE.
      IF(NCUT.NE.0) THEN
        IF(IAREDO.LT.9.OR.IAREDO.GT.11.OR.NCUT.LE.NMX) THEN
          FDOWN=.TRUE.
          CALL DGTXTG(H+PH2,V+PV2,TCUT(2:NCUT),NCUT-1,NPGR,NGRK)
        ELSE
          CALL DGTDIR(90)
          CALL DGTXTG(H+PH1,V+PV3,TCUT(2:NCUT),NCUT-1,NPGR,NGRK)
          CALL DQLEVL(ICTXDD)
        END IF
      END IF
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MOD(IFIX(DFWIDU(IZOMDO))/100,10).NE.0) THEN
        NMD=MIN(NMID,N24)
        TCTF=THMID(1:NMD)
        NCTF=NMD
        NGTF=0
        IF(PARADA(4,J_PCF).EQ.1.) THEN
          NCTF1=NCTF+1
          NCTF=NCTF+11
          CF=PARADA(2,J_PCF)*0.5
          FI=PARADA(2,J_PFI)
          TCTF(NCTF1:NCTF)=' '//DT3(FI-CF)//'<F <'//DT3(FI+CF)
          NGTF=NGTF+1
          NPTF(NGTF)=NCTF1+5
        END IF
        IF(PARADA(4,J_PCT).EQ.1.) THEN
          NCTF1=NCTF+1
          NCTF=NCTF+11
          CT=PARADA(2,J_PCT)*0.5
          TE=PARADA(2,J_PTE)
          TCTF(NCTF1:NCTF)=' '//DT3(TE-CT)//'<J <'//DT3(TE+CT)
          NGTF=NGTF+1
          NPTF(NGTF)=NCTF1+5
        END IF
        IF(FDOWN) THEN
          CALL DGTDIR(90)
          CALL DGTXTG(H+PH1,V+PV1,TCTF,NCTF,NPTF,NGTF)
          CALL DQLEVL(ICTXDD)
        ELSE
          CALL DGTXTG(H    ,V+PV2,TCTF,NCTF,NPTF,NGTF)
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
CH --------------------------------------------------------------------  DCTYP0
CH
      ENTRY DCTYP0(M)
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
      NCUT=0
      IF(FPIKDP.OR.
     &  MOD(IFIX(DFWIDU(IZOMDO))/1000,10).EQ.0) GO TO 999
C     IF(WISUDW.GT.0..AND.IAREDO.GE.1.AND.IAREDO.LE.6) GO TO 999
      TCUT=' '
      NGRK=0
      MODTP=M
      MODTR=M
      MODBA=M
      MODPA=M
      MODCA=M
      NMX=NMAX(IAREDO)
      RETURN
  998 NCUT=NMX+1
      TCUT(NCUT:NCUT)='-'
  999 MODTP=0
      MODTR=0
      MODBA=0
      MODPA=0
      MODCA=0
      END
*DK DCUTEC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTEC(K,FOUT)
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
      E=DVEC(IVENDV,K)
      IF(E.LT.ECMIDU) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE,J_HCL'
      CALL DPARAM(20
     &  ,J_HCE,J_HCL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_HCE).EQ.1..AND.E.LT.PARADA(2,J_HCE)) RETURN
      IF(PARADA(4,J_HCL).EQ.1.) THEN
         IF(PARADA(2,J_HCL).EQ.0.)THEN
            IF(DVEC(IVU1DV,K).EQ.0.) RETURN
         ELSE
            IF(PARADA(2,J_HCL).NE.DVEC(IVU1DV,K)) RETURN
         END IF
      END IF
      FOUT=.FALSE.
      END
*DK DCUTHC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTHC(K,FOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-Y1988
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
      E=DVHC(IVENDV,K)
      IF(E.LT.HCMIDU) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE'
      CALL DPARAM(20
     &  ,J_HCE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_HCE).EQ.1..AND.E.LT.PARADA(2,J_HCE)) RETURN
      FOUT=.FALSE.
      END
*DK DCUTLC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTLC(K,FOUT)
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
      DATA CE/0.03/
      FOUT=.TRUE.
      E=DVLC(IVENDV,K)
      IF(E.LT.CCMIDU) RETURN
      IF(E.LT.CE) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE'
      CALL DPARAM(20
     &  ,J_HCE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_HCE).EQ.1..AND.
     &   PARADA(2,J_HCE).GE.E) RETURN
      FOUT=.FALSE.
      END
*DK DCUTFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Get cut limits
C    Inputs    :
C    Outputs   :symbol cut fs1<phi<fs2  ts1<theta<ts2
C               total  cut fc1<phi<fc2  tc1<theta<tc2
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FI=PARADA(2,J_PFI)
      FS1=-999.
      FS2= 999.
      IF(PARADA(4,J_PCF).EQ.-1.) THEN
         D=999.
      ELSE
         D=0.5*PARADA(2,J_PCF)
      END IF
      IF(D.EQ.0.) D=999.
      FC1=FI-D
      FC2=FI+D
      TE=PARADA(2,J_PTE)
      TS1=-999.
      TS2= 999.
      IF(PARADA(4,J_PCT).EQ.-1.) THEN
         D=999.
      ELSE
         D=0.5*PARADA(2,J_PCT)
      END IF
      IF(D.EQ.0.) D=999.
      TC1=TE-D
      TC2=TE+D
      RETURN
      END
*DK DCUTTF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTTF(FCUTF,FC1,FC2,FCUTT,TC1,TC2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Get cut limits
C    Inputs    :
C    Outputs   :symbol cut fs1<phi<fs2  ts1<theta<ts2
C               total  cut fc1<phi<fc2  tc1<theta<tc2
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FCUTF,FCUTT
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PCF).EQ.-1..OR.PARADA(2,J_PCF).EQ.0.) THEN
         FC1=-1024.
         FC2= 1024.
         FCUTF=.FALSE.
      ELSE
         FI=PARADA(2,J_PFI)
         D=0.5*PARADA(2,J_PCF)
         FC1=FI-D
         FC2=FI+D
         FCUTF=.TRUE.
      END IF
      IF(PARADA(4,J_PCT).EQ.-1..OR.PARADA(2,J_PCT).EQ.0.) THEN
         TC1=-1024.
         TC2= 1024.
         FCUTT=.FALSE.
      ELSE
         TE=PARADA(2,J_PTE)
         D=0.5*PARADA(2,J_PCT)
         TC1=TE-D
         TC2=TE+D
         FCUTT=.TRUE.
      END IF
      END
*DK DCUTTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTTR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (BFIELD=15.0,CLGHT=29.9792458)
      DATA PIDEG/57.29577951/
      DATA ZNT2/2./
      LOGICAL FCUT
      FCUTDT=.FALSE.
      CALL DVTR0(NUM)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCA,J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2'
      CALL DPARAM(20
     &  ,J_HCA,J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2)
      TPARDA=
     &  'J_HZZ,J_HST,J_HCQ'
      CALL DPARAM(20
     &  ,J_HZZ,J_HST,J_HCQ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(NUM.EQ.0) GO TO 10
C
C     ========================= SET UP TRACK CUTS
C
      IF(PARADA(4,J_HCM).EQ.1..AND.PARADA(2,J_HCM).NE.0.) THEN
         IF(PARADA(2,J_HCM).GE.0) THEN
            CMO1=PARADA(2,J_HCM)
            CMO2=99999.
         ELSE
            CMO1=0.
            CMO2=-PARADA(2,J_HCM)
         END IF
         FCUTDT=.TRUE.
      ELSE
         CMO1=0.
         CMO2=99999.
      END IF
      IF(PARADA(4,J_HZ0).EQ.1.AND.PARADA(2,J_HZ0).NE.0.) THEN
         IF(PARADA(2,J_HZ0).LT.0.) THEN
            CDZ1=-PARADA(2,J_HZ0)
            CDZ2=99999.
         ELSE
            CDZ1=0.
            CDZ2=PARADA(2,J_HZ0)
         END IF
         FCUTDT=.TRUE.
      ELSE
         CDZ1=0.
         CDZ2=99999.
      END IF
      IF(PARADA(4,J_HD0).EQ.1.AND.PARADA(2,J_HD0).NE.0.) THEN
         IF(PARADA(2,J_HD0).LT.0.) THEN
            CDR1=-PARADA(2,J_HD0)
            CDR2=99999.
         ELSE
            CDR1=0.
            CDR2=PARADA(2,J_HD0)
         END IF
         FCUTDT=.TRUE.
      ELSE
         CDR1=0.
         CDR2=99999.
      END IF
      IF(PARADA(4,J_HT1).EQ.1.) THEN
         CT1=PARADA(2,J_HT1)
         FCUTDT=.TRUE.
      ELSE
         CT1=-99999.
      END IF
      IF(PARADA(4,J_HT2).EQ.1.) THEN
         CT2=PARADA(2,J_HT2)
         FCUTDT=.TRUE.
      ELSE
         CT2=99999.
      END IF
      IF(PARADA(4,J_HZZ).EQ.1.AND.PARADA(2,J_HZZ).NE.0.) THEN
         IF(PARADA(2,J_HZZ).LT.0.) THEN
            CT1=MAX(90.,CT1)
         ELSE
            CT2=MIN(90.,CT2)
         END IF
         FCUTDT=.TRUE.
      END IF
      IF(PARADA(4,J_HF1).EQ.1.) THEN
         CF1=PARADA(2,J_HF1)
         FCUTDT=.TRUE.
      ELSE
         CF1=-99999.
      END IF
      IF(PARADA(4,J_HF2).EQ.1.) THEN
         CF2=PARADA(2,J_HF2)
         FCUTDT=.TRUE.
      ELSE
         CF2=99999.
      END IF
      IF(IHTRDO(2).NE.3.AND.
     &  PARADA(4,J_HST).EQ.1.AND.PARADA(2,J_HST).NE.0.) THEN
         CST=PARADA(2,J_HST)
         FCUTDT=.TRUE.
      ELSE
         CST=0.
      END IF
      IF(PARADA(4,J_HCQ).EQ.1.AND.PARADA(2,J_HCQ).NE.0.) THEN
         CCH=-SIGN(1.,PARADA(2,J_HCQ))
         FCUTDT=.TRUE.
      ELSE
         CCH=0.
      END IF
      NCITC=0
      IF(PARADA(4,J_HNI).EQ.1.AND.PARADA(2,J_HNI).NE.0.) THEN
         CALL=DVCHI0(NTRI)
         IF(NTRI.GT.0) THEN
           CITC=PARADA(2,J_HNI)
           FCUTDT=.TRUE.
           NCITC=CITC
         END IF
      END IF
      NCTPC=0
C     ......... IN TF,RZ,RO,FZ TRACKS NEED AT LEAST 2 TPC HITS TO BE DRAWN
C     ................................................... -> FZTRDT=.TRUE.
      IF(FZTRDT.OR.
     &  (PARADA(4,J_HNT).EQ.1.AND.PARADA(2,J_HNT).NE.0.) ) THEN
        CALL=DVCHT0(NTRT)
        IF(NTRT.GT.0) THEN
          IF(PARADA(4,J_HNT).LE.0.) THEN
            CTPC=ZNT2
          ELSE
            CTPC=MAX(ZNT2,PARADA(2,J_HNT))
          END IF
          FCUTDT=.TRUE.
          NCTPC=CTPC
        END IF
      END IF
      IF(IHTRDO(5).EQ.3) THEN
         FNOTDT(0)=.TRUE.
         FCUT=.TRUE.
      ELSE
         FNOTDT(0)=.FALSE.
      END IF
C
C     ============================= STORE IF TRACK IS CUT OUT
C
      IF(FCUTDT) THEN
         DO   700  N=1,NUM
            IF(COLRDT(N).LT.0.) GO TO 1
            IF(NCITC.GT.0) THEN
              CALL=DVCHI(N,NITC)
              IF(NITC.LT.NCITC) GO TO 1
            END IF
            IF(NCTPC.GT.0) THEN
              CALL=DVCHT(N,NTPC)
              IF(NTPC.LT.NCTPC) GO TO 1
            END IF
            CALL DVTRV(N)
            TE=DATN2D(1.,TPHEDT(2))
            IF(TE.LT.CT1.OR.TE.GT.CT2) GO TO 1
            CH=SIGN(1.,TPHEDT(1))
            IF(CH.EQ.CCH) GO TO 1
            RHO=1./ABS(TPHEDT(1))
            PT=BFIELD*RHO*CLGHT/100000.
            PTOT=PT/SIND(TE)
            IF(PTOT.LT.CMO1.OR.PTOT.GT.CMO2) GO TO 1
            FI=MOD(3600.+TPHEDT(3)*PIDEG,360.)
            IF(FI.LT.CF1.OR.FI.GT.CF2) GO TO 1
            D0=ABS(TPHEDT(4))
            IF(D0.LT.CDR1.OR.D0.GT.CDR2) GO TO 1
            Z0=ABS(TPHEDT(5))
            IF(Z0.LT.CDZ1.OR.Z0.GT.CDZ2) GO TO 1
            NST=CST
            IF(NST.GT.0.) THEN
               IF(NST.NE.N) GO TO 1
            ELSE
               IF(-NST.EQ.N) GO TO 1
            END IF
            FNOTDT(N)=.FALSE.
            GO TO 700
    1       FNOTDT(N)=.TRUE.
  700    CONTINUE
      ELSE
         DO   710  N=1,NUM
            IF(COLRDT(N).LT.0.) THEN
              FNOTDT(N)=.TRUE.
              FCUTDT=.TRUE.
            ELSE
              FNOTDT(N)=.FALSE.
            END IF
  710    CONTINUE
      END IF
C
C     ======================== CUT HITS
C                                               UC
   10 IF(IHTRDO(5).EQ.2) THEN
        FCUHDT=.TRUE.
        DO 720 K=1,NUM
          FNOHDT( K)=.TRUE.
          FNOHDT(-K)=.TRUE.
  720   CONTINUE
        FNOHDT(0)=.FALSE.
        RETURN
      END IF
      IF(FCUTDT.AND.PARADA(2,J_HCA).NE.0.) THEN
C       ================================= TRACK CUTS IMPOSED ON HITS
        FCUHDT=FCUTDT
        IF(IHTRDO(5).LE.1) THEN
C         ===============================         AC
          DO 731 K=1,NUM
            FNOHDT( K)=FNOTDT(K)
            FNOHDT(-K)=FNOTDT(K)
  731     CONTINUE
          FNOHDT(0)=.FALSE.
        ELSE IF(IHTRDO(5).EQ.3) THEN
C         ===============================         TC
          DO 733 K=1,NUM
            FNOHDT( K)=FNOTDT(K)
            FNOHDT(-K)=FNOTDT(K)
  733     CONTINUE
          FNOHDT(0)=.TRUE.
          FCUHDT=.TRUE.
        ELSE IF(IHTRDO(5).GE.4) THEN
C         ===============================         FC,CC
          DO 734 K=1,NUM
            FNOHDT( K)=FNOTDT(K)
            FNOHDT(-K)=.TRUE.
  734     CONTINUE
          IF(IHTRDO(5).EQ.4) THEN
C           =============================         FC
            FNOHDT(0)=.TRUE.
          ELSE
C           =============================         CC
            FNOHDT(0)=.FALSE.
          END IF
          FCUHDT=.TRUE.
        END IF
      ELSE
C       ================================= TRACK CUTS NOT IMPOSED ON HITS
        FCUHDT=.FALSE.
        IF(IHTRDO(5).LE.1) THEN
C         ===============================         AC
          DO 741 K=1,NUM
            FNOHDT( K)=.FALSE.
            FNOHDT(-K)=.FALSE.
  741     CONTINUE
          FNOHDT(0)=.FALSE.
        ELSE IF(IHTRDO(5).EQ.3) THEN
C         ===============================         TC
          DO 743 K=1,NUM
            FNOHDT( K)=.FALSE.
            FNOHDT(-K)=.FALSE.
  743     CONTINUE
          FNOHDT(0)=.TRUE.
          FCUHDT=.TRUE.
        ELSE IF(IHTRDO(5).GE.4) THEN
C         ===============================         FC,CC
          DO 744 K=1,NUM
            FNOHDT( K)=.FALSE.
            FNOHDT(-K)=.TRUE.
  744     CONTINUE
          IF(IHTRDO(5).EQ.4) THEN
C           =============================         FC
            FNOHDT(0)=.TRUE.
          ELSE
C           =============================         CC
            FNOHDT(0)=.FALSE.
          END IF
          FCUHDT=.TRUE.
        END IF
      END IF
      END
