*DK DBT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT
CH
      SUBROUTINE DBT
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
C INVOKED BY TANSW.EQ.'BT'  (DBT)
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL CHG
      CHARACTER *2 TANSW,TLAST,TCC
      DATA TLAST/'ST'/
      PARAMETER (MPR=20)
      DIMENSION PR(4,MPR)
      DATA PR/0.1,  1., 99., 1.,
     &        0. ,  1., 31., 1.,
     &        0. ,200.,999., 1.,
     &        0. ,  0., 99.,-1.,
     &        0. ,  1., 31., 1.,
     &        0. ,  1., 31., 1.,
     &        0.1,  1., 99., 1.,
     &        0. ,  8., 15.,30.,
     &        0. ,  8., 15.,31.,
     &     -999. ,  1.,1024.,1.,
     &     -999. ,  1.,1024.,1.,
     &      -99. ,  1.,1024.,1.,
     &     -999. ,  1.,1024.,1.,
     &     -999. ,  1.,1024.,1.,
     &      -99. ,  1.,1024.,1.,
     &     -999. ,  4.,77777777.,1.,
     &     -999. , 40.,77777777.,1.,
     &         0.,  1.,  99.,1.,
     &         0., .95,   9.,1.,
     &       -99.,  3.,  99.,1./
      CHARACTER *2 TPR(MPR)
C                1    2    3    4    5    6    7    8    9   10
      DATA TPR/'SZ','CR','NU','RN','CO','CA','SH','FR','TO','H1',
     &         'H2','DH','V1','V2','DV','P1','P2','PL','QR','DF'/
      CHARACTER *49 T1,T2,T3,T4
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:W?: Brain test pictures. SZ$123 SH$123 NU$123'/
      DATA T2/'CO$12  CR$12  CA$12  FR$12  TO$12         RN$12'/
      DATA T3/'H1$1234 H2$1234 DH$123   V1$1234 V2$1234 DV$123'/
      DATA T4/'P1$12345678 P2$12345678 PL$12 QR$1234 DF$1234'/
      DIMENSION HH(2),VV(2)
      CALL DQWIL(0.)
      FRNL=FRNLDU
      FRNLDU=0.
  930 CALL DTYPT('TYPE',TPICDO,1,MPR,PR,TPR,T1)
      CALL DTYPT('TYPE',  ' ' ,1,MPR,PR,TPR,T2)
      CALL DTYPT('TYPE',  ' ' ,1,MPR,PR,TPR,T3)
      CALL DTYPT('TYPE',  ' ' ,1,MPR,PR,TPR,T4)
  936 CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,MPR,TPR,PR,
     &  NEXEC,CHG,TANSW)
      GO TO (910,920,930,940),NEXEC
  910 FRNLDU=FRNL
      RETURN
  920 TCC=TLAST
      TLAST=TANSW
      IF(TANSW.EQ.'AA') THEN
        CALL AAAA(PR)
        GO TO 930
      END IF
C     .......................................................... SHOW POINTS
      IF(TANSW.EQ.'SP') THEN
        CALL DBTSP(PR)
        GO TO 930
      END IF
C     .......................................................... SHOW POINTS
      IF(TANSW.EQ.'PB') THEN
        CALL DBTPLB(1,PR)
        GO TO 930
      END IF
C     ........................................... SHOW POINTS OF 2 SIZES
      IF(TANSW.EQ.'D0') THEN
        CALL DBT_POINT_COLORS(0,PR)
        TLAST='DP'
        GO TO 930
      END IF
C     ........................................... SHOW POINTS OF 2 SIZES
      IF(TANSW.EQ.'DP') THEN
        CALL DBT_POINT_COLORS(1,PR)
        GO TO 930
      END IF
C     .......................................................... SHOW LINES
      IF(TANSW.EQ.'SL') THEN
        CALL DBTSL(PR)
        GO TO 930
      END IF
C     .......................................................... SHOW LINES
      IF(TANSW.EQ.'LB') THEN
        CALL DBTPLB(2,PR)
        GO TO 930
      END IF
C     ........................................................ SHOW 2 LINES
      IF(TANSW.EQ.'S2') THEN
        CALL DBTSL2
        GO TO 930
      END IF
C     ........................................................ SHOW 2 LINES
      IF(TANSW.EQ.'DL') THEN
        CALL DBT_2_COL_LINES(PR)
        GO TO 930
      END IF
C     .................................................... SQUARE IN A SQURE
      IF(TANSW.EQ.'F2') THEN
        CALL DBTSA2(PR)
        GO TO 930
      END IF
C     ....................................................... PAIRS OF LINES
      IF(TANSW.EQ.'L2') THEN
        CALL DBT2LI(PR)
        GO TO 930
      END IF
C     ........................................................ STAR OF LINES
      IF(TANSW.EQ.'SS') THEN
        CALL DBTSTA(PR)
        GO TO 930
      END IF
C     ................................................... VERTCAL LINE IN WW
      IF(TANSW.EQ.'VC') THEN
        CALL DBTVCO(PR)
        GO TO 930
      END IF
      CALL DBTSLI(TANSW,PR)
      IF(TANSW.EQ.'**') GO TO 930
C     .................................................... LINES OF POINTS
      IF(TANSW.EQ.'S0') THEN
        CALL DBTTR(0,PR,0)
        TLAST='ST'
        GO TO 930
      END IF
      IF(TANSW.EQ.'ST') THEN
        CALL DBTTR(1,PR,0)
        GO TO 930
      END IF
      IF(TANSW.EQ.'DT') THEN
        CALL DBTTR(1,PR,1)
        GO TO 930
      END IF
      IF(TANSW.EQ.'S6') THEN
        CALL DBTTR6(PR)
        GO TO 930
      END IF
      CALL DO_STR('H0:set up hit recognition')
      IF(TANSW.EQ.'H0') THEN
        CALL DBTTR_COL(0,PR)
        TLAST='HR'
        GO TO 930
      END IF
      CALL DO_STR('HR:hit recognition')
      IF(TANSW.EQ.'HR') THEN
        CALL DBTTR_COL(1,PR)
        GO TO 930
      END IF
C     ................................................. X -> 1/X ; Y -> Y/X
      IF(TANSW.EQ.'LO') THEN
        CALL DBTLIN
        GO TO 930
      END IF
C     .......................................................... SHOW NOISE
      IF(TANSW.EQ.'N0') THEN
        CALL DBTSN(0,PR)
        TLAST='SN'
        GO TO 930
      END IF
      IF(TANSW.EQ.'SN') THEN
        CALL DBTSN(1,PR)
        GO TO 930
      END IF
C     .................................................... TEST GRID: XXXXX
      IF(TANSW.EQ.'TG') THEN
        CALL DBTTGR
        GO TO 930
      END IF
C     .................................................... COLOUR MIXING
      IF(TANSW.EQ.'XD') THEN
        CALL DBTSDP(1)
        GO TO 930
      END IF
C     .................................................... COLOUR MIXING
      IF(TANSW.EQ.'XP') THEN
        CALL DBTSDP(2)
        GO TO 930
      END IF
C     ................................................ DRAW PERSPECTIVE IMAGE
      IF(TANSW.EQ.'PR') THEN
        CALL DBTPRS(PR)
        GO TO 930
      END IF
C     ................................ VARYING BACK GROUND WITH YELLOW POINTS
      IF(TANSW.EQ.'B0') THEN
        CALL DBTVB(0,PR)
        TLAST='BV'
        GO TO 930
      END IF
C     ................................................... VARYING BACK GROUND
      IF(TANSW.EQ.'BV') THEN
        CALL DBTVB(1,PR)
        GO TO 930
      END IF
C     .............................................. VARYING BACK GROUND COLOR
      IF(TANSW.EQ.'BC') THEN
        CALL DBTVB1
        GO TO 930
      END IF
C     ....................................... VARYING BACK GROUND: MOVE POINTS
      IF(TANSW.EQ.'BM') THEN
        CALL DBTVBM(PR)
        GO TO 930
      END IF
C     ................................................. SETUP COLOR BLIND TEXT
      IF(TANSW.EQ.'C0') THEN
        CALL DBCBTX(0,PR)
        TLAST='CT'
        GO TO 930
      END IF
C     ................................................... COLOR BLIND TEXT
      IF(TANSW.EQ.'CT') THEN
        CALL DBCBTX(1,PR)
        GO TO 930
      END IF
C     ................................................. SETUP GRID WITH SQUARE
      IF(TANSW.EQ.'Y0') THEN
        CALL DBCBSQ(0,PR)
        TLAST='YY'
        GO TO 930
      END IF
C     ................................................. GRID WITH SQUARE
      IF(TANSW.EQ.'YY') THEN
        CALL DBCBSQ(1,PR)
        GO TO 930
      END IF
C     ......................................... RECOLOR GRID WITH SQUARE
      IF(TANSW.EQ.'YC') THEN
        CALL DBCBSQ(2,PR)
        GO TO 930
      END IF
C     ......................................... GRID WITH SQUARES
      IF(TANSW.EQ.'SG') THEN
        CALL DBT_SG(PR)
        GO TO 930
      END IF
C     ......................................... WHITE RED WHITE LINES
      IF(TANSW.EQ.'RW') THEN
        CALL DBT_WRW(PR)
        GO TO 930
      END IF
C     ......................................... COLOR TREE
      IF(TANSW.EQ.'TR') THEN
        CALL DBT_TR(PR)
        GO TO 930
      END IF
C     ......................................... FARBEN KREIS
      IF(TANSW.EQ.'FK') THEN
        CALL DBT_FK(PR)
        GO TO 930
      END IF
C     ......................................... LINES IN STEREO
      IF(TANSW.EQ.'L0') THEN
        CALL DBT_STEREO(0,PR)
        TLAST='LS'
        GO TO 930
      END IF
C     ......................................... LINES IN STEREO
      IF(TANSW.EQ.'LS') THEN
        CALL DBT_STEREO(1,PR)
        GO TO 930
      END IF
C     ......................................... 1 LINE IN STEREO
      IF(TANSW.EQ.'X0') THEN
        CALL DBT_STEREO_INT(0,PR)
        TLAST='LS'
        GO TO 930
      END IF
C     ......................................... 1 LINE IN STEREO
      IF(TANSW.EQ.'XX') THEN
        CALL DBT_STEREO_INT(1,PR)
        GO TO 930
      END IF
C     ......................................... random points in stereo
      IF(TANSW.EQ.'Z0') THEN
        CALL DBT_STEREO_RANDOM_POINTS(0,PR)
        TLAST='ZR'
        GO TO 930
      END IF
C     ......................................... random points in stereo
      IF(TANSW.EQ.'ZR') THEN
        CALL DBT_STEREO_RANDOM_POINTS(1,PR)
        GO TO 930
      END IF
C     .................................................. TYPE COLOR INTENSITY
      IF(TANSW.EQ.'T0') THEN
        CALL DBTVBT(0,PR)
        TLAST='TB'
        GO TO 930
      END IF
      IF(TANSW.EQ.'TB') THEN
        CALL DBTVBT(1,PR)
        GO TO 930
      END IF
C     ......................................................... MOVING POINTS
      IF(TANSW.EQ.'MM') THEN
        CALL DBTM(PR)
        GO TO 930
      END IF
C     ......................................................... CHANGE COLORS
      IF(TANSW.EQ.'CC') THEN
        TLAST=TCC
        CALL DBTCC(1,PR)
        GO TO 930
      END IF
C     ...................................................... TEST COLOR VISION
      IF(TANSW.EQ.'V0') THEN
        CALL DBTCBL(0,PR)
        TLAST='VV'
        GO TO 930
      END IF
      IF(TANSW.EQ.'VV') THEN
        CALL DBTCBL(1,PR)
        GO TO 930
      END IF
C     ...................................................... PERSPECTIVE
      IF(TANSW.EQ.'P0') THEN
        CALL DBTNPR(0,PR)
        TLAST='PP'
        GO TO 930
      END IF
      IF(TANSW.EQ.'PP') THEN
        CALL DBTNPR(1,PR)
        GO TO 930
      END IF
C     ............................................................... 3D CUBES
      IF(TANSW.EQ.'D3') THEN
        CALL DBT3DZ(1.)
        CALL DBT3D(PR)
        GO TO 930
      END IF
C     .................................................... CHANGE COLOUR MIXING
      IF(TANSW.EQ.'DD') THEN
        CALL DBT3DC(PR,-1.)
        GO TO 936
      END IF
C     .................................................... UP
      IF(TANSW.EQ.'UU') THEN
        CALL DBT3DC(PR, 1.)
        GO TO 936
      END IF
C     .................................................... Z FORWARD BACKWARD
      IF(TANSW.EQ.'ZZ') THEN
        CALL DBT3DZ(0.)
        CALL DBT3D(PR)
        GO TO 936
      END IF
C     .................................................... DRAW EXPONENTIAL
      IF(TANSW.EQ.'EX') THEN
        CALL DBTEXP(PR,1)
        GO TO 936
      END IF
C     .......................................... DRAW EXPONENTIAL IN LOG SCALE
      IF(TANSW.EQ.'LG') THEN
        CALL DBTEXP(PR,2)
        GO TO 936
      END IF
C     ........................................................... DRAW CIRCLE
      IF(TANSW.EQ.'CI') THEN
        CALL DBTCIR(PR,1)
        GO TO 936
      END IF
C     .................................................... DRAW CIRCLE AS RO/FI
      IF(TANSW.EQ.'RF') THEN
        CALL DBTCIR(PR,2)
        GO TO 936
      END IF
C     .................................................... ROTATE R/F
      IF(TANSW.EQ.'RR') THEN
        CALL DBTCIR(PR,3)
        GO TO 936
      END IF
C     .................................................... ROTATE RF AND SCALE
      IF(TANSW.EQ.'RS') THEN
        CALL DBTCIR(PR,4)
        GO TO 936
      END IF
C     .................................................... EX+LG+CI+RF+RR+RS
      IF(TANSW.EQ.'LC') THEN
        IAREDO=1
        CALL DBTEXP(PR,1)
        IAREDO=2
        CALL DBTEXP(PR,2)
        IAREDO=3
        CALL DBTCIR(PR,1)
        IAREDO=4
        CALL DBTCIR(PR,2)
        IAREDO=6
        CALL DBTCIR(PR,3)
        IAREDO=5
        CALL DBTCIR(PR,4)
        GO TO 936
      END IF
C     ........................................... COPY COLOR INTO WINDOW
      IF(TANSW.EQ.'CW') THEN
        NCOL=PR(2,6)
        CALL DGLEVL(NCOL)
        CALL DGRAR(HMINDG(IAREDO),VMINDG(IAREDO),
     &             HHGHDG(IAREDO),VHGHDG(IAREDO))
        GO TO 936
      END IF
C     ............................................................ DRAW LINE
      IF(TANSW.EQ.'DR') THEN
        NCOL=PR(2,5)
        CALL DGLEVL(NCOL)
        DLINDD=PR(2,1)
        HH(1)=PR(2,10)+PR(2,12)
        HH(2)=PR(2,11)+PR(2,12)
        VV(1)=PR(2,13)+PR(2,15)
        VV(2)=PR(2,14)+PR(2,15)
        CALL DGDRAW(2,HH,VV)
        GO TO 936
      END IF
      IF(TANSW.EQ.'CU') THEN
        CALL DBTCUB(PR)
        GO TO 936
      END IF
C     ........................................ 3D POINT AS PERSPECTIVE DRAWING
      IF(TANSW.EQ.'P3') THEN
        CALL DBT3DP(PR)
        GO TO 936
      END IF
C     ............................................ PUZZLE PLOT explenation 5*5
      IF(TANSW.EQ.'SI') THEN
        CALL DBTSIC(PR)
        GO TO 936
      END IF
C     .............................. drawing sequence for squares and frames
      IF(TANSW.EQ.'SQ') THEN
        CALL DBTFR(1,PR)
        GO TO 930
      END IF
      IF(TANSW.EQ.'Q0') THEN
        CALL DBTFR(0,PR)
        TLAST='SQ'
        GO TO 930
      END IF
      IF(TANSW.EQ.'E0') THEN
        CALL DBT_MAGIC_EYE_1(0,PR)
        TLAST='EM'
        GO TO 930
      END IF
      IF(TANSW.EQ.'EM') THEN
        CALL DBT_MAGIC_EYE_1(1,PR)
        GO TO 930
      END IF
      IF(TANSW.EQ.'E1') THEN
        CALL DBT_MAGIC_EYE_2(0,PR)
        TLAST='ER'
        GO TO 930
      END IF
      IF(TANSW.EQ.'ER') THEN
        CALL DBT_MAGIC_EYE_2(1,PR)
        TLAST='ER'
        GO TO 930
      END IF
      IF(TANSW.EQ.'R0') THEN
        CALL DBT_RANDOM_STEREO(0,PR)
        TLAST='R3'
        GO TO 930
      END IF
      IF(TANSW.EQ.'R3') THEN
        CALL DBT_RANDOM_STEREO(1,PR)
        GO TO 930
      END IF
      IF(TANSW.EQ.'A1') THEN
        CALL DBT_SMOOTH_ROT(0,PR)
        TLAST='AC'
        GO TO 930
      END IF
      IF(TANSW.EQ.'AC') THEN
        CALL DBT_SMOOTH_ROT(1,PR)
        GO TO 930
      END IF
      IF(TANSW.EQ.'F1') THEN
        CALL DBT_FRAMED_AREAS(0,PR)
        TANSW='FA'
        GO TO 930
      END IF
      IF(TANSW.EQ.'FA') THEN
        CALL DBT_FRAMED_AREAS(1,PR)
        GO TO 930
      END IF
C      IF(TANSW.EQ.'I0') THEN
C        CALL DBT_MEAS_COL_INTENSITY(0,PR)
C        TLAST='IM'
C        GO TO 930
C      END IF
C      IF(TANSW.EQ.'IM') THEN
C        CALL DBT_MEAS_COL_INTENSITY(1,PR)
C        GO TO 930
C      END IF

      CALL DO_STR('B1"BP"BR: blue problem')
      IF(TANSW.EQ.'BP'.OR.
     &   TANSW.EQ.'B1'.OR.
     &   TANSW.EQ.'BR') THEN
        CALL DBT_BLUE(TANSW,PR)
        GO TO 930
      END IF

      CALL DO_STR('I1"IP: 1 point in many')
      IF(TANSW.EQ.'IP'.OR.
     &   TANSW.EQ.'I1') THEN
        CALL DBT_1_IN_MANY(TANSW,PR)
        GO TO 930
      END IF

      CALL DWR_IC(TANSW)
      GO TO 936
  940 TANSW=TLAST
      GO TO 920
      END
*DK DBTSLI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSLI
CH
      SUBROUTINE DBTSLI(TANSW,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C DRAW LINES X VERSUS Z AND (X/Z) VERSUS (1/Z)
C
C
C INVOKED BY TANSW.EQ.'BT'  (DBTSLI)
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DH
      COMMON /DHELPT/ TAN1DH,TPRGDH,TLT3DH
      CHARACTER *1 TPRGDH,TLT3DH
      CHARACTER *3 TAN1DH
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      CHARACTER *2 TANSW
      CHARACTER *3 DT3
      CHARACTER *51 T
C      DATA C/0.0001/
      DIMENSION HH(4),VV(4)
      DATA DS/8./
      CHARACTER *2 TNUM(0:127)
      DATA TNUM/' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9',
     &          '10','11','12','13','14','15',112*'  '/
C      DATA DTXH/20./,DTXL/2./,L1/-1/,L2/-2/,LD/2/
      DATA DTXL/2./
C      DATA NSL/15/,DHBL/4./,DVSL/7./,L31/31/,L16/16/
      DATA NSL/15/,DHBL/4./,DVSL/7./
      IF(TANSW.EQ.'SL') THEN
        CALL DQCL(IAREDO)
        NCOL=PDCODD(2,ICBGDD)
        CALL DGLEVL(NCOL)
        CALL DGRAR(HMINDG(IAREDO),VMINDG(IAREDO),
     &             HHGHDG(IAREDO),VHGHDG(IAREDO))
        DHSL=(HHGHDG(IAREDO)-HMINDG(IAREDO))/NSL
        VV(1)=VMINDG(IAREDO)
        HH(1)=HMINDG(IAREDO)
        DO L=1,31
          IF(L.EQ.NCOL) THEN
            LEV=0
          ELSE
            LEV=L
          END IF
          CALL DGLEVL(LEV)
          HH(2)=HH(1)
          DO N=1,NSL
            VV(1)=VV(1)+1.
            VV(2)=VV(1)
            HH(2)=HH(2)+DHSL
            CALL DGDRAW(2,HH,VV)
          END DO
          VV(1)=VV(1)+DVSL
        END DO
        CALL DGLEVL(NCOL)
        VV(1)=VMINDG(IAREDO)
        VV(2)=VV(1)
        VV(3)=VHGHDG(IAREDO)
        VV(4)=VV(3)
        HH(1)=HMINDG(IAREDO)-DHBL
        DO L=1,NSL-1
          HH(1)=HH(1)+DHSL
          HH(2)=HH(1)+2*DHBL
          HH(3)=HH(2)
          HH(4)=HH(1)
          CALL DGAREA(4,HH,VV)
        END DO
        TANSW='sc'
      ELSE IF(TANSW.EQ.'SC') THEN
        LARE=PR(2,9)-1.
        GR6=PR(2,5)/10.
        CALL DQCL(IAREDO)
        H1=HMINDG(IAREDO)+DS
        H2=HHGHDG(IAREDO)-DS
        V1=VMINDG(IAREDO)+DS
        V2=VHGHDG(IAREDO)-DS
        DH=0.25*(H2-H1)
        DV=0.25*(V2-V1)
        L=0
        DO IV=3,0,-1
          VN=IV
          DO IH=0,3
            HN=IH
            HH(1)=H1+HN*DH
            VV(1)=V1+VN*DV
            HH(3)=HH(1)+DH
            VV(3)=VV(1)+DV
            HH(2)=HH(3)
            HH(4)=HH(1)
            VV(2)=VV(1)
            VV(4)=VV(3)
            CALL DGLEVL(L)
            CALL DGAREA(4,HH,VV)
            IF(GRCODD(L).GT.GR6) THEN
              CALL DGLEVL(1)
            ELSE
              CALL DGLEVL(8)
            END IF
            IF(IAREDO.GT.0) THEN
              CALL DGTEXT(HH(1),VV(1)+DTXL,TNUM(L),2)
            ELSE
              T=' '
              T( 1: 2)=      TNUM(L)
              T(13:17)='R='//DT3(RDCODD(L))
              T(19:23)='G='//DT3(GRCODD(L))
              T(25:29)='B='//DT3(BLCODD(L))
              CALL DGTEXT(HH(1),VV(1)+DTXL,T   ,29)
            END IF
            IF(L.EQ.LARE) THEN
C             IF(WISUDW.LE.0..OR.IAREDO.LT.1..OR.IAREDO.GT.8.) THEN
                HL1=HMINDG(IAREDO)+DS
                HL2=HHGHDG(IAREDO)-DS
                VL1=VMINDG(IAREDO)+DS
                VL2=VHGHDG(IAREDO)-DS
                DLH=0.25*(HL2-HL1)-1.
                DLV=0.25*(VL2-VL1)-1.
                DLH=DLH/16.
                DLV=DLV/16.
                HH(1)=HL1
                HH(2)=HL2
                VV(1)=VL1-DLV*0.5
                DLINDD=PDCODD(2,LITRDD)
                DO LL=0,63
                  LCOL=MOD(LL,16)
                  CALL DGLEVL(LCOL)
                  VV(1)=VV(1)+DLV
                  VV(2)=VV(1)
                  IF(LCOL.GT.LARE) CALL DGDRAW(2,HH,VV)
                END DO
C             END IF
            END IF
            L=L+1
          END DO
        END DO
        TANSW='sc'
      ELSE IF(TANSW.EQ.'sc') THEN
        TANSW='**'
        DFWI0=DFWIDU(0)
        DFWI1=DFWIDU(1)
        DFWIDU(0)=0.
        DFWIDU(1)=0.
        CALL DQFR(IAREDO)
        DFWIDU(0)=DFWI0
        DFWIDU(1)=DFWI1
        TAN1DH='  '
      ELSE IF(TANSW.EQ.'VL') THEN
        LARE=AREADU
        TANSW='**'
        H1=HMINDG(IAREDO)+DS
        H2=HHGHDG(IAREDO)-DS
        V1=VMINDG(IAREDO)+DS
        V2=VHGHDG(IAREDO)-DS
        DH=0.25*(H2-H1)-1.
        DH=DH/16.
        VV(1)=V1
        VV(2)=V2
        HH(1)=H1-DH*0.5
        DO L=0,63
          LCOL=MOD(L,16)
          CALL DGLEVL(LCOL)
          IF(LCOL.GT.LARE) THEN
            DLINDD=PDCODD(2,LITRDD)
          ELSE
            DLINDD=PDCODD(2,LIDTDD)
          END IF
          HH(1)=HH(1)+DH
          HH(2)=HH(1)
          IF(DLINDD.NE.0.) CALL DGDRAW(2,HH,VV)
        END DO
        TAN1DH='  '
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTVCO
CH
      SUBROUTINE DBTVCO(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C COLOURS OF VERTICAL LINES
C
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION ICOL(8),LCOL(9)
      DATA ICOL/ 8,15,12, 9, 1,14,13,10/
      DATA LCOL/15, 9,12, 8,13,10,14,11, 1/
      DIMENSION PR(4,*)
      DIMENSION H(2),V(2),VV(3),VM(8),VH(8),HM(8),HH(8)
      DATA VM/  1.,  1.,  1.,  1.,330.,330.,330.,330./
      DATA VH/331.,331.,331.,331.,661.,661.,661.,661./
      DATA HM/  1.,241.,481.,721.,  1.,241.,481.,721./
      DATA HH/240.,480.,720.,960.,240.,480.,720.,960./
      DATA D2/2./,D80/80./,D24/24./
      IAREDO=0
      CALL DQCL(0)
      NCOL=PR(2,2)
      DO I=1,8
        CALL DGLEVL(ICOL(I))
        CALL DGRAR(HM(I),VM(I),HH(I),VH(I))
        V(1)=VM(I)
        V(2)=VH(I)
        IF(V(1).LT.10.) THEN
          VV(1)=V(1)
          VV(2)=V(1)+D80
          VV(3)=V(1)+2.*D80
        ELSE
          VV(1)=V(2)
          VV(2)=V(2)-D80
          VV(3)=V(2)-2.*D80
        END IF
        H(1)=HM(I)
        DO L=1,9
          H(1)=H(1)+D24
          H(2)=H(1)
          CALL DGLEVL(NCOL)
          DLINDD=PR(2,1)+D2
          CALL DGDRAW(2,H,VV(2))
          CALL DGLEVL(LCOL(L))
          CALL DGDRAW(2,H,VV(1))
          DLINDD=PR(2,1)
          CALL DGDRAW(2,H,V)
        END DO
      END DO
      CALL DGEXEC
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSTA
CH
      SUBROUTINE DBTSTA(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C DRAW STAR
C
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DATA HRB/-.6, .6, .6,-.6/
      DATA VRB/-.6,-.6, .6, .6/
      DATA DA/15./
      DIMENSION DL(2),NC(2)
      DL(2)=PR(2,1)
      DL(1)=DL(2)+2.
      NC(1)=PR(2,2)
      NC(2)=PR(2,5)
      CALL DQCL(IAREDO)
      IZOMDO=0.
      CALL DQRU(HRB,VRB)
      NCOL=PR(2,6)
      CALL DGLEVL(NCOL)
      CALL DGRAR(HMINDG(IAREDO),VMINDG(IAREDO),
     &           HHGHDG(IAREDO),VHGHDG(IAREDO))
      IF(PR(4,2).EQ.1.) THEN
        L1=1
      ELSE
        L1=2
      END IF
      DO L=L1,2
        DLINDD=DL(L)
        CALL DGLEVL(NC(L))
        DO A=0.,359.,DA
          SA=SIND(A)
          CA=COSD(A)
          CALL DQL2E(0.,0.,CA,SA)
        END DO
      END DO
      CALL DGEXEC
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT2LI
CH
      SUBROUTINE DBT2LI(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C DRAW 2 LINES
C
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION H1(2),H2(2),V1(2),V2(2)
      DATA N6/6/,DD/1./
      CALL DQCL(IAREDO)
      NCOL=PR(2,6)
      CALL DGLEVL(NCOL)
      CALL DGRAR(HMINDG(IAREDO),VMINDG(IAREDO),
     &           HHGHDG(IAREDO),VHGHDG(IAREDO))
      NCOL=PR(2,5)
      CALL DGLEVL(NCOL)
      DLINDD=PR(2,1)
      DS=PR(2,12)
      D=(VHGHDG(IAREDO)-VMINDG(IAREDO))/(FLOAT(N6)+1.)
      V1(1)=VMINDG(IAREDO)-DS
      V1(2)=VMINDG(IAREDO)+DS
      H1(1)=HMINDG(IAREDO)
      H1(2)=HHGHDG(IAREDO)
      DL=0.
      DO N=1,N6
        V1(1)=V1(1)+D
        V1(2)=V1(2)+D
        CALL DGDRAW(2,H1,V1)
        V2(1)=V1(1)+DL
        V2(2)=V1(2)+DL
        CALL DGDRAW(2,H1,V2)
        DL=DL+DD
      END DO
      D=(HHGHDG(IAREDO)-HMINDG(IAREDO))/(FLOAT(N6)+1.)
      H1(1)=HMINDG(IAREDO)+DS
      H1(2)=HMINDG(IAREDO)-DS
      V1(1)=VMINDG(IAREDO)
      V1(2)=VHGHDG(IAREDO)
      DL=0.
      DO N=1,N6
        H1(1)=H1(1)+D
        H1(2)=H1(2)+D
        CALL DGDRAW(2,H1,V1)
        H2(1)=H1(1)+DL
        H2(2)=H1(2)+DL
        CALL DGDRAW(2,H2,V1)
        DL=DL+DD
      END DO
      CALL DGEXEC
      END
*DK DBTLIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTLIN
CH
      SUBROUTINE DBTLIN
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C DRAW LINES X VERSUS Z AND (X/Z) VERSUS (1/Z)
C
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA QH/0.18/,LC1,LC2,LC3/3,3,5/
      CALL DQCL(IAREDO)
      V0=VMINDG(IAREDO)
      V1=VLOWDG(IAREDO)
      V2=VHGHDG(IAREDO)
      VM=0.5*(V0+V2)
      H0=HMINDG(IAREDO)
      H1=HLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      DH=(H2-H0)*QH
      CALL DBTLID('Y/X',H0,H2-DH,VM,V2,LC1)
      CALL DBTLID('LIN',H0,H2-DH,V0,VM,LC2)
      CALL DBTLID('Y/X',H0,H2-DH,V0,VM,-1  )
      LCOL=PDCODD(2,ICM2DD)
      CALL DBTLID('INV',H2-DH,H2,V0,VM,LC3)
      VLOWDG(IAREDO)=V1
      VHGHDG(IAREDO)=V2
      HLOWDG(IAREDO)=H1
      HHGHDG(IAREDO)=H2
      CALL DGLEVL(8)
      CALL DQDAR(H0,V0,H2,V2)
      CALL DGEXEC
      END
*DK DBTLID
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTLIN
CH
      SUBROUTINE DBTLID(TMODE,H1,H2,V1,V2,LCOL)

CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C DRAW LINES X VERSUS Z AND (X/Z) VERSUS (1/Z)
C
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TMODE
      DIMENSION HRB(4),VRB(4)
      DIMENSION A(8),B(8)
C              W     G     Y     O     R     M     L     B
C              1     2     3     4     5     6     7     8
      DATA A/  8. , 12. ,-10. ,  0. ,-10. ,  6. ,-10. , 0.  /
      DATA B/ 0.08, 0.27,-.305,-0.36, 0.25,-0.32, 0.08, 0.3 /
      DATA HU1,VU1,HU2,VU2/-1.,-50.,100.,50./,HI1,HI2/-2.,21./
      DATA HP1/81./,HP2/99./,DP/2./
      DIMENSION ICOL(0:7)
      DATA ICOL/8,12,14,11,15,10,13,9/,AMP/100./
      VV(HH)=AA+BB*HH
      VLOWDG(IAREDO)=V1
      VHGHDG(IAREDO)=V2
      HLOWDG(IAREDO)=H1
      HHGHDG(IAREDO)=H2
      IF(LCOL.GE.0) THEN
        CALL DQFWIA(LCOL)
        CALL DGLEVL(8)
        CALL DQDWI
      END IF
      IF(TMODE.EQ.'INV') THEN
        CALL DQRER(0,HI1,VU1,HI2,VU2,HRB,VRB)
        CALL DQRU(HRB,VRB)
      ELSE
        CALL DQRER(0,HU1,VU1,HU2,VU2,HRB,VRB)
        CALL DQRU(HRB,VRB)
        CALL DGLEVL(8)
        CALL DQPD(0.,0.)
      END IF
C//   CALL DQPD0(MSYMDL(MTPCDL,0),WDSNDL(2,4,MTPCDL),0.)
      CALL DPAR_SET_SY(62,0.)
      DO K=1,8
        AA=A(K)
        BB=B(K)
        IF(TMODE.EQ.'LIN') THEN
          CALL DGLEVL(K+7)
          CALL DQL2E(0.,VV(0.),99.,VV(99.))
        ELSE
          DO H=HP1,HP2,DP
            V=VV(H)
            QVH=V/H
            LEV=ICOL(MOD(INT(800.+QVH*AMP),8))
            CALL DGLEVL(LEV)
            IF(TMODE.EQ.'Y/X') THEN
              CALL DQPD(H,V)
            ELSE
              HI=HP1*HP2*(1./H-1./HP2)
              VI=QVH*HP2
              CALL DQPD(HI,VI)
            END IF
          END DO
        END IF
      END DO
      END
*DK DBTSA2
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSA2
CH
      SUBROUTINE DBTSA2(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA Q2/0.4/,Q4/0.2/
C     DIMENSION NCOL(10)
C                1  2  3  4  5  6  7  8  8 10
C     DATA NCOL/ 1, 8,12,15,10,14, 9,13,11, 7/
      DATA D15/15./,N15/15/,D1/1./
C     DATA ICOL/ 8, 1, 8, 8, 1, 1, 1, 8, 8, 1/
      DIMENSION PR(4,*)
      CALL DQCL(IAREDO)
      CALL DQFFWI(N1)
      H1=HMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      DH=(H2-H1)/D15
      DH2=DH*Q2
      DH4=MIN(DH*Q4,PR(2,1))
      DH5=DH4+D1
      V1=VMINDG(IAREDO)
      V2=VHGHDG(IAREDO)
      DV=(V2-V1)/D15
      DV2=DV*Q2
      DV4=MIN(DV*Q4,PR(2,1))
      DV5=DV4+D1
      H=HMINDG(IAREDO)-0.5*DH
      ICOL=PR(2,2)
      DO NH=1,N15
        H=H+DH
        V=VMINDG(IAREDO)-0.5*DV
        DO NV=1,N15
          V=V+DV
          CALL DGLEVL(NH)
          CALL DQFAR(H-DH2,V-DV2,H+DH2,V+DV2)
          IF(PR(4,2).GT.0.) THEN
            CALL DGLEVL(ICOL)
            CALL DQDAR(H-DH2,V-DV2,H+DH2,V+DV2)
            CALL DQFAR(H-DH5,V-DV5,H+DH5,V+DV5)
          END IF
          CALL DGLEVL(NV)
          CALL DQFAR(H-DH4,V-DV4,H+DH4,V+DV4)
C          IF(PR(4,2).GT.0.) THEN
C            CALL DGLEVL(ICOL)
C            CALL DQDAR(H-DH4,V-DV4,H+DH4,V+DV4)
C          END IF
        END DO
      END DO
      CALL DGEXEC
      END
*DK DBTSDP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSDP
CH
      SUBROUTINE DBTSDP(JDP)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *12 T
      DATA CLEV/.6/,HT/6./,VT/6./,L0/30/,L1/31/
      DIMENSION CR(5,2),CG(5,2),CB(5,2)
      DATA CR/ 0.,.25,.5 ,.75,1. ,   0.,.2 ,.4 ,.6 ,1. /
      DATA CG/ 0.,.25,.5 ,.75,1. ,   0.,.2 ,.4 ,.6 ,1. /
      DATA CB/ 0.,.25,.5 ,.75,1. ,   0.,.35,.5 ,.65,1. /
      DATA DC/0./
      DATA LDEB/0/
      CALL DCDMS
      RDCODD(30)=0.
      GRCODD(30)=0.
      BLCODD(30)=0.
      RDCODD(31)=1.
      GRCODD(31)=1.
      BLCODD(31)=1.
      HM=HMINDG(0)
      HH=HHGHDG(0)
      VM=VMINDG(0)
      VH=VHGHDG(0)
      DH=0.2*(HH-HM)
      DV=0.2*(VH-VM)
      DO IR=1,5
        L=0
        V1=VH
        DO IB=1,5
          V2=V1-DV
          H1=HM
          DO IG=1,5
            H2=H1+DH
            RDCODD(L)=CR(IR,JDP)
            GRCODD(L)=CG(IG,JDP)
            BLCODD(L)=CB(IB,JDP)
            CALL DGLEVL(L)
            CALL DQFAR(H1,V1,H2,V2)
            IF(CG(IG,JDP).LT.CLEV.AND.CR(IR,JDP).LT.CLEV) THEN
              CALL DGLEVL(L1)
            ELSE
              CALL DGLEVL(L0)
            END IF
            IF(LDEB.EQ.0) CALL DQDAR(H1,V1,H2,V2)
            ICR=CR(IR,JDP)*100.
            ICG=CG(IG,JDP)*100.
            ICB=CB(IB,JDP)*100.
            WRITE(T,1000) ICR,ICG,ICB
 1000       FORMAT(3I4)
            CALL DGTEXT(H1+HT,V2+VT,T,12)
            L=L+1
            CH=CH+DC
            H1=H2
          END DO
          V1=V2
        END DO
        CALL DW_SET_CO
        CALL DGETLN(T,LF,2)
      END DO
      CALL DCDMR
      END
*DK DBTSL2
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSL2
CH
      SUBROUTINE DBTSL2
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION V(2),H(2),NCOL(4),GV(2),NEND(2)
      DATA NCOL/9,10,12,15/,NEND/4,8/,GV/6.,28./,N1/1/,N8/1/
      CALL DQCL(IAREDO)
      CALL DQFFWI(N1)
      DLINDD=1.
      LC=2
      IF(IAREDO.GT.0.AND.IAREDO.LT.9) LC=1
      H1=HMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      DH=(H2-H1)/8.
      V1=VMINDG(IAREDO)
      V2=VHGHDG(IAREDO)
      DV=(V2-V1)/GV(LC)
      VMID=V1+DV*0.5
      DO N=1,NEND(LC)-1
        DO J=N+1,NEND(LC)
          H(1)=H1+0.25*DH
          H(2)=H1+0.75*DH
          DO K=1,8
            IF(LC.EQ.1) THEN
              CALL DGLEVL(NCOL(N))
            ELSE
              CALL DGLEVL(7+N)
            END IF
            DO L=1,K
              V(1)=VMID+L
              V(2)=V(1)
              CALL DGDRAW(2,H,V)
            END DO
            IF(LC.EQ.1) THEN
              CALL DGLEVL(NCOL(J))
            ELSE
              CALL DGLEVL(7+J)
            END IF
            DO L=0,K-1
              V(1)=VMID-L
              V(2)=V(1)
              CALL DGDRAW(2,H,V)
            END DO
            IF(K.EQ.8) THEN
              V(1)=VMID
              V(2)=V(1)
              CALL DGLEVL(N8)
              CALL DGDRAW(2,H,V)
              V(1)=VMID+1.
              V(2)=V(1)
              CALL DGLEVL(N1)
              CALL DGDRAW(2,H,V)
            END IF
            H(1)=H(1)+DH
            H(2)=H(2)+DH
          END DO
          VMID=VMID+DV
        END DO
      END DO
      DLINDD=PDCODD(2,LITRDD)
      CALL DQLEVL(ICFRDD)
      CALL DQDAR(
     &  HMINDG(IAREDO),VMINDG(IAREDO),
     &  HHGHDG(IAREDO),VHGHDG(IAREDO))
      CALL DGEXEC
      END
*DK DBTSN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSN
CH
      SUBROUTINE DBTSN(MO,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      COMMON/DTBSNC/SNDH,SNDV
      DIMENSION PR(4,*)
      DIMENSION IR(0:99)
      DATA IRAN/123913/,QV/0.13/,LEV/2/,N3/3/,N7/7/
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      IF(MO.EQ.0) THEN
        PR(2,5)=8.
        PR(2,2)=8.
        PR(4,4)=-1.
        PR(2,7)=4.
        PR(4,7)=-1.
        PR(2,1)=2.
        PR(2,3)=555.
        PR(2,6)=1.
      END IF
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        DO N=0,99
          IR(N)=(IRAN/N7)*N3
          QQ=RAN(IRAN)
        END DO
      END IF
      NR=PR(2,4)
      LEV=PR(2,6)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LEV)
      H1=HMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V1=VMINDG(IAREDO)
      V2=VHGHDG(IAREDO)
      VM=V1+QV*(V2-V1)
      NRP=PR(2,3)
      SNDV=PR(2,1)
      IF(PR(4,7).GT.0.) THEN
        SNDH=PR(2,7)
      ELSE
        SNDH=SNDV
      END IF
      IRA=IR(NR)
      CALL DBTSND(PR,NRP,IRA,H1,V1,H2,VM)
      IRA=IR(NR)
      CALL DBTSND(PR,NRP,IRA,H1,VM,H2,V2)
      CALL DGEXEC
      IF(PR(4,4).LE.0..AND.PR(2,4).LT.PR(3,4)) PR(2,4)=PR(2,4)+1.
      END
*DK DBTSND
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSND
CH
      SUBROUTINE DBTSND(PR,NRP,IRAN,H1,V1,H2,V2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DATA NTR/20/,QV/0.2/,QH/0.02/,NCOL/8/,JCOL/8/
      DATA QH2/0.4/,D2/2./
      DIMENSION HW(2),VW(2)
      LOGICAL F2
      NCOL=PR(2,5)
      JCOL=PR(2,2)
      IF(NCOL.EQ.JCOL) THEN
        F2=.TRUE.
      ELSE
        F2=.FALSE.
      END IF
      CALL DGLEVL(NCOL)
      DH=H2-H1
      DV=V2-V1
      DV1=    (V2-V1)/NTR
      IF(.NOT.F2) THEN
        DH1=    (H2-H1)/NTR
        HM=H1
        HD=QH2*DH*2.
      ELSE
        DH1=0.5*(H2-H1)/NTR
        HM=0.5*(H1+H2)
        HD=QH2*DH
      END IF
      IF(F2) CALL DBTSNT(NTR, H1,V1,DH1,DV1)
      CALL        DBTSNT(NTR, HM,V1,DH1,DV1)
      DH2=-DH1*QH
      DV2= DV1*QV
      IF(F2) CALL DBTSNT(NTR, H1+HD,V1,DH2,DV1)
      CALL        DBTSNT(NTR, HM+HD,V1,DH2,DV1)
      HS1=0.5*(H1+HM)
      HS2=0.5*(HM+H2)
      VS =0.5*(V1+V2)
      IF(F2) CALL DBTSNT(NTR,HS1,VS,DH2,DV2)
      CALL        DBTSNT(NTR,HS2,VS,DH2,DV2)
      CALL DQLEVL(ICFRDD)
      CALL DQDAR(H1,V1,H2,V2)
      IF(F2) THEN
        DLIN=DLINDD
        DLINDD=D2
        HW(1)=HM
        HW(2)=HM
        VW(1)=V1
        VW(2)=V2
        CALL DGDRAW(2,HW,VW)
        DLINDD=DLIN
        DH=0.5*DH
      END IF
      CALL DGLEVL(JCOL)
      DO K=1,NRP
        Q=RAN(IRAN)
        H=HM+Q*DH
        Q=RAN(IRAN)
        V=V1+Q*DV
        CALL DBTSNP(H,V)
      END DO
      END
*DK DBTSNT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSNT
CH
      SUBROUTINE DBTSNT(NTR,HH,VV,DH,DV)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      H=HH
      V=VV
      DO K=1,NTR
        H=H+DH
        V=V+DV
        CALL DBTSNP(H,V)
      END DO
      END
*DK DBTSNP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSNP
CH
      SUBROUTINE DBTSNP(H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      COMMON/DTBSNC/SNDH,SNDV
      DIMENSION HW(4),VW(4)
      HW(1)=H
      HW(4)=HW(1)
      HW(3)=H+SNDH
      HW(2)=HW(3)
      VW(1)=V
      VW(2)=VW(1)
      VW(3)=V+SNDV
      VW(4)=VW(3)
      CALL DGAREA(4,HW,VW)
      END
*DK DBTSPB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSP
CH
      SUBROUTINE DBTPLB(IPL,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION CR(6),CA(6)
      DATA CR/1.,8.,1.,8.,1.,8./
      DATA CA/1.,1.,0.,0.,8.,8./
      PR(4,2)=1.
      IF(PR(2,9).LE.8.) THEN
        PR(2,9)=15.
        PR(2,8)=7.
      END IF
      CALL DGTIM0
      DO IAREDO=1,6
        PR(2,2)=CR(IAREDO)
        PR(2,6)=CA(IAREDO)
        IF(IPL.EQ.1) THEN
          CALL DBTSP(PR)
        ELSE
          CALL DBTSL(PR)
        END IF
      END DO
      CALL DGTIM1
      END
*DK DBTSP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSP
CH
      SUBROUTINE DBTSP(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HH(4),VV(4),HR(4),VR(4)
      DATA N16/16/,J2/2/
      NP=MIN(N16,IFIX(PR(2,3)))
      CALL DQCL(IAREDO)
      CALL DQFFWI(IFIX(PR(2,6)))
      L1=PR(2,8)
      L2=PR(2,9)
      DV=(VHGHDG(IAREDO)-VMINDG(IAREDO))/FLOAT(L2-L1+J2)
      IDV2=0.5*DV
      IDV=DV
      IVM=VMINDG(IAREDO)
      DH=(HHGHDG(IAREDO)-HMINDG(IAREDO))/FLOAT(N16+1)
      IDH2=0.5*DH
      IDH=DH
      IH=HMINDG(IAREDO)-IDH2
      D1=PR(2,1)
      LR=PR(2,2)
      DO KH=1,NP
        IH=IH+IDH
        IV=IVM-IDV2
        ID=KH-1
        DO KV=L1,L2
          IV=IV+IDV
          HH(1)=IH
          HH(3)=IH+ID
          VV(1)=IV
          VV(3)=IV+ID
          HH(2)=HH(3)
          HH(4)=HH(1)
          VV(2)=VV(1)
          VV(4)=VV(3)
          IF(PR(4,2).EQ.1.) THEN
            HR(1)=HH(1)-D1
            HR(3)=HH(3)+D1
            VR(1)=VV(1)-D1
            VR(3)=VV(3)+D1
            HR(2)=HR(3)
            HR(4)=HR(1)
            VR(2)=VR(1)
            VR(4)=VR(3)
            CALL DGLEVL(LR)
            CALL DGAREA(4,HR,VR)
          END IF
          CALL DGLEVL(KV)
C          IF(KH.EQ.1) THEN
C            CALL DGPLOT(1,H,V)
C          ELSE
C            CALL DGAREA(4,HH,VV)
C          END IF
          CALL DGAREA(4,HH,VV)
        END DO
      END DO
      CALL DGLEVL(IFIX(PR(2,5)))
      DLINDD=PDCODD(2,LIDTDD)
      CALL DQDAR(
     &  HMINDG(IAREDO),VMINDG(IAREDO),
     &  HHGHDG(IAREDO),VHGHDG(IAREDO))
      DLINDD=PDCODD(2,LITRDD)
      CALL DQFR(IAREDO)
      END
*DK DBTSL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTSL
CH
      SUBROUTINE DBTSL(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HH(2),VV(2)
      DIMENSION DA1(2),DA2(2)
      DATA DA1/0., 9./
      DATA DA2/0.,13./
      DATA QL/0.7/,N3/3/,G4/4./
      CALL DQCL(IAREDO)
      CALL DQFFWI(IFIX(PR(2,6)))
      L1=PR(2,8)
      L2=PR(2,9)
      DH=(HHGHDG(IAREDO)-HMINDG(IAREDO))/G4
      DV=(VHGHDG(IAREDO)-VMINDG(IAREDO))/(FLOAT(L2-L1+1)+0.5)
      H=HMINDG(IAREDO)-0.5*DH
      LR=PR(2,2)
      DL=QL*DH
      DO KH=1,N3
        H=H+DH
        V=VMINDG(IAREDO)-0.5*DV
        DO KV=L1,L2
          V=V+DV
          DO IL=1,2
            HH(1)=H
            HH(2)=H+DL
            VV(1)=V+DA1(IL)
            VV(2)=V+DA2(IL)
            IF(PR(4,2).EQ.1.) THEN
              DLINDD=KH+2
              CALL DGLEVL(LR)
              CALL DGDRAW(2,HH,VV)
            END IF
            DLINDD=KH
            CALL DGLEVL(KV)
            CALL DGDRAW(2,HH,VV)
          END DO
        END DO
      END DO
      CALL DGLEVL(IFIX(PR(2,5)))
      DLINDD=PDCODD(2,LIDTDD)
      CALL DQDAR(
     &  HMINDG(IAREDO),VMINDG(IAREDO),
     &  HHGHDG(IAREDO),VHGHDG(IAREDO))
      DLINDD=PDCODD(2,LITRDD)
      CALL DGEXEC
      END
*DK DBTTGR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTTGR
CH
      SUBROUTINE DBTTGR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *1 T
      DATA T/'X'/
      DATA D/12./,DH/10./,DV/12./
      DATA IDEB/0/
      H1=HMINDG(0)
      H2=HHGHDG(0)
      V1=VMINDG(0)
      V2=VHGHDG(13)
      IF(IDEB.EQ.0) THEN
        CALL DQLEVL(ICBGDD)
        CALL DGRAR(H1,V1,H2,V2)
      END IF
      CALL DQLEVL(ICTXDD)
      VR=V1-DV
    1 VR=VR+D
      HR=H1-DH
    2 HR=HR+D
      CALL DGTEXT(HR,VR,T,1)
      IF(HR.LT.H2) GO TO 2
      IF(VR.LT.V2) GO TO 1
      END
*DK DBTTR6
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTTR
CH
      SUBROUTINE DBTTR6(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DATA N1/1/,H1/1./,DH/165./,HS/826./,V1/1./,V2/660./
      PR1=PR(2,1)
      PR7=PR(2,7)
      P15=PR(2,15)
      CALL DBTTR(0,PR,0)
      PR(2,1)=PR1
      PR(2,7)=PR7
      H=H1
      DO I=1,6
        HMINDG(I)=H
        H=H+DH
        HHGHDG(I)=H
        VMINDG(I)=V1
        VHGHDG(I)=V2
      END DO
      HHGHDG(12)=HS
C     ....................................
      IAREDO=1
      PR(2,8)=1.
      CALL DBTTR(1,PR,0)
      NOCLDT=1
      PR(2,8)=2.
      CALL DBTTR(1,PR,0)
      NOCLDT=0
C     ....................................
      IAREDO=2
      CALL DBTTR(1,PR,0)
C     ....................................
      IAREDO=3
      PR(2,5)=0.
      CALL DBTTR(1,PR,0)
      PR(2,5)=8.
C     ....................................
      PR(2,17)=N1
      PR(2,12)=8.
      IAREDO=4
      PR(2,15)=P15
      CALL DBTTR(1,PR,0)
      PR(2,15)=0.
CC     ....................................
C      IAREDO=5
C      PR(2,14)=6.
C      CALL DBTTR(1,PR,0)
C      PR(2,14)=8.
C     ....................................
      IAREDO=5
      PR(2,12)=12.
      CALL DBTTR(1,PR,0)
C     ....................................
      IAREDO=6
      CALL DBTTR(1,PR,-1)
      PR(2,12)=8.
      PR(2,15)=P15
      END
*DK DBTTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTTR
CH
      SUBROUTINE DBTTR(MO,PR,IHTR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DATA D2/3./
      DIMENSION HD(3),VD(4),HD2(2),VD2(2),
     &  NH1(6),NH2(6),NV1(6),NV2(6),HU1(6),HU2(6),VU1(6),VU2(6)
      DATA NH1/1,1,1,2,2,2/
      DATA NH2/2,2,2,3,3,3/
      DATA NV1/3,2,1,3,2,1/
      DATA NV2/4,3,2,4,3,2/
      DATA HU1/-1., 6.5, 6.5,-1., 6.5, 6.5/
      DATA HU2/21.,13.5,13.5,21.,13.5,13.5/
      DATA VU1/-1., 6.5, 6.5,-1., 6.5, 6.5/
      DATA VU2/21.,13.5,13.5,21.,13.5,13.5/
      DIMENSION HP(0:20,12),VP(0:20,12),HRB(5),VRB(5),HT(4),VT(4)
      DIMENSION HTR(2,12,2),LTR(12),VTR(2),WTD(12)
      DATA VTR/0.,20./,VT/0.,0.,20.,20./
      DATA HTR/11. , 9. ,
     1          9. , 8. ,
     &          9.5,12.5,
     &         11.7,11. ,
     &          8.5,10.5,
     &         12.3, 8.5,
     &          6.5,11.3,
     &          2. , 6. ,
     &          5. , 1. ,
     &         20. ,14. ,
     &         14. ,19. ,
     &         16. ,17. ,
     2         11. , 8. ,
     &          9.3, 8. ,
     &          9.5,12.5,
     &         12.3,11.5,
     &          8. ,11. ,
     &         12.3,10. ,
     &          9.5, 9.5,
     &          2. , 6. ,
     &          5. , 1. ,
     &         20. ,14. ,
     &         14. ,19. ,
     &         16. ,17. /
      DATA LTR/12,9,10,11,8,14,13,15,8,9,10,12/
      DATA W2/2./,SX/1./,NX/3/,D1/1./
      DATA QPIC/0.065/,NCAR/12/,MCOL/0/
      DATA L1/1/,DHP0/1./
      LOGICAL FIN
      DLIN=DLINDD
      IF(MO.EQ.0) THEN
        PR(2,6)=1.
        PR(2,5)=8.
        PR(2,2)=1.
        PR(4,2)=-1.
        PR(2,1)=3.
        PR(2,17)=0.
        PR(2,8)=3.
        PR(2,7)=1.
        PR(2,11)=1.
        PR(2,10)=1.
        PR(2,16)=0.
        PR(2,13)=8.
        PR(2,14)=8.
        PR(2,15)=0.
        PR(2,12)=12.
        RETURN
      END IF
      LCA=PR(2,6)
      LCO=PR(2,5)
      LCR=PR(2,2)
      SIZ=PR(2,1)
      NTX=PR(2,17)
      NTP=PR(2,8)
      WTR=PR(2,7)
      DL1=PR(2,11)
      LTX=PR(2,12)
      MS=PR(2,13)
      NX=PR(2,14)
      SX=SIZ+PR(2,15)
      MOT=MOD(PR(2,16),5.)
      IF(PR(2,10).EQ.2) THEN
        NTR=2
      ELSE
        NTR=1
      END IF
      DO N=1,12
        Q=(HTR(2,N,NTR)-HTR(1,N,NTR))/20.
        V=0.
        DO K=0,20
          VP(K,N)=V
          HP(K,N)=HTR(1,N,NTR)+V*Q
          V=V+1.
        END DO
      END DO
      CALL DQCL(IAREDO)
      IF(NOCLDT.EQ.0) CALL DQFFWI(LCA)
      CALL DQAR0(0.,0.)
      P=0.
      VL=VLOWDG(IAREDO)
      HL=HLOWDG(IAREDO)
      VH=VHGHDG(IAREDO)
      HH=HHGHDG(IAREDO)
      VD(1)=VMINDG(IAREDO)
      VD(4)=VHGHDG(IAREDO)
      DV=(VD(4)-VD(1))
      VD(2)=VD(1)+DV*QPIC
      VD(3)=0.5*(VD(2)+VD(4))
      HD(1)=HMINDG(IAREDO)
      HD(3)=HHGHDG(IAREDO)
      DHP=0.5*(HD(3)-HD(1))-DHP0
      L1=1
      L6=6
      IF     (NTP.EQ.1) THEN
        HD(3)=2.*HD(3)-HD(1)
        L6=3
      ELSE IF(NTP.EQ.2) THEN
        HD(1)=2.*HD(1)-HD(3)
        L1=4
      END IF
      HD(2)=0.5*(HD(1)+HD(3))
      DO L=L1,L6
        DLINDD=D1
        HLOWDG(IAREDO)=HD(NH1(L))
        HHGHDG(IAREDO)=HD(NH2(L))
        VLOWDG(IAREDO)=VD(NV1(L))
        VHGHDG(IAREDO)=VD(NV2(L))
        CALL DQRER(0,HU1(L),VU1(L),HU2(L),VU2(L),HRB,VRB)
        HRB(5)=HRB(1)
        VRB(5)=VRB(1)
        IF(L.EQ.2.OR.L.EQ.5) THEN
          IF(MOT.EQ.0) THEN
            CALL DGLEVL(NCAR)
            DO I=1,4
              CALL DQLIE(HRB(I),VRB(I))
            END DO
          END IF
        END IF
        CALL DQRU(HRB,VRB)
        IF     (LCA.EQ.1) THEN
          CALL DGLEVL(8)
        ELSE IF(LCA.EQ.8) THEN
          CALL DGLEVL(1)
        ELSE
          CALL DQLEVL(ICFRDD)
        END IF
        CALL DQDAR(
     &    HLOWDG(IAREDO),VLOWDG(IAREDO),
     &    HHGHDG(IAREDO),VHGHDG(IAREDO))
        P=P+1.
        IF(L.LE.3) THEN
          IF(MOT.EQ.0) THEN
            IF(PR(4,2).GT.0.) THEN
              DLINDD=WTR+W2
              CALL DGLEVL(LCR)
              DO N=1,12
                CALL DQLIE(HTR(1,N,NTR),VTR)
              END DO
            END IF
            DLINDD=WTR
            CALL DGLEVL(LCO)
            DO N=1,12
              IF(LCO.EQ.0) CALL DGLEVL(LTR(N))
              CALL DQLIE(HTR(1,N,NTR),VTR)
            END DO
          ELSE
            WTRD=WTR/AHSCDQ
            DO N=1,12
              CALL DQPO0('AREA',LTR(N),0,' ')
              DHTD=(HTR(2,N,NTR)-HTR(1,N,NTR))/AHSCDQ
              DVTD=(VTR(2      )-VTR(1      ))/BVSCDQ
              WTD(N)=WTRD*DVTD/SQRT(DVTD**2+DHTD**2)
              HT(1)=HTR(1,N,NTR)-WTD(N)
              HT(2)=HTR(1,N,NTR)+WTD(N)
              HT(3)=HTR(2,N,NTR)+WTD(N)
              HT(4)=HTR(2,N,NTR)-WTD(N)
              CALL DQPOL(4,HT,VT)
            END DO
C           ............... MOT=0 : FIRST FRAME THAN TRACKS
C           ............... MOT=1 : TRACK AREA
C           ............... MOT=2 : TRACK AREA + BORDERS IN TRACK COLOR
C           ............... MOT=3 : TRACK AREA + BLACK BORDER
C           ............... MOT=4 : TRACK AREA + FRAMED COLORED BORDER
            IF(MOT.GT.1) THEN
              DLINDD=DL1
              DO N=12,1,-1
                IF(MOT.GT.3) THEN
                  CALL DGLEVL(LCR)
                  DLINDD=DL1+2.
                  CALL DQL2E(HTR(1,N,NTR)-WTD(N),VTR(1),
     &                       HTR(2,N,NTR)-WTD(N),VTR(2))
                  CALL DQL2E(HTR(1,N,NTR)+WTD(N),VTR(1),
     &                       HTR(2,N,NTR)+WTD(N),VTR(2))
                  DLINDD=DL1
                END IF
                IF(MOT.EQ.3) THEN
                  CALL DGLEVL(LCR)
                ELSE
                  CALL DGLEVL(LTR(N))
                END IF
                DLINDD=1.
                CALL DQL2E(HTR(1,N,NTR)-WTD(N),VTR(1),
     &                     HTR(2,N,NTR)-WTD(N),VTR(2))
                CALL DQL2E(HTR(1,N,NTR)+WTD(N),VTR(1),
     &                     HTR(2,N,NTR)+WTD(N),VTR(2))
              END DO
            END IF
          END IF
        ELSE
          IF(PR(4,2).GT.0.) THEN
            CALL DGLEVL(LCR)
            CALL DQPD0(8,SIZ+W2,0.)
            DO N=1,12
              DO K=0,20
                CALL DQPD(HP(K,N),VP(K,N))
              END DO
            END DO
          END IF
          IF(IHTR.GE.0) THEN
C           ................................. IHTR=-1 ONLY 1 TRACK FOR 'S6'
            CALL DGLEVL(LCO)
            CALL DQPD0(MS,SIZ,0.)
            DO N=1,12
              IF(PR(2,5).EQ.0) CALL DGLEVL(LTR(N))
              DO K=0,20
                CALL DQPD(HP(K,N),VP(K,N))
                IF(IHTR.EQ.1) THEN
                  CALL DQPOC(HP(K,N),VP(K,N),HPD,VPD,FIN)
                  IF(FIN) CALL DQPD_DC(HPD-DHP,VPD)
                END IF
              END DO
              IF(N.EQ.NTX) CALL DQPD0(MS,SIZ,0.)
            END DO
          END IF
          IF(NTX.GT.0.AND.NTX.LE.12) THEN
            IF(SX.NE.8.) THEN
              CALL DQPD0(8,SX,0.)
              CALL DGLEVL(LCA)
              DO K=0,20
                CALL DQPD(HP(K,NTX),VP(K,NTX))
              END DO
            END IF
            CALL DQPD0(NX,SX,0.)
            CALL DGLEVL(LTX)
            DO K=0,20
              CALL DQPD(HP(K,NTX),VP(K,NTX))
            END DO
          END IF
        END IF
      END DO
      VHGHDG(IAREDO)=VH
      VLOWDG(IAREDO)=VL
      HHGHDG(IAREDO)=HH
      HLOWDG(IAREDO)=HL
      IF(MCOL.NE.0) THEN
        CALL DGLEVL(MCOL)
        DLINDD=D2
        VD2(1)=VMINDG(IAREDO)
        VD2(2)=VHGHDG(IAREDO)
        DO L=1,3
          HD2(1)=HD(L)
          HD2(2)=HD(L)
          CALL DGDRAW(2,HD2,VD2)
        END DO
      END IF
      DLINDD=DLIN
      CALL DGEXEC
      END
*DK DBTTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTTR
CH
      SUBROUTINE DBTTR_COL(MO,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HTR(8,2),ICOL(8),LC(2),H0(2),V0(2)
c                1  2  3 4  5  6 7  8
      DATA ICOL/15,12,13,9,14,10,8,11/
      DATA HTR/119.,152.,191.,269.,286.,316.,350.,360,
     &         221.,131.,226.,260.,232.,330.,335.,377./
      DATA D1/1./,QH/496./,QW/0.1/,DFR/2./
      DLIN=DLINDD
      IF(MO.EQ.0) THEN
        PR(2,1)=3.
        PR(2,2)=1.
        PR(4,2)=-1.
        PR(2,3)=6.
        PR(2,6)=1.
        PR(2,7)=1.
        PR(2,16)=7777777.
        PR(2,17)=7777777.
        PR(2,18)=1.
        PR(2,19)=0.13
      END IF
      SIZ=PR(2,1)
      GNU=PR(2,3)
      NU=GNU
      LCA=PR(2,6)
      LC1=PR(2,16)
      LC2=PR(2,17)
      LIN=PR(2,18)
      DLI=PR(2,7)
      LCR4=PR(4,2)
      LCR2=PR(2,2)
      QW=PR(2,19)
      CALL DQFFWI(LCA)
      HL=HMINDG(IAREDO)
      HH=HHGHDG(IAREDO)
      VL=VMINDG(IAREDO)
      VH=VHGHDG(IAREDO)
      HM=0.5*(HH+HL)
      HD=0.5*(HH-HL)
      VM=VL+QW*(VH-VL)
      DV1=(VM-VL)/GNU
      DV2=(VH-VM)/GNU
      CALL DQLEVL(ICFRDD)
      DLINDD=D1
      DVM=DV2/4.
      CALL DQDRAW(HM,VL,HM,VH)
      CALL DQDRAW(HL,VM+DVM,HH,VM+DVM)
      DLINDD=DLI+DFR
      SIR=SIZ+DFR
      DCR=DFR
      CALL DGLEVL(LCR2)
      DO I=1,2
        IF(LCR4.EQ.1) THEN
          DO N=1,7
C           ...................... LC(LEFT OR RIGHT)
            LCC=LC1/10**(7-N)
            LC(1)=MOD(LCC,10)
            LCC=LC2/10**(7-N)
            LC(2)=MOD(LCC,10)
            HTR1=HTR(N,1)/QH
            HTR2=HTR(N,2)/QH
C           ...................... H0(LEFT OR RIGHT)
            H0(1)=HL+HTR1*HD
            H0(2)=HM+HTR1*HD
            DH=(HTR2-HTR1)*HD/GNU
            DO L=1,2                       ! LEFT and RIGHT
              IF(LC(L).GE.0) THEN
                IF(I.EQ.2) CALL DGLEVL(ICOL(LC(L)))
                V1=VL+0.5*DV1
                V2=VM+0.5*DV2
C               .............................. VO(UP or DOWN)
                V0(1)=V1
                V0(2)=V2
                H7=H0(L)
                DO K=1,NU
                  CALL DQFAR(H7-DCR,V1-DCR,H7+SIR,V1+SIR)
                  CALL DQFAR(H7-DCR,V2-DCR,H7+SIR,V2+SIR)
                  IF(K.LT.NU) THEN
                    H7=H7+DH
                    V1=V1+DV1
                    V2=V2+DV2
                  END IF
                END DO
                IF(LIN.EQ.3.OR.LIN.EQ.L) THEN
                  CALL DQDRAW(H0(L),V0(1),H7,V1)
                  CALL DQDRAW(H0(L),V0(2),H7,V2)
                END IF
              END IF
            END DO
          END DO
        END IF
        SIR=SIZ
        DCR=0.
      END DO
      DLINDD=DLIN
      CALL DQFR(IAREDO)
      END
*DK DBTPRS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTPRS
CH
      SUBROUTINE DBTPRS(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION X(5),Y(5),DX(2),XX(5,2),YY(5,2),FAC(2),HRB(4),VRB(4)
      DATA X/ 0., 0., 2., 2., 0./
      DATA Y/-1., 1., 1.,-1.,-1./
      DATA DX/0.,20./,FAC/1.,10./
      CHARACTER *4 T4
      DATA T4/' '/
      DATA LA/1/,LI/8/,DV/.1/,KEND/500/,DH/.1/
      DIMENSION DTY(4),LCOL(4),DY1(4)
      DATA LCOL/14,12,8,10/
      DATA DTX/.2/,DTY/.02,0.,-.03,.04/,DX1/.02/,DY1/0.,0.,0.,0.2/
      B=PR(2,16)
      T=PR(2,17)
      CALL DQCL(IAREDO)
      HI=DX(2)+FAC(2)*(X(3)-X(1))
      DO N=1,2
        DO K=1,5
          XS=FAC(N)*X(K)+DX(N)
          YS=FAC(N)*Y(K)
          XX(K,N)=XS*T /(HI*B+XS*(T-B))
          YY(K,N)=YS*HI/(HI*B+XS*(T-B))
        END DO
      END DO
      HRB(1)=YY(1,2)-DH
      HRB(3)=YY(2,2)+DH
      VRB(1)=XX(1,1)-DV
      VRB(3)=XX(3,2)+DV
      HRB(2)=HRB(3)
      HRB(4)=HRB(1)
      VRB(2)=VRB(1)
      VRB(4)=VRB(3)
      CALL DQRU(HRB,VRB)
      CALL DQPO0('AR+L',LA,LI,T4)
      CALL DQPOL(5,YY(1,1),XX(1,1))
      CALL DQPOL(5,YY(1,2),XX(1,2))
C//   CALL DQPD0(MSYMDL(MTPCDL,0),WDSNDL(2,4,MTPCDL),0.)
      CALL DPAR_SET_SY(62,0.)
      DO I=1,4
        XT=DX1
        YT=DY1(I)
        CALL DGLEVL(LCOL(I))
        N=1
        DO K=1,KEND
          IF(XT.LE.X(3).OR.XT.GE.DX(2)) THEN
            XXT=XT*T /(HI*B+XT*(T-B))
            YYT=YT*HI/(HI*B+XT*(T-B))
            CALL DQPD(YYT,XXT)
          END IF
          IF(XT.GE.DX(2)) N=2
          XT=XT+DTX   *FAC(N)
          IF(XT.GT.HI) GO TO 3
          YT=YT+DTY(I)*FAC(N)
        END DO
    3 END DO
      CALL DQFR(IAREDO)
      END
*DK DBTCUB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTCUB
CH
      SUBROUTINE DBTCUB(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION XX(8),YY(8),ZZ(8),X0(2),Y0(2),Z0(2),SC(2)
      DATA XX/-1., 1., 1.,-1.,-1., 1., 1.,-1./
      DATA YY/-1.,-1., 1., 1.,-1.,-1., 1., 1./
      DATA ZZ/-1.,-1.,-1.,-1., 1., 1., 1., 1./
      DATA X0/-2.,5./,Y0/0.,-3./,Z0/0.,-3./, SC/4.,1./
      DIMENSION XA(8,2),YA(8,2),ZA(8,2)
      DIMENSION XB(8,2),YB(8,2),ZB(8,2),ZE(8,2)
      DIMENSION                 ZC(8,2)
      DIMENSION XD(8,2),YD(8,2),ZD(8,2)
      DATA ZE/0.,0.,5.,9.,9.,-8.,20.,-5.,8*3./,DM/-4./,DK/-5./,DJ/4./
      DIMENSION N1(12),N2(12)
      DATA N1/1,2,3,4,5,6,7,8,1,2,3,4/
      DATA N2/2,3,4,1,6,7,8,5,5,6,7,8/
      DIMENSION HRB(4),VRB(4)
      DATA DRU/1./,FI/90./,TE/70./,AL/160./,NM/3/,NK/7/,NJ/6/
      DATA DR0/9.121789/
      DATA Y3/0./
      DATA IDEB/0/
C     FOR P1=15 DEG. AND DRU=0.
C     .................................................... X2= CF*X1+SF*Y1
C     .................................................... Y2=-X1*SF+CF*Y1
C     .................................................... Z2= Z1
C     .................................................... X3= CT*X2+ST*Z2
C     .................................................... Y3= Y2
C     .................................................... Z3=-ST*X2+CT*Z2
C     ...................................................H=X4= X3
C     .................................................. V=Y4= CG*Y3+SG*Z3
C     .................................................... Z4=-SG*Y3+CG*Z3
      CALL DQCL(IAREDO)
      DO L=1,2
        DO K=1,8
          XA(K,L)=X0(L)+SC(L)*XX(K)
          YA(K,L)=Y0(L)+SC(L)*YY(K)
          ZA(K,L)=Z0(L)+SC(L)*ZZ(K)
        END DO
      END DO
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_RAL'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_RAL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IDEB.EQ.1) THEN
        SF=SIND(PARADA(2,J_PFI))
        CF=COSD(PARADA(2,J_PFI))
        ST=SIND(PARADA(2,J_PTE))
        CT=COSD(PARADA(2,J_PTE))
        SG=SIND(PARADA(2,J_RAL))
        CG=COSD(PARADA(2,J_RAL))
      ELSE
        SF=SIND(FI)
        CF=COSD(FI)
        ST=SIND(TE)
        CT=COSD(TE)
        SG=SIND(AL)
        CG=COSD(AL)
      END IF
      DO L=1,2
        DO K=1,8
          X1=XA(K,L)
          Y1=YA(K,L)
          Z1=ZA(K,L)
          X2= CF*X1+SF*Y1
          Y2=-SF*X1+CF*Y1
          Z3=-ST*X2+CT*Z1
          X4= CT*X2+ST*Z1
          Y4= CG*Y2+SG*Z3
          Z4=-SG*Y3+CG*Z3
          XB(K,L)=X4
          YB(K,L)=Y4
          ZB(K,L)=Z4
          ZC(K,L)=ZB(K,L)+ZE(K,L)
        END DO
      END DO
      M1=N1(NM)
      M2=N2(NM)
      XMC=0.5*(XB(M1,1)+XB(M2,1))
      YMC=0.5*(YB(M1,1)+YB(M2,1))
      ZMC=0.5*(ZC(M1,1)+ZC(M2,1))
      ZMC1=ZMC+DM
      ZMC2=ZMC-DM
      K1=N1(NK)
      K2=N2(NK)
      XKC=0.5*(XB(K1,1)+XB(K2,1))
      YKC=0.5*(YB(K1,1)+YB(K2,1))
      ZKC=0.5*(ZC(K1,1)+ZC(K2,1))
      ZKC1=ZKC+DK
      J1=N1(NJ)
      XJC=XB(J1,1)
      YJC=YB(J1,1)
      ZJC=ZC(J1,1)+DJ
      SA=SIND(PR(2,16))
      CA=COSD(PR(2,16))
      IF(SA.EQ.0.) CALL DWRT('P1=15')
      HMIN= 999.
      HMAX=-999.
      VMIN= 999.
      VMAX=-999.
      DO L=1,2
        DO K=1,8
          X1=XB(K,L)
          Y1=YB(K,L)
          Z1=ZC(K,L)
          X2= CA*X1+SA*Z1
          Z2=-SA*X1+CA*Z1
          Y2=Y1
          XD(K,L)=X2
          YD(K,L)=Y2
          ZD(K,L)=Z2
          HMIN=MIN(HMIN,X2)
          HMAX=MAX(HMAX,X2)
          VMIN=MIN(VMIN,Y2)
          VMAX=MAX(VMAX,Y2)
        END DO
      END DO
      YMD=YMC
      YKD=YKC
      YJD=YJC
      XMD1= CA*XMC+SA*ZMC1
      ZMD1=-SA*XMC+CA*ZMC1
      XMD2= CA*XMC+SA*ZMC2
      ZMD2=-SA*XMC+CA*ZMC2
      XKD1= CA*XKC+SA*ZKC1
      ZKD1=-SA*XKC+CA*ZKC1
      XJD1= CA*XJC+SA*ZJC
      ZJD1=-SA*XJC+CA*ZJC
      HM=0.5*(HMIN+HMAX)
      VM=0.5*(VMIN+VMAX)
      IF(PR(2,16).NE.0.) THEN
        DR=0.5*MAX(HMAX-HMIN,VMAX-VMIN)+DRU
      ELSE
        DR=DR0
      END IF
      CALL DQRER(0,HM-DR,VM-DR,HM+DR,VM+DR,HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DGLEVL(IFIX(PR(2,5)))
      DLIN=DLINDD
      DLINDD=PR(2,1)
      DO L=1,2
        DO N=1,12
          IF(     L.EQ.1.AND.N.EQ.NM) THEN
            CALL DQL2E(XD(N1(N),L),YD(N1(N),L),     XMD1,YMD)
            CALL DQL2E(     XMD2,YMD,          XD(N2(N),L),YD(N2(N),L))
          ELSE IF(L.EQ.1.AND.N.EQ.NK) THEN
            CALL DQL2E(XD(N1(N),L),YD(N1(N),L),     XKD1,YKD)
            CALL DQL2E(     XKD1,YKD,          XD(N2(N),L),YD(N2(N),L))
          ELSE IF(L.EQ.1.AND.N.EQ.NJ) THEN
            CALL DQL2E(     XJD1,YJD,          XD(N2(N),L),YD(N2(N),L))
          ELSE
            CALL DQL2E(XD(N1(N),L),YD(N1(N),L),XD(N2(N),L),YD(N2(N),L))
          END IF
        END DO
      END DO
      DLINDD=DLIN
      CALL DQFR(IAREDO)
      END
*DK DBT3D
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT3D
CH
      SUBROUTINE DBT3D(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DATA CMPIX/32./
      IF(PR(4,2).GE.0) THEN
        IAREDO=7
      ELSE
        IAREDO=8
      END IF
      LBG=PR(2,6)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      DLINDD=PR(2,1)
C      DFWI0=DFWIDU(0)
C      DFWI1=DFWIDU(1)
C      DFWIDU(0)=0.
C      DFWIDU(1)=0.
      HLOW=HLOWDG(IAREDO)
      HHGH=HHGHDG(IAREDO)
      VLOW=VLOWDG(IAREDO)
      VHGH=VHGHDG(IAREDO)
      HSID=HHGHDG(1)
      VMID=0.5*(VLOW+VHGH)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_REY'
      CALL DPARAM(15
     &  ,J_REY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      E=PARADA(2,J_REY)
      EC=E*CMPIX
      E2=EC*2
      HLOWDG(IAREDO)=HSID-E2
      HHGHDG(IAREDO)=HSID
      VLOWDG(IAREDO)=VMID-EC
      VHGHDG(IAREDO)=VMID+EC
      PARADA(2,J_REY)=-E
      CALL DBT3DD(PR,1)
      HLOWDG(IAREDO)=HSID
      HHGHDG(IAREDO)=HSID+E2
      PARADA(2,J_REY)= E
      CALL DBT3DD(PR,2)
      HLOWDG(IAREDO)=HLOW
      HHGHDG(IAREDO)=HHGH
      VLOWDG(IAREDO)=VLOW
      VHGHDG(IAREDO)=VHGH
C      DFWIDU(0)=DFWI0
C      DFWIDU(1)=DFWI1
      DLINDD=PDCODD(2,LITRDD)
      IF(IAREDO.EQ.7) THEN
      END IF
      CALL DGEXEC
      END
*DK DBT3DD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT3DD
CH
      SUBROUTINE DBT3DD(PR,NSID)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION X(5,2),Y(5,2),Z(5,2),XX(2),YY(2),ZZ(2)
      DATA X/-1., 1., 1.,-1.,-1.,   -1., 1., 1.,-1.,-1./
      DATA Y/-1.,-1., 1., 1.,-1.,   -1.,-1., 1., 1.,-1./
      DATA Z/-1.,-1.,-1.,-1.,-1.,    1., 1., 1., 1., 1./
      DIMENSION X0(5),Y0(5),Z0(5),SL(5),LC(5),LW(5)
      DATA X0/ 0., 2., 7.,-5.,-8./
      DATA Y0/ 0.,-6., 3.,-1., 7./
      DATA Z0/ 0.,-7.,-7., 8.,-20./
      DATA SL/ 9., 1., 1., 1., 1./
      DATA LW/12 , 9 ,14 ,10 , 8 /
      DIMENSION XA0(2),YA0(2),ZA0(2),SLA(2),XXA(4),YYA(4)
      DATA XA0/-5., 5./
      DATA YA0/ 5.,-5./
      DATA ZA0/-5., 5./
      DATA SLA/ 2., 2./
      DIMENSION HRB(4),VRB(4)
      DATA HRB/-16., 16., 16.,-16./
      DATA VRB/-16.,-16., 16., 16./
      N5=MIN(PR(2,17),5.)
      LC(1)=PR(2,5)
      DO L=2,5
        LC(L)=PR(2,2)
      END DO
      CALL DQRU(HRB,VRB)
      CALL DBT3D0
      DO N=1,N5
        IF(NSID.EQ.2.AND.PR(4,2).LT.0) THEN
          CALL DGLEVL(LW(N))
        ELSE
          CALL DGLEVL(LC(N))
        END IF
        DO L=1,2
          DO K=1,4
            XX(1)=X0(N)+SL(N)*X(K  ,L)
            XX(2)=X0(N)+SL(N)*X(K+1,L)
            YY(1)=Y0(N)+SL(N)*Y(K  ,L)
            YY(2)=Y0(N)+SL(N)*Y(K+1,L)
            ZZ(1)=Z0(N)+SL(N)*Z(K  ,L)
            ZZ(2)=Z0(N)+SL(N)*Z(K+1,L)
            CALL DBT3DE(XX,YY,ZZ)
          END DO
        END DO
        DO J=1,4
          XX(1)=X0(N)+SL(N)*X(J,1)
          XX(2)=X0(N)+SL(N)*X(J,2)
          YY(1)=Y0(N)+SL(N)*Y(J,1)
          YY(2)=Y0(N)+SL(N)*Y(J,2)
          ZZ(1)=Z0(N)+SL(N)*Z(J,1)
          ZZ(2)=Z0(N)+SL(N)*Z(J,2)
          CALL DBT3DE(XX,YY,ZZ)
        END DO
      END DO
      IF(N5.EQ.1) THEN
        CALL DGLEVL(LC(2))
        DO N=1,2
          DO K=1,4
            XXA(K)=XA0(N)+SLA(N)*X(K,1)
            YYA(K)=YA0(N)+SLA(N)*Y(K,1)
          END DO
          CALL DBT3DA(XXA,YYA,ZA0(N))
        END DO
      END IF
      END
*DK DBT3DE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT3DE
CH
      SUBROUTINE DBT3DE(XX,YY,ZZ)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION XX(2),YY(2),ZZ(2),HH(4),VV(4),XA(4),YA(4)
      LOGICAL FIN
      DATA CMPIX/32./
      DATA LDEB/0/
C     .................................................... X2= CF*X1+SF*Y1
C     .................................................... Y2=-X1*SF+CF*Y1
C     .................................................... Z2= Z1
C     .................................................... X3= CT*X2+ST*Z2
C     .................................................... Y3= Y2
C     .................................................... Z3=-ST*X2+CT*Z2
C     ...................................................H=X4= X3
C     .................................................. V=Y4= CG*Y3+SG*Z3
C     .................................................... Z4=-SG*Y3+CG*Z3
      DO K=1,2
        X1=XX(K)
        Y1=YY(K)
        Z1=ZZ(K)
        X2= CF*X1+SF*Y1
        Y2=-SF*X1+CF*Y1
        H = CT*X2+ST*Z1
        Z3=-ST*X2+CT*Z1
        IF(LDEB.EQ.1) Z3=DIR*Z3
        V = CG*Y2+SG*Z3
        W=-SG*Y2+CG*Z3
        ESW=ES+W
        HH(K)=H+(EY-H)*W/ESW
        VV(K)=V*ES/ESW
      END DO
      CALL DQLIE(HH,VV)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------       US
CH
      ENTRY DBT3DA(XA,YA,ZA)
      DO K=1,4
        H=XA(K)
        V=YA(K)
        Z3=DIR*ZA
        W=-SG*Y2+CG*Z3
        ESW=ES+W
        HU=H+(EY-H)*W/ESW
        VU=V*ES/ESW
        CALL DQPOC(HU,VU,HH(K),VV(K),FIN)
      END DO
      CALL DGAREA(4,HH,VV)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------       US
CH
      ENTRY DBT3DZ(DIR0)
      IF(DIR0.EQ.0.) THEN
        DIR=-DIR
      ELSE
        DIR=DIR0
      END IF
      RETURN
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------       US
CH
      ENTRY DBT3D0
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_RAL,J_RES,J_REY'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_RAL,J_RES,J_REY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      SF=SIND(PARADA(2,J_PFI))
      CF=COSD(PARADA(2,J_PFI))
      ST=SIND(90.-PARADA(2,J_PTE))
      CT=COSD(90.-PARADA(2,J_PTE))
      SG=SIND(PARADA(2,J_RAL))
      CG=COSD(PARADA(2,J_RAL))
      ES= PARADA(2,J_RES)*CMPIX/AHSCDQ
      EY=-PARADA(2,J_REY)*CMPIX/AHSCDQ
      END
*DK DBT3DC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT3DC
CH
      SUBROUTINE DBT3DC(PR,DIR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      CHARACTER *49 T
      L=PR(2,2)
      DC=DIR*PR(2,16)*0.01
      IF(RDCODD(L).NE.0.) RDCODD(L)=MAX(0.,MIN(1.,RDCODD(L)+DC))
      IF(GRCODD(L).NE.0.) GRCODD(L)=MAX(0.,MIN(1.,GRCODD(L)+DC))
      IF(BLCODD(L).NE.0.) BLCODD(L)=MAX(0.,MIN(1.,BLCODD(L)+DC))
      CALL DW_SET_CO
      WRITE(T,1000) RDCODD(L),GRCODD(L),BLCODD(L)
 1000 FORMAT(' R=',F4.2,' G',F4.2,' B=',F4.2)
      CALL DWRT(T)
      END
*DK DBTEXP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTEXP
CH
      SUBROUTINE DBTEXP(PR,IACT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4,2)
      DATA HRB/0.,3.,3.,0./
      DATA VRB/-30.,-30.,360.,360.,  -0.09,-0.09,3.,3./
      DATA HS1/2.93/,HS2/3./,HT/2.65/,VT/0./
      DATA DX/.02/,XK/1.5/,QE/0.8/,XE/2.7/,DYS/1./
      CHARACTER *3 TS(0:2)
      DATA TS/'  1',' 10','100'/
      CALL DQWIL(1.)
      LBG=PR(2,6)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      CALL DQRU(HRB,VRB(1,IACT))
      CALL DGLEVL(IFIX(PR(2,5)))
      X=0.3
      Y=10.**X
      IF(IACT.EQ.1) THEN
        H1=X
        V1=Y
      ELSE
        DLINDD=1.
        DO M=0,2
          YM=10**M
          GM=M
          CALL DQTXT(HT,GM+VT,TS(M),3)
          DO YY=1.,9.5,DYS
            YS=YY*YM
            VS1=ALOG10(YS)
            VS2=VS1
            CALL DQL2E(HS1,VS1,HS2,VS2)
          END DO
        END DO
        H1=X
        V1=X
      END IF
      DLINDD=PR(2,1)
      CALL DQLEVL(LCTCDD)
      DO K=1,1000
        X=X+DX
        Y=10.**X
        IF(IACT.EQ.1) THEN
          H2=X
          V2=Y
        ELSE
          H2=X
          V2=X
        END IF
        CALL DQL2E(H1,V1,H2,V2)
        H1=H2
        V1=V2
        IF(X.GT.XK) GO TO 5
      END DO
    5 CALL DGLEVL(IFIX(PR(2,5)))
      QQ=(10.**X)/(10.**(QE*X))
      DO K=1,1000
        X=X+DX
        Y=QQ*10.**(QE*X)
        IF(IACT.EQ.1) THEN
          H2=X
          V2=Y
        ELSE
          H2=X
          V2=ALOG10(Y)
        END IF
        CALL DQL2E(H1,V1,H2,V2)
        H1=H2
        V1=V2
        IF(X.GT.XE) GO TO 6
      END DO
    6 DLINDD=PDCODD(2,LITRDD)
      IF(IACT.EQ.1) THEN
        CALL DQSCA('V',VRB(2,IACT),VRB(3,IACT),' ',1,'Y',1)
      ELSE
        CALL DQSCA('V',VRB(2,IACT),VRB(3,IACT),' ',1,'LOG Y',5)
      END IF
      CALL DQSCA('H',HRB(1),HRB(2),' ',1,'X',1)
      CALL DQFR(IAREDO)
      CALL DQWIL(0.)
      END
*DK DBTCIR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTCIR
CH
      SUBROUTINE DBTCIR(PR,IACT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(5),VRB(4),VRS(5)
      DATA XC/-20./,YC/130./,QRH/0.2/,QRV/0.4/,DVSK/5./
      DATA DF/3./,KK/10/,KE/20/,QC/.8/,FK/1./,QSK/2./
      DIMENSION HH(20),VV(20)
      CALL DQWIL(1.)
      LBG=PR(2,6)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      RC=SQRT(XC**2+YC**2)
      FI=DATN2D(YC,XC)-180.+FK*DF
      DO K=1,KK
        FI=FI+DF
        X=XC+RC*COSD(FI)
        Y=YC+RC*SIND(FI)
        IF(IACT.EQ.1) THEN
          HH(K)=X
          VV(K)=Y
        ELSE
          HH(K)=SQRT(X**2+Y**2)
          VV(K)=DATN2D(Y,X)
        END IF
      END DO
      DFK=PR(2,20)
      QC =PR(2,19)
      X1=XC-X
      Y1=YC-Y
      X2= COSD(DFK)*X1+SIND(DFK)*Y1
      Y2=-SIND(DFK)*X1+COSD(DFK)*Y1
      FI=FI-DFK
      XD=X+X2*QC
      YD=Y+Y2*QC
      RD=RC*QC
      DO K=KK+1,KE
        FI=FI+DF
        X=XD+RD*COSD(FI)
        Y=YD+RD*SIND(FI)
        IF(IACT.EQ.1) THEN
          HH(K)=X
          VV(K)=Y
        ELSE
          HH(K)=SQRT(X**2+Y**2)
          VV(K)=DATN2D(Y,X)
        END IF
      END DO
      DHH=HH(KE)-HH(1)
      DHR=QRH*DHH
      IF(IACT.EQ.4) THEN
        DHS=QSK*DHH
      ELSE
        DHS=DHR
      END IF
      HRB(1)=HH( 1)-DHR
      HRB(3)=HH(KE)+DHS
      HRB(2)=HRB(3)
      HRB(4)=HRB(1)
      QQQ=(VV(KE)-VV(1))/DHH
      V0=(HRB(1)-HH( 1))*QQQ+VV(1)
      V3=(HRB(3)-HH( 1))*QQQ+VV(1)
      VRS(1)=V0-DVSK
      VRS(2)=V3-DVSK
      VRS(3)=V3+DVSK
      VRS(4)=V0+DVSK
      IF(IACT.LT.3) THEN
        DVR=QRV*(VV(KE)-VV(1))
        IF(IACT.EQ.1) THEN
          VRB(1)=VV( 1)-DHR
          VRB(3)=VRB(1)+HRB(3)-HRB(1)
        ELSE
          VRB(1)=VV( 1)-DVR
          VRB(3)=VV(KE)+DVR
        END IF
        VRB(2)=VRB(1)
        VRB(4)=VRB(3)
      ELSE
        CALL UCOPY(VRS,VRB,4)
      END IF
      CALL DQRU(HRB,VRB)
      IF(IACT.EQ.2.AND.PR(4,2).GT.0.) THEN
        CALL DGLEVL(IFIX(PR(2,2)))
        VRS(5)=VRS(1)
        HRB(5)=HRB(1)
        DO L=1,4
          CALL DQLIE(HRB(L),VRS(L))
        END DO
      END IF
      DLINDD=PR(2,1)
      IF(PR(2,18).GE.2.) THEN
        CALL DQLEVL(LCTCDD)
        DO K=1,KK-1
          CALL DQLIE(HH(K),VV(K))
        END DO
        CALL DGLEVL(IFIX(PR(2,5)))
        DO K=KK+1,KE-1
          CALL DQLIE(HH(K),VV(K))
        END DO
      END IF
      IF(PR(2,18).LE.2.) THEN
C//     CALL DQPD0(MSYMDL(MTPCDL,0),WDSNDL(2,4,MTPCDL),0.)
        CALL DPAR_SET_SY(62,0.)
        CALL DQLEVL(LCTCDD)
        DO K=1,KK
          CALL DQPD(HH(K),VV(K))
        END DO
        CALL DGLEVL(IFIX(PR(2,5)))
        DO K=KK+1,KE
          CALL DQPD(HH(K),VV(K))
        END DO
      END IF
      IF(IACT.EQ.1) THEN
        CALL DQSCA('H',HRB(1),HRB(2),' ',1,'X',1)
        CALL DQSCA('V',VRB(1),VRB(4),' ',1,'Y',1)
      ELSE
        CALL DQSCA('H',HRB(1),HRB(2),' ',1,'&r',2)
        CALL DQSCA('V',VRB(1),VRB(4),' deg',4,'&f',2)
      END IF
      CALL DQFR(IAREDO)
      CALL DQWIL(0.)
      END
*DK DBT3DP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT3DP
CH
      SUBROUTINE DBT3DP(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*),HRB(4),VRB(4)
      DATA HRB/0.,12.,12.,0./
      DATA VRB/0.,0.,12.,12./
      DIMENSION HA(4,3),VA(4,3),HP(3),VP(3)
      DATA HA/ 5.,10., 9.7, 9.7,
     &         5., 5., 4.7, 5.3,
     &         5., 2., 2.5, 2. /
      DATA VA/ 5., 5., 4.7, 5.3,
     &         5.,10., 9.7, 9.7,
     &         5., 2., 2. , 2.5/
      DATA H0/7./,V0/3./
      DATA HP/7.,9.,3./VP/8.,5.,3./
      DIMENSION IP(2)
      DATA IP/4,4/
      LBG=PR(2,6)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      CALL DQRU(HRB,VRB)
      CALL DGLEVL(IFIX(PR(2,5)))
      DLINDD=PR(2,1)
C//   CALL DQPD0(MSYMDL(MTPCDL,0),WDSNDL(2,4,MTPCDL),0.)
      CALL DPAR_SET_SY(62,0.)
      DO K=1,3
        CALL DQL2E(HA(1,K),VA(1,K),HA(2,K),VA(2,K))
        CALL DQL2E(HA(3,K),VA(3,K),HA(2,K),VA(2,K))
        CALL DQL2E(HA(4,K),VA(4,K),HA(2,K),VA(2,K))
      END DO
      DO I=1,3
        CALL DQL2E(H0,V0,HP(I),VP(I))
        CALL DGDASH(2,IP)
      END DO
      CALL DGDASH(0,0)
      CALL DQPD(HP(1),VP(1))
      CALL DQPD(H0,V0)
      DLINDD=PDCODD(2,LITRDD)
      CALL DQFR(IAREDO)
      END
*DK DBTCC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTCC
CH
      SUBROUTINE DBTCC(IFROM,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     FAST COLOR CHANGE
      DIMENSION PR(4,*)
      CHARACTER *1 TA
      DATA RJ/10./
C     ............................... COLOR TO BE VARIED MUST BE IN PR(2,5)
      CALL DBTMCC(PR,'?')
  927 CALL DGETLN(TA,NA,1)
      IF(NA.LT.1) RETURN
      IF(IFROM.EQ.1.AND.TA.EQ.' ') THEN
        IF(PR(2,17).LT.RJ) CALL DBTM(PR)
      ELSE
        CALL DBTMCC(PR,TA)
      END IF
      GO TO 927
      END
*DK DBTM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTM
CH
      SUBROUTINE DBTM(PR)
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
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      COMMON /DBTMC/ IRAN,D,HP(17),VP(17)
      DIMENSION H(5,17),V(5,17),IH(5,17),IV(5,17),NDSEG(2,17)
      DIMENSION HR(5),VR(5)
      DATA IRAN/123913/,NUMPL/17/
      DIMENSION LRB(2),RD(2),GR(2),BL(2)
      DATA LSY/12/,LBG/1/,LRB/115,126/
      DATA H5,V5/825.,495./,RJ/10./,N5/-5/
      EXTERNAL DBTMM
C     WINKEL         = RANDOM
C     BACKGROUND     = COLOR 1
C     POINT COLOR    = COLOR 12
C     POINTSIZE      = SZ = 1
C     POINT DISTANCE = P2 = 17 = RADIUS TO NEXT POINT
C     # OF RANDOM P. = NU = 3
      KEND=PR(2,3)
      D=0.5*PR(2,1)
      Q=RAN(IRAN)
      RR=PR(2,17)
      DO K=1,2
        RD(K)=RDCODD(LRB(K))
        GR(K)=GRCODD(LRB(K))
        BL(K)=BLCODD(LRB(K))
      END DO
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      CALL DGLEVL(LSY)
      H1=HMINDG(IAREDO)
      V1=VMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      DH=H2-H1
      DV=V2-V1
      IF(RR.LT.RJ) THEN
        H0=H1+RR+RAN(IRAN)*(DH-2.*RR)
        V0=V1+RR+RAN(IRAN)*(DV-2.*RR)
      ELSE
        H0=0.5*(H1+H2)
        V0=0.5*(V1+V2)
      END IF
      K=0
      N=1
      DO HK=-1.,2.
        K=K+1
        NDSEG(1,K)=N
        N=N+5
        NDSEG(2,K)=N5
        HP(K)=HK*RR
        VP(K)=-2.*RR
        CALL DBTMSQ(H0+HP(K),V0+VP(K),D,H(1,K),V(1,K))
      END DO
      DO VK=-1.,2.
        K=K+1
        NDSEG(1,K)=N
        N=N+5
        NDSEG(2,K)=N5
        HP(K)=2.*RR
        VP(K)=VK*RR
        CALL DBTMSQ(H0+HP(K),V0+VP(K),D,H(1,K),V(1,K))
      END DO
      DO HK=1.,-2.,-1.
        K=K+1
        NDSEG(1,K)=N
        N=N+5
        NDSEG(2,K)=N5
        HP(K)=HK*RR
        VP(K)=2.*RR
        CALL DBTMSQ(H0+HP(K),V0+VP(K),D,H(1,K),V(1,K))
      END DO
      DO VK=1.,-2.,-1.
        K=K+1
        NDSEG(1,K)=N
        N=N+5
        NDSEG(2,K)=N5
        HP(K)=-2.*RR
        VP(K)=VK*RR
        CALL DBTMSQ(H0+HP(K),V0+VP(K),D,H(1,K),V(1,K))
      END DO
      IF(RR.LT.RJ) THEN
        DO K=1,16
          IF(N5.GT.0) THEN
            CALL DGDRAW(5,H(1,K),V(1,K))
          ELSE
            CALL DGAREA(5,H(1,K),V(1,K))
          END IF
        END DO
      END IF
      K=K+1
      HP(K)=H5-H0
      VP(K)=V5-V0
      CALL DBTMSQ(H5,V5,D,H(1,K),V(1,K))
      NDSEG(1,K)=N
      NDSEG(2,K)=N5
      DO K=1,KEND
        Q=RAN(IRAN)
        HS=H1+Q*DH
        Q=RAN(IRAN)
        VS=V1+Q*DV
        CALL DBTMSQ(HS,VS,D,HR,VR)
        IF(N5.GT.0) THEN
          CALL DGDRAW(5,HR,VR)
        ELSE
          CALL DGAREA(5,HR,VR)
        END IF
      END DO
      CALL DGCHKX
      CALL DGEXEC
      IF(RR.LT.RJ) RETURN
      IHC=H0
      IVC=V0
      CALL DBTMM0(H0,V0,LBG,LSY,LRB)
      CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DBTMM,.FALSE.,NDSEG,NUMPL)
      DO K=1,2
        RDCODD(LRB(K))=RD(K)
        GRCODD(LRB(K))=GR(K)
        BLCODD(LRB(K))=BL(K)
      END DO
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTMM
CH
      SUBROUTINE DBTMM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      COMMON /DBTMC/ IRAN,D,HP(17),VP(17)
      DIMENSION H(*),V(*),NDSEG(2,*),IRB(*)
      DIMENSION PR(4,*)
      CHARACTER *(*) TKBD
      CHARACTER *1 TUD
      DATA TUD/''''/
      CHARACTER *2 TLOST(8)
      DATA TLOST/'dl','dr','rd','ru','ur','ul','lu','ld'/
      CHARACTER *49 TS
      DATA TS/'Symbols (color 12):       Display b.gr. (color 1)'/
      CHARACTER *1 TR(2),TG(2),TB(2)
      DATA TR/'r','R'/,TG/'g','G'/,TB/'b','B'/
      DIMENSION RCOL(7),GCOL(7),BCOL(7)
      CHARACTER *1 TCOL(7),TVAL(0:10),TDIR(-1:2),TSD(2),TSL(0:1)
      DATA TCOL/'R','G','B','Y','C','M','W'/
      DATA RCOL/ 1 , 0 , 0 , 1 , 0 , 1 , 1 /
      DATA GCOL/ 0 , 1 , 0 , 1 , 1 , 0 , 1 /
      DATA BCOL/ 0 , 0 , 1 , 0 , 1 , 1 , 1 /
      DATA TVAL/'`','1','2','3','4','5','6','7','8','9','0'/
      DATA TDIR/'-','_','+','='/,TSD/'S','D'/,TSL/':','='/
      DIMENSION MSD(2),LRB(2)
      DATA ISD/2/,LSEL/7/,DANG/1./,QA/.1/,DC/.01/
      LOGICAL FBUT(4),FUP,FMM
      DATA FUP/.TRUE./
      DATA P0/0./
      IF(.NOT.FBUT(4)) THEN
        FUP=.TRUE.
        FMM=.TRUE.
        LSD=MSD(ISD)
        GO TO 100
      END IF
      IF(FUP.AND.(.NOT.FBUT(1))) THEN
        FUP=.FALSE.
        ANG=RAN(IRAN)*90.
        GO TO 10
      END IF
      IF(.NOT.FBUT(1)) THEN
        ANG=QA*(VC-V0)
        GO TO 10
      END IF
      IF(.NOT.FBUT(2)) THEN
        IF(FUP) THEN
          FUP=.FALSE.
          IHC=H00
          IVC=V00
        END IF
        RETURN
      END IF
      FUP=.TRUE.
      DANG=-DANG
      H0=IHC
      V0=IVC
      N=1
      DO K=1,17
        IF(K.EQ.LOST2) THEN
          CALL DBTMSQ(P0+HP(K),P0+VP(K),D,H(N),V(N))
        ELSE
          CALL DBTMSQ(H0+HP(K),V0+VP(K),D,H(N),V(N))
        END IF
        N=N+5
      END DO
      RETURN

   10 HC=IHC
      VC=IVC
      SF=SIND(ANG)
      CF=COSD(ANG)
C     ..................................... LAST POINT IS NOT ROTATED
      N=1
      DO K=1,16
        HH=HP(K)
        VV=VP(K)
        HP(K)=CF*HH-SF*VV
        VP(K)=SF*HH+CF*VV
        CALL DBTMSQ(H0+HP(K),V0+VP(K),D,H(N),V(N))
        N=N+5
      END DO
      IHC=H0
      IVC=V0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTMCC
CH
      ENTRY DBTMCC(PR,TKBD)
CH
CH --------------------------------------------------------------------
      FMM=.FALSE.
      LSD=PR(2,5)
  100 LOST=1+RAN(IRAN)*8.
      LOST2=LOST*2-1
      DO K=-1,2
        IF(TKBD.EQ.TDIR(K)) THEN
          IF(K.GT.0) THEN
            DCS=DC
          ELSE
            DCS=-DC
          END IF
          RDCODD(LSD)=MAX(0.,MIN(1.,RDCODD(LSD)+DCS*RCOL(LSEL)))
          GRCODD(LSD)=MAX(0.,MIN(1.,GRCODD(LSD)+DCS*GCOL(LSEL)))
          BLCODD(LSD)=MAX(0.,MIN(1.,BLCODD(LSD)+DCS*BCOL(LSEL)))
          CALL DGSETC
          GO TO 2
        END IF
      END DO
      DO K=0,10
        IF(TKBD.EQ.TVAL(K)) THEN
          DVAL=0.1*FLOAT(K)
          IF(RCOL(LSEL).NE.0.) RDCODD(LSD)=DVAL
          IF(GCOL(LSEL).NE.0.) GRCODD(LSD)=DVAL
          IF(BCOL(LSEL).NE.0.) BLCODD(LSD)=DVAL
          IF(FMM) GO TO 3
          CALL DGSETC
          GO TO 2
        END IF
      END DO
      DO K=1,7
        IF(TKBD.EQ.TCOL(K)) THEN
          LSEL=K
          GO TO 2
        END IF
      END DO
      DO K=1,2
        IF(TKBD.EQ.TSD (K)) THEN
          ISD=K
          CALL DWRT(TS)
          CALL DWRT('move mouse.')
          GO TO 2
        END IF
      END DO
      IF(TKBD.EQ.'Z') THEN
        RDCODD(LSD)=0.
        GRCODD(LSD)=0.
        BLCODD(LSD)=0.
        IF(FMM) GO TO 3
        CALL DGSETC
        GO TO 2
      END IF
      IF(TKBD.EQ.'?') THEN
        CALL DWRT(TS)
        CALL DWRT('move mouse.')
      END IF
      GO TO 2
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTMM0
CH
      ENTRY DBTMM0(HIN,VIN,IBG,ISY,IRB)
CH
CH --------------------------------------------------------------------
      H0=HIN
      V0=VIN
      H00=H0
      V00=V0
      MSD(1)=ISY
      MSD(2)=IBG
      LRB(1)=IRB(1)
      LRB(2)=IRB(2)
      CALL DWRT(TS)
      CALL DWRT('move mouse.')
      LOST=1
    3 DO L=1,2
        RDCODD(LRB(L))=RDCODD(MSD(1))
        GRCODD(LRB(L))=GRCODD(MSD(1))
        BLCODD(LRB(L))=BLCODD(MSD(1))
      END DO
      CALL DGSETC
    2 IF(FMM) THEN
        IF(ISD.EQ.1) THEN
          IS=2
          ID=1
        ELSE
          IS=1
          ID=2
        END IF
        WRITE(TXTADW,1000)
     &    TR(IS),TSL(IFIX(RCOL(LSEL))),RDCODD(MSD(1)),
     &    TG(IS),TSL(IFIX(GCOL(LSEL))),GRCODD(MSD(1)),
     &    TB(IS),TSL(IFIX(BCOL(LSEL))),BLCODD(MSD(1)),
     &    TR(ID),TSL(IFIX(RCOL(LSEL))),RDCODD(MSD(2)),
     &    TG(ID),TSL(IFIX(GCOL(LSEL))),GRCODD(MSD(2)),
     &    TB(ID),TSL(IFIX(BCOL(LSEL))),BLCODD(MSD(2)),
     &    TSD(ISD),TCOL(LSEL),TLOST(LOST),TUD
 1000   FORMAT(6(2A,F4.2,1X),A,1X,A,1X,2A)
        CALL DWR_OVER_PRINT(49)
      ELSE
        WRITE(TXTADW,1001)
     &    'R',TSL(IFIX(RCOL(LSEL))),RDCODD(LSD),
     &    'G',TSL(IFIX(GCOL(LSEL))),GRCODD(LSD),
     &    'B',TSL(IFIX(BCOL(LSEL))),BLCODD(LSD),TUD
 1001   FORMAT(3(2A,F4.2,1X),A)
        CALL DWR_OVER_PRINT(22)
      END IF
C     .................... 123456789 123456789 123456789 123456789 123456789
C     .................... r:1.00 g=0.00 b=0.00 R:0.00 G=0.40 B=0.40 D C dl
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTMM
CH
      SUBROUTINE DBTMSQ(HS,VS,D,H,V)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
      DIMENSION H(5),V(5)
      H(1)=HS-D
      V(1)=VS-D
      H(3)=HS+D
      V(3)=VS+D
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTVB
CH
      SUBROUTINE DBTVB(MCO,PR)
CH
CH --------------------------------------------------------------------
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      CHARACTER *20 T
      DATA IR/5745789/,DW/.05/,N8/8/,Q14/14./,DQ/0./
C     DATA L1/1/,L15/15/
      DATA DHT/2./,W22/22./
C     DATA HH/980./,DH/198./,VV/640./,DV/220./
      DATA L0/0/,I15/15/,W2/2./,D2/4./,Q2/1.2/,KMAX/9999/
      DIMENSION HRB(4),VRB(4),R(3),G(3),B(3),LC(3)
      DIMENSION HM(999),VM(999)
      DATA LC/ 0  , 1,  15/
      DATA  R/0.00, 0., 1./
      DATA  G/0.56, 0., 1./
      DATA  B/0.56, 0., 1./
C      IF(DH.GT.190.) THEN
C        W22=WS22
C      ELSE
C        W22=0.
C      END IF
      IF(MCO.EQ.0) THEN
        DO K=1,3
          L=LC(K)
          RDCODD(L)=R(K)
          GRCODD(L)=G(K)
          BLCODD(L)=B(K)
        END DO
        PR(2,1)=3.                    ! SZ  POINTSIZE
        PR(2,3)=20.                   ! NU # OF RANDOM POINTS
        PR(2,5)=0.                    ! CO  COLOR TO BB MODIFIED VIA "CC"
        PR(2,2)=1.                    ! CR FRAME COLOR
        PR(4,2)=-1.
        PR(2,17)=40.                  ! SIZE OF MOVING SQUARE
        PR(2,16)=4.                   ! WIDTH OF WINDOW FRAME
      END IF
      H1=HMINDG(0)
      V1=VMINDG(0)
      H2=HHGHDG(0)-W22
      V2=VHGHDG(0)-W22
      DH=(H2-H1)/5.
      DV=(V2-V1)/3.
      Q=0.
      DQ=1./Q14
      DO L=1,14
        RDCODD(L)=RDCODD(1)+Q*(RDCODD(I15)-RDCODD(1))
        GRCODD(L)=GRCODD(1)+Q*(GRCODD(I15)-GRCODD(1))
        BLCODD(L)=BLCODD(1)+Q*(BLCODD(I15)-BLCODD(1))
        Q=Q+DQ
      END DO
      CALL DW_SET_CO
      CALL DQCL(0)
      CALL DGCHKX
      CALL DGEXEC
      IAR=IAREDO
      IAREDO=1
      CALL DQWIL(0.)
      HMIN=HMINDG(1)
      HLOW=HLOWDG(1)
      HHGH=HHGHDG(1)
      VMIN=VMINDG(1)
      VLOW=VLOWDG(1)
      VHGH=VHGHDG(1)
      D=(D2+Q2*PR(2,1))/DH
      K=0
      NPO=0
      PR(2,3)=MIN(PR(2,3),999.)
      DO N=1,IFIX(PR(2,3))
    1   K=K+1
        IF(K.GT.KMAX) GO TO 2
        HHM=RAN(IR)
        VVM=RAN(IR)
        DO I=1,NPO
          IF(ABS(HHM-HM(I)).LT.D.AND.
     &       ABS(VVM-VM(I)).LT.D) GO TO 1
        END DO
        NPO=N
        HM(N)=HHM
        VM(N)=VVM
      END DO
    2 CALL DQPD0(N8,PR(2,1),0.)
      DD=PR(2,16)
      L=1
      VMI=V1-DV
      DO JV=1,3
        VMI=VMI+DV
        VMINDG(1)=VMI+DD
        VLOWDG(1)=VMI+W22+DD
        VHGHDG(1)=VMI+DV-DD
        HMI=H1-DH
        DO JH=1,5
          HMI=HMI+DH
          HMINDG(1)=HMI+DD
          HLOWDG(1)=HMI+DD
          HHGHDG(1)=HMI+DH-DD
          CALL DQCL(1)
          CALL DQFFWI(L)
          CALL DQRER(0,-DW,-DW,1.+DW,1.+DW,HRB,VRB)
          CALL DQRU(HRB,VRB)
          IF(PR(4,2).GT.0.) THEN
            CALL DQPD0(8,PR(2,1)+W2,0.)
            CALL DGLEVL(IFIX(PR(2,2)))
            DO K=1,NPO
              CALL DQARD(HM(K),VM(K))
            END DO
            CALL DQPD0(N8,PR(2,1),0.)
          END IF
          CALL DGLEVL(L0)
          DO K=1,NPO
            CALL DQARD(HM(K),VM(K))
          END DO
          IF(W22.NE.00) THEN
            WRITE(T,1000) RDCODD(L),GRCODD(L),BLCODD(L)
 1000       FORMAT('r=',F4.2,' g=',F4.2,' b=',F4.2)
            CALL DGTEXT(HMINDG(1)+DHT,VMINDG(1)+DHT,T,20)
            CALL DGCHKX
            CALL DGEXEC
          END IF
          L=L+1
        END DO
      END DO
      IRUN=IR
      IAREDO=IAR
      HMINDG(1)=HMIN
      HLOWDG(1)=HLOW
      HHGHDG(1)=HHGH
      VMINDG(1)=VMIN
      VLOWDG(1)=VLOW
      VHGHDG(1)=VHGH
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DBTVB1
CH
      ENTRY DBTVB1
CH
CH --------------------------------------------------------------------
      IF(DQ.NE.0.) THEN
        Q=0.
        DO L=1,15
          IF(L.NE.0.AND.L.NE.I15) THEN
            RDCODD(L)=RDCODD(1)+Q*(RDCODD(I15)-RDCODD(1))
            GRCODD(L)=GRCODD(1)+Q*(GRCODD(I15)-GRCODD(1))
            BLCODD(L)=BLCODD(1)+Q*(BLCODD(I15)-BLCODD(1))
          END IF
          Q=Q+DQ
        END DO
        CALL DW_SET_CO
      END IF
      END
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DBTVBM
CH
      SUBROUTINE DBTVBM(PR)
CH
CH --------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EXTERNAL DBTVMM
      DIMENSION PR(4,*)
C     .. 5=CORNERS, 3=POINTS/WINDOW, 5 HORIZONTAL WINDOWS, 3 VERTICAL WINDOWS
      DIMENSION NDSEG(2,45),DSH(5),DSV(5)
      DIMENSION IHM(225),IVM(225),H2(225),V2(225)
      COMMON /DBTVC/ H1(225),V1(225)
      DATA IRUN/4578567/,K1/112/,K2/127/
      DATA DH/198./,DV/220./
      D=0.5*PR(2,1)
      DS=PR(2,17)*0.2
      CALL DBTMSQ(0.,0.,DS,DSH,DSV)
      DO K=K1,K2
        RDCODD(K)=RDCODD(0)
        GRCODD(K)=GRCODD(0)
        BLCODD(K)=BLCODD(0)
      END DO
      CALL DGSETC
      L=1
      VN=0.
      DO IV=1,3
        HN=0.
        DO IH=1,5
          R=1.+RAN(IRUN)*4.
          IR=R
          DO I=1,4
            IF(I.NE.IR) THEN
              HA=HN+DSH(I)
              VA=VN+DSV(I)
              CALL DBTMSQ(HA,VA,D,H1(L),V1(L))
              L=L+5
            END IF
          END DO
          HN=HN+DH
        END DO
        VN=VN+DV
      END DO
      HC=DH*0.5
      VC=DV+0.5
      DO K=1,225
        H2(K)=H1(K)+HC
        V2(K)=V1(K)+VC
      END DO
      L=1
      DO K=1,45
        NDSEG(1,K)=L
        NDSEG(2,K)=-5
        L=L+5
      END DO
      NUMPL=45
      IHC=HC
      IVC=VC
      CALL DGLINM(IHC,IVC,H2,V2,IHM,IVM,NAR,DBTVMM,.FALSE.,NDSEG,NUMPL)
      END
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DBTVBT
CH
      SUBROUTINE DBTVBT(MO,PR)
CH
CH --------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION V(11),H(11,6,2),LEV(6),KEND(6),VL2(6),LBW(6),IP(2)
C         4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14 /
      DATA V/
     &  21.5,28.6,35.7,42.8,50.0,57.1,64.3,71.4,78.6,85.7,92.9/
      DATA
     &  H/
     &  23. ,29. ,37. ,44. ,51. ,59. ,66. ,75. ,81. ,89. ,94. , !Y
     &  24. ,33. ,38. ,46. ,53. ,61. ,70. ,78. ,86. ,95. ,-1. , !C
     &  25. ,33. ,41. ,50. ,58. ,65. ,75. ,82. ,89. ,-1. ,-1. , !G
     &  32. ,45. ,55. ,67. ,80. ,95. ,-1. ,-1. ,-1. ,-1. ,-1. , !M
     &  35. ,47. ,64. ,80. ,97. ,-1. ,-1. ,-1. ,-1. ,-1. ,-1. , !R
     &  47. ,72. ,93. ,-1. ,-1. ,-1. ,-1. ,-1. ,-1. ,-1. ,-1. , !B
     &  21. ,30. ,36. ,44. ,51. ,59. ,66. ,72. ,81. ,88. ,97. ,
     &  25. ,33. ,41. ,50. ,57. ,66. ,75. ,84. ,91. ,-1. ,-1. ,
     &  24. ,34. ,41. ,51. ,58. ,68. ,77. ,86. ,-1. ,-1. ,-1. ,
     &  24. ,37. ,47. ,60. ,71. ,82. ,96. ,-1. ,-1. ,-1. ,-1. ,
     &  25. ,39. ,49. ,61. ,75. ,90. ,-1. ,-1. ,-1. ,-1. ,-1. ,
     &  40. ,64. ,83. ,-1. ,-1. ,-1. ,-1. ,-1. ,-1. ,-1. ,-1. /
      DATA LEV/10,14, 9,13,12,15/,KEND/11,10,9,6,5,3/
      DATA LBW/ 1, 1, 1, 8, 8, 8/
      DATA VL1/10./,HL1/10./,HL2/100./,VL2/97.,91.,86.,61.,52.,38./
      DATA IP/4,4/,H100/106./,V100/101./,I8/8/,V60/50./
      DATA D0/0./,D2/2./,D1/1./,DS/3./,DI/1./,VT0/94./,HT0/-8./
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TC(6)
      CHARACTER *3 DT3
      DATA TC/'Y=','C=','G=','M=','R=','B='/
      CHARACTER *5 T
      IF(MO.EQ.0) THEN
        PR(2,1)=3.
        PR(2,5)=1.
        PR(2,2)=1.
        PR(2,6)=8.
        PR(2,18)=1.
      END IF
      NUSER=PR(2,18)
      DO 1 L=1,6
        DO K=11,1,-1
          IF(H(K,L,NUSER).NE.-1.) THEN
            KEND(L)=K
            GO TO 1
          END IF
        END DO
    1 CONTINUE
      CALL DQCL(IAREDO)
      IZOMDO=0
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      CALL DQRER(0,0.,0.,H100,V100,HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DQAR0(0.,0.)
      CALL DGLEVL(IFIX(PR(2,6)))
      CALL DQAR(0.,0.,100.,100.)
      IF(PR(4,2).GT.0.) THEN
        DLINDD=PR(2,1)+D2
        CALL DGLEVL(IFIX(PR(2,2)))
        DO L=1,6
          DO K=1,KEND(L)-1
            CALL DQLIE(H(K,L,NUSER),V(K))
          END DO
        END DO
        CALL DQL2E(V60,V60,100.,100.)
      END IF
      DLINDD=D1
      CALL DGDASH(2,IP)
      CALL DGLEVL(IFIX(PR(2,5)))
      IF(PR(2,17).NE.1.) THEN
        DO VH=10.,90.,10.
          CALL DQL2E(VH,0.,VH,100.)
          CALL DQL2E(0.,VH,100.,VH)
        END DO
      END IF
      DO L=1,6
        CALL DQL2E(HL1,VL1,HL2,VL2(L))
        IVL2=VL2(L)
        WRITE(T,1000) IVL2
 1000   FORMAT(I2)
        CALL DQTXT(HL2,VL2(L),T,2)
      END DO
      IF(IAREDO.EQ.12.OR.IAREDO.EQ.0) THEN
        DO I=2,6
          VT=VT0-10.*(6-I)
          DO L=1,I
            HC=HL1+(VL2(I)-VL1)*(HL2-HL1)/(VL2(L)-VL1)
            HT=HT0+10.*L
            T=TC(L)//DT3(HC)
            CALL DQTXT(HT,VT,T,5)
          END DO
        END DO
      END IF
      CALL DGDASH(0,0)
      DLINDD=PR(2,1)
      DO L=1,6
        DO K=1,KEND(L)-1
          CALL DGLEVL(LEV(L))
          CALL DQLIE(H(K,L,NUSER),V(K))
        END DO
      END DO
      CALL DGLEVL(8)
      CALL DQL2E(V60,V60,100.,100.)
      CALL DGLEVL(1)
      CALL DQL2E(V60,VL1,100.,VL1)
      IF     (PR(2,16).EQ.1.) THEN
        CALL DGLEVL(IFIX(PR(2,5)))
        CALL DQPD0(I8,PR(2,1)+D0,0.)
        DO L=1,6
          DO K=1,KEND(L)
            CALL DQPD(H(K,L,NUSER),V(K))
          END DO
        END DO
      ELSE IF(PR(2,16).EQ.2.) THEN
        CALL DQPD0(I8,PR(2,1)+DS,0.)
        DO L=1,6
          DO K=1,KEND(L)
            CALL DGLEVL(LEV(L))
            CALL DQPD(H(K,L,NUSER),V(K))
          END DO
        END DO
      ELSE IF(PR(2,16).EQ.3.) THEN
        CALL DQPD0(I8,PR(2,1)+DS,0.)
        CALL DGLEVL(IFIX(PR(2,5)))
        DO L=1,6
          DO K=1,KEND(L)
            CALL DQPD(H(K,L,NUSER),V(K))
          END DO
        END DO
        CALL DQPD0(I8,PR(2,1)+DI,0.)
        DO L=1,6
          DO K=1,KEND(L)
            CALL DGLEVL(LEV(L))
            CALL DQPD(H(K,L,NUSER),V(K))
          END DO
        END DO
      ELSE IF(PR(2,16).GT.0.) THEN
        CALL DQPD0(I8,PR(2,1)+DS,0.)
        DO L=1,6
          DO K=1,KEND(L)
            CALL DGLEVL(LBW(L))
            CALL DQPD(H(K,L,NUSER),V(K))
          END DO
        END DO
        CALL DQPD0(I8,PR(2,1)+DI,0.)
        DO L=1,6
          DO K=1,KEND(L)
            CALL DGLEVL(LEV(L))
            CALL DQPD(H(K,L,NUSER),V(K))
          END DO
        END DO
      END IF
      DLINDD=PDCODD(2,LITRDD)
      CALL DQSCA('H',HRB(1),HRB(3),'%',1,'DISP.',5)
      CALL DQSCA('V',VRB(1),VRB(3),'%',1,'BRAIN',5)
      CALL DQFR(IAREDO)
      END
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------- DBTVBM
CH
      SUBROUTINE DBTVMM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH --------------------------------------------------------------------
      DIMENSION H(*),V(*),NDSEG(2,*)
      CHARACTER *(*) TKBD
      LOGICAL FBUT(4)
      COMMON /DBTVC/ H1(225),V1(225)
      IF(FBUT(4)) THEN
        HC=FLOAT(IHC)*0.2
        VC=FLOAT(IVC)*0.3
        DO K=1,225
          H(K)=H1(K)+HC
          V(K)=V1(K)+VC
        END DO
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBSIC
CH
      SUBROUTINE DBTSIC(PR)
CH
CH --------------------------------------------------------------------
CH
C     THEORIE OF PUZZLE PLOT VIA 5*5*5 CUBES
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DATA N0/0/,N7/7/,N8/8/
      DATA N1,N2,N3/1,25,1/
      DATA Q2D/.5/,Q3D/.1666667/,DS/.9/,D3/.9/,D1/.8/
      DATA Q1/0.4/,Q2/0./,Q3/0.12/,Q3DN/.08/,QS/0.4/
      DL2=PR(2, 1)
      DL1=PR(2, 7)
      DH =PR(2,12)
      N = PR(2,16)
      NM =PR(2,17)
      IF(N.LE.0) THEN
C                   123456789 123456789 123456789 123456789 123456789
        CALL DWRT('2D 1 layer: P1=1=3D, 2=wrong 2D box, 3= 2D box')
        CALL DWRT('  4=3 lines + 3D box, 5=2 lines + 2D BOX, 6=cross')
        CALL DWRT('3D 5 layers: 7=3D no hits, 8=3D, 9=3 lines + box')
        CALL DWRT('            10 = 3 lines')
        CALL DWRT('3D Skew:    11: 3D, 12=3 lines + box, 13=3 lines')
        CALL DWRT('Puzzle:     14:diagonal=color  15:diagonal=grey')
        CALL DWRT('P1=16 PUZZLE ANIMATION, call 14 before.')
        CALL DWRT('P1=0: HELP   P1=17: NUMBER PUZZLE')
        CALL DWRT('P2=1: COL=energy, SZ=line width, DH=energy width')
        CALL DWRT('P2=2: COL=CLUST., SH=grey line width.')
        CALL DWRT('P2>2 READ FOR???.DAT')
        RETURN
      END IF
      IF(N.EQ.16) THEN
        N=14+N14
      ELSE
        CALL DQCL(IAREDO)
        N14=0
      END IF
      GO TO (10,20,30,40,50,60,70,80,90,
     &  100,110,120,130,140,150,160,170),N
   10 CALL DBTSSK(DL1,DL2,D1,NM,.TRUE. ,N7,N1,N2,N3,Q1)
      GO TO 900
   20 CALL DBTSSK(DL1,DL2,1.,NM,.TRUE. ,N7,N1,N2,N3,Q2)
      GO TO 900
   30 CALL DBTSSK(DL1,DL2,DS,NM,.TRUE. ,N7,N1,N2,N3,Q2)
      GO TO 900
   40 CALL DBTSSK(DL1,DL2,1.,NM,.FALSE.,N7,N1,N2,N3,QS)
      GO TO 900
   50 CALL DBTSSK(DL1,DL2,1.,NM,.FALSE.,N7,N1,N2,N3,Q2)
      GO TO 900
   60 CALL DBTSPU(DL1,DL2,NM,-1,N1,Q2D,DH)
      GO TO 901
   70 CALL DBTS3D(DL1,DL2,1.,NM,0,N8)
      GO TO 900
   80 CALL DBTS3D(DL1,DL2,D3,NM,1,N7)
      GO TO 900
   90 CALL DBTS3D(DL1,DL2,1.,NM,2,N7)
      GO TO 900
  100 CALL DBTS3D(DL1,DL2,1.,NM,2,-1)
      GO TO 900
  110 CALL DBTSSK(DL1,DL2,DS,NM,.TRUE. ,N7,1,125,1,Q3)
      GO TO 900
  120 CALL DBTSSK(DL1,DL2,1.,NM,.FALSE.,N7,1,125,1,Q3)
      GO TO 900
  130 CALL DBTSSK(DL1,DL2,1.,NM,.FALSE.,-1,1,125,1,Q3)
      GO TO 900
  140 CALL DBTSPU(DL1,DL2,NM,-1,5,Q3D,DH)
      N14=2
      GO TO 901
  150 CALL DBTSPU(DL1,DL2,NM,N0,5,Q3DN,DH)
      GO TO 901
  160 CALL DWRT('Commands <blanc>, All, Full, Go full, Stop')
      CALL DBTSCA
      CALL DWRT('Finish')
      GO TO 901
  170 CALL DBTSPU(DL1,DL2,NM,-99,5,Q3DN,DH)
      GO TO 901
  900 CALL DQFR(IAREDO)
  901 DLINDD=PDCODD(2,LITRDD)
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTS3D
CH
      SUBROUTINE DBTS3D(DL1,DL2,D,NM,IMOD,NGR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C     FI=22 TE=90 AL=10
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
      DIMENSION L(125),X(10),Y(10),Z(10),
     &  XX(10,125),YY(10,125)
      DATA NEND/125/,DR/.1/,N8/8/,SS/-1./
      DATA A1,A2,A3/22.,90.,9./
      CALL DBTSCL(NM,L)
      XMAX=-999.
      YMAX=-999.
      XMIN=9999.
      YMIN=9999.
      SF=SIND(    A1)
      CF=COSD(    A1)
      ST=SIND(90.-A2)
      CT=COSD(90.-A2)
C     SG IS DEFINED NEGATIVE UNLIKE NORMAL USE IN DALI, SO THAT ROTATION AND
C     SKEW ARE EASIER TO BE COMPARED.
      SG=SS*SIND(    A3)
      CG=COSD(    A3)
      N=0
      DO ZN=1.,5.
        DO YN=1.,5.
          DO XN=1.,5.
            N=N+1
            X(1)=XN
            X(2)=XN+D
            X(3)=XN+D
            X(4)=XN
            X(5)=XN
            X(6)=XN
            X(7)=XN+D
            X(8)=XN+D
            X(9)=XN
            X(10)=XN
            Y(1)=YN
            Y(2)=YN
            Y(3)=YN+D
            Y(4)=YN+D
            Y(5)=YN
            Y(6)=YN
            Y(7)=YN
            Y(8)=YN+D
            Y(9)=YN+D
            Y(10)=YN
            Z(1)=ZN
            Z(2)=ZN
            Z(3)=ZN
            Z(4)=ZN
            Z(5)=ZN
            Z(6)=ZN+D
            Z(7)=ZN+D
            Z(8)=ZN+D
            Z(9)=ZN+D
            Z(10)=ZN+D
            DO K=1,10
              X1=X(K)
              Y1=Y(K)
              Z1=Z(K)
              X2= CF*X1+SF*Y1
              Y2=-SF*X1+CF*Y1
              X4= CT*X2+ST*Z1
              Z3=-ST*X2+CT*Z1
              Y4= CG*Y2+SG*Z3
              XX(K,N)=X4
              YY(K,N)=Y4
              XMIN=MIN(X4,XMIN)
              XMAX=MAX(X4,XMAX)
              YMIN=MIN(Y4,YMIN)
              YMAX=MAX(Y4,YMAX)
            END DO
          END DO
        END DO
      END DO
      CALL DQRER(0,XMIN-DR,YMIN-DR,XMAX+DR,YMAX+DR,HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DGLEVL(NGR)
      IF(NGR.GT.0) THEN
        DLINDD=DL1
        DO N=1,NEND
          DO K=1,9
            CALL DQLIE(XX(K,N),YY(K,N))
          END DO
          DO K=2,4
            CALL DQL2E(XX(K,N),YY(K,N),XX(K+5,N),YY(K+5,N))
          END DO
        END DO
      END IF
      DLINDD=DL2
      IF(IMOD.EQ.1) THEN
        DO N=1,NEND
          IF(L(N).GT.0) THEN
            CALL DGLEVL(L(N)+N8)
            DO K=1,9
              CALL DQLIE(XX(K,N),YY(K,N))
            END DO
            DO K=2,4
              CALL DQL2E(XX(K,N),YY(K,N),XX(K+5,N),YY(K+5,N))
            END DO
          END IF
        END DO
      ELSE IF(IMOD.EQ.2) THEN
        DO N=1,NEND
          IF(L(N).GT.0) THEN
            CALL DGLEVL(L(N)+N8)
            DO K=2,6,2
              CALL DQL2E(XX(1,N),YY(1,N),XX(K,N),YY(K,N))
            END DO
          END IF
        END DO
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTSSK
CH
      SUBROUTINE DBTSSK(DL1,DL2,D,NM,FULL,NGR,N1,N2,N3,Q)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
      DIMENSION L(125),X(10),Y(10),Z(10),
     &  XX(10,125),YY(10,125)
      DATA DR/.2/,N8/8/
      LOGICAL FULL
      CALL DBTSCL(NM,L)
      XMAX=-999.
      YMAX=-999.
      XMIN=9999.
      YMIN=9999.
      N=0
      DO ZN=1.,5.
        DO YN=1.,5.
          DO XN=1.,5.
            N=N+1
            IF(N.GT.N2) GO TO 1
            X(1)=XN
            X(2)=XN+D
            X(3)=XN+D
            X(4)=XN
            X(5)=XN
            X(6)=XN
            X(7)=XN+D
            X(8)=XN+D
            X(9)=XN
            X(10)=XN
            Y(1)=YN
            Y(2)=YN
            Y(3)=YN+D
            Y(4)=YN+D
            Y(5)=YN
            Y(6)=YN
            Y(7)=YN
            Y(8)=YN+D
            Y(9)=YN+D
            Y(10)=YN
            Z(1)=ZN
            Z(2)=ZN
            Z(3)=ZN
            Z(4)=ZN
            Z(5)=ZN
            Z(6)=ZN+D
            Z(7)=ZN+D
            Z(8)=ZN+D
            Z(9)=ZN+D
            Z(10)=ZN+D
            DO K=1,10
              XX(K,N)=X(K)+Q*Z(K)
              YY(K,N)=Y(K)+Q*Z(K)
              XMIN=MIN(XX(K,N),XMIN)
              XMAX=MAX(XX(K,N),XMAX)
              YMIN=MIN(YY(K,N),YMIN)
              YMAX=MAX(YY(K,N),YMAX)
            END DO
          END DO
        END DO
      END DO
    1 CALL DQRER(0,XMIN-DR,YMIN-DR,XMAX+DR,YMAX+DR,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(NGR.GT.0) THEN
        DLINDD=DL1
        CALL DGLEVL(NGR)
        DO N=N1,N2,N3
          DO K=1,9
            CALL DQLIE(XX(K,N),YY(K,N))
          END DO
          DO K=2,4
            CALL DQL2E(XX(K,N),YY(K,N),XX(K+5,N),YY(K+5,N))
          END DO
        END DO
      END IF
      DLINDD=DL2
      IF(FULL) THEN
        DO N=N1,N2,N3
          IF(L(N).GT.0) THEN
            CALL DGLEVL(L(N)+N8)
            DO K=1,9
              CALL DQLIE(XX(K,N),YY(K,N))
            END DO
            DO K=2,4
              CALL DQL2E(XX(K,N),YY(K,N),XX(K+5,N),YY(K+5,N))
            END DO
          END IF
        END DO
      ELSE
        DO N=N1,N2,N3
          IF(L(N).GT.0) THEN
            CALL DGLEVL(L(N)+N8)
            DO K=2,6,2
              CALL DQL2E(XX(1,N),YY(1,N),XX(K,N),YY(K,N))
            END DO
          END IF
        END DO
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTSPU
CH
      SUBROUTINE DBTSPU(DL1I,DL2I,NM,NDIAI,N2,Q,DHI)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------


C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
      DIMENSION L(125),I(125),LI(125),LC(125),NE(7)
      DATA NE/13,14,8,8,8,12,8/,QE/0.03/
      DATA D/1./,DR/.2/,NGR/7/,DD/.1/,N8/8/,L0/0/,L8/8/,DL0/2./
      DATA LC7/7/,LC8/-2/,DTX/4./,DTY/4./
      CHARACTER *1 T1,T0,DT1
      DL1=DL1I
      DL2=DL2I
      NDIA=NDIAI
      DH=DHI
      CALL DBTSCL(NM,L)
      CALL DBTSCL(1 ,I)
      GO TO 1
      ENTRY DBTSPA(T1,LI,LC,N2,Q)
      IF(T1.EQ.'S') RETURN
      IF(T1.NE.'A'.AND.T1.NE.'G') THEN
        CALL DGETLN(T0,L1,1)
        IF(L1.EQ.1) T1=T0
        IF(T1.EQ.'S') RETURN
      END IF
      CALL DQCL(IAREDO)
      DO K=1,125
        IF(LI(K).NE.0) THEN
          IF(LC(K).NE.0) THEN
            L(K)=LC(K)
          ELSE
            L(K)=LC7
          END IF
        ELSE
          IF(LC(K).NE.0) THEN
            L(K)=LC8
          ELSE
            L(K)=0
          END IF
        END IF
      END DO
    1 CALL DQRER(0,1.-DR,1.-DR,6.+DR,6.+DR,HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DGLEVL(NGR)
      DLINDD=DL1
      DO XN=1.,6.
        CALL DQL2E(XN,1.,XN,6.)
      END DO
      DO YN=1.,6.
        CALL DQL2E(1.,YN,6.,YN)
      END DO
      DLINDD=DL2
      N=0
      Q2=.5*Q
      DO ZN=1.,N2
        DO YN=1.,5.
          DO XN=1.,5.
            N=N+1
            IF(L(N).NE.0) THEN
              CALL DGLEVL(L(N)+N8)
              A=ZN*Q
              Y=YN+A
              X=XN+A
              IF(NDIA.NE.-99) THEN
                CALL DQL2E(XN,Y,XN+D,Y)
                CALL DQL2E(X,YN,X,YN+D)
                IF(N2.GT.1.AND.NDIA.LT.0) CALL DQL2E(X-Q2,Y-Q2,X+Q2,
     &            Y+Q2)
              ELSE
                CALL DQTXT(X-DTX,Y-DTY,DT1(ZN),1)
              END IF
            END IF
          END DO
        END DO
      END DO
      IF(NDIA.NE.-99.AND.DH.GT.0.) THEN
        N=0
        D2=.5*D
        DLB=DH-DL0
        DO ZN=1.,N2
          DO YN=1.,5.
            DO XN=1.,5.
              N=N+1
              IF(L(N).NE.0) THEN
                A=ZN*Q
                Y=YN+A
                X=XN+A
                DLINDD=DH
                E=QE*I(N)
                IF(L0.LT.0.OR.DLB.LE.0.) THEN
                  CALL DGLEVL(NE(L(N)))
                  CALL DQL2E(XN+D2-E,Y,XN+D2+E,Y)
                ELSE
                  CALL DGLEVL(L8)
                  CALL DQL2E(XN+D2-E,Y,XN+D2+E,Y)
                  DLINDD=DLB
                  CALL DGLEVL(L0)
                  CALL DQL2E(XN+D2-E,Y,XN+D2+E,Y)
                END IF
              END IF
            END DO
          END DO
        END DO
      END IF
      IF(NDIA.GE.0) THEN
        CALL DGLEVL(NDIA)
        DO XN=1.,5.
          DO YN=1.,5.
            CALL DQL2E(XN+DD,YN+DD,XN+D-DD,YN+D-DD)
          END DO
        END DO
      END IF
      CALL DQFR(IAREDO)
      CALL DGCHKX
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBTMM
CH
      SUBROUTINE DBTSCL(MI,LL)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
      CHARACTER *1 T1
      DIMENSION L(5,5,5),LL(125),I(5,5,5),JC(6)
      DATA L/
     &  1,1,5,0,4,
     &  3,0,0,1,1,
     &  0,6,3,0,4,
     &  1,1,0,2,0,
     &  2,2,2,3,0,
     &   0,0,2,0,4,
     &   6,0,4,0,0,
     &   3,0,0,0,5,
     &   0,0,0,0,5,
     &   0,2,0,3,0,
     &    3,1,0,3,1,
     &    0,0,0,0,0,
     &    2,0,2,0,0,
     &    0,0,0,0,1,
     &    0,3,2,3,3,
     &     0,2,4,5,0,
     &     2,0,0,0,1,
     &     3,6,0,4,1,
     &     3,0,0,0,0,
     &     0,0,5,0,1,
     &      0,0,0,4,0,
     &      2,1,0,4,0,
     &      0,0,4,0,1,
     &      1,5,2,2,3,
     &      2,1,0,2,0/
      DATA JC/6,4,2,3,1,5/,N6/6/,N2/5/,Q/.1666667/,LW/-2/
      LOGICAL FAN
      FAN=.FALSE.
      T1=' '
      M=MI
      GO TO 107
      ENTRY DBTSCA
      FAN=.TRUE.
      T1=' '
      M=2
  107 IF(M.EQ.1) THEN
        CALL UCOPY(L,LL,125)
      ELSE
        IF(M.GT.2) THEN
          READ(M,1000,ERR=9) NN6
          IF(NN6.GT.2) N6=NN6
          DO NZ=1,5
            DO NY=1,5
              READ(M,1000,ERR=9) (L(NX,NY,NZ),NX=1,5)
C 1000         FORMAT(7X,4I,I1)
 1000         FORMAT(6X,5(1X,I1))
            END DO
          END DO
          CLOSE(UNIT=M)
        END IF
        CALL VZERO(I,125)
        K=0
        DO J=1,6
          DO NZ=1,5
            DO NY=1,5
              DO NX=1,5
                IF(L(NX,NY,NZ).NE.0.AND.I(NX,NY,NZ).EQ.0) THEN
                  I(NX,NY,NZ)=JC(J)
                  IF(FAN) CALL DBTSPA(T1,L,I,N2,Q)
                  GO TO 3
                END IF
              END DO
            END DO
          END DO
          GO TO 6
    3     DO N=1,N6
            DO NZ=1,5
              DO NY=1,5
                DO NX=1,5
                  IF(L(NX,NY,NZ).NE.0.AND.I(NX,NY,NZ).EQ.0) THEN
                    IF(FAN) THEN
                      IF(T1.EQ.'F'.OR.T1.EQ.'G') THEN
                        III=I(NX,NY,NZ)
                        I(NX,NY,NZ)=LW
                        CALL DBTSPA('A',L,I,N2,Q)
                        I(NX,NY,NZ)=III
                      END IF
                    END IF
                    IF     (NX.GT.1.AND.I(NX-1,NY  ,NZ  ).NE.0) THEN
                      I(NX,NY,NZ)=I(NX-1,NY  ,NZ  )
                      IF(FAN) CALL DBTSPA(T1,L,I,N2,Q)
                    ELSE IF(NX.LT.5.AND.I(NX+1,NY  ,NZ  ).NE.0) THEN
                      I(NX,NY,NZ)=I(NX+1,NY  ,NZ  )
                      IF(FAN) CALL DBTSPA(T1,L,I,N2,Q)
                    ELSE IF(NY.GT.1.AND.I(NX  ,NY-1,NZ  ).NE.0) THEN
                      I(NX,NY,NZ)=I(NX  ,NY-1,NZ  )
                      IF(FAN) CALL DBTSPA(T1,L,I,N2,Q)
                    ELSE IF(NY.LT.5.AND.I(NX  ,NY+1,NZ  ).NE.0) THEN
                      I(NX,NY,NZ)=I(NX  ,NY+1,NZ  )
                      IF(FAN) CALL DBTSPA(T1,L,I,N2,Q)
                    ELSE IF(NZ.GT.1.AND.I(NX  ,NY  ,NZ-1).NE.0) THEN
                      I(NX,NY,NZ)=I(NX  ,NY  ,NZ-1)
                      IF(FAN) CALL DBTSPA(T1,L,I,N2,Q)
                    END IF
                    IF(NZ.LT.5.AND.I(NX  ,NY  ,NZ+1).NE.0) THEN
                      I(NX,NY,NZ)=I(NX  ,NY  ,NZ+1)
                      IF(FAN) CALL DBTSPA(T1,L,I,N2,Q)
                    END IF
                  END IF
                END DO
              END DO
            END DO
          END DO
        END DO
    6   IF(FAN.OR.T1.NE.' ') RETURN
        CALL UCOPY(I,LL,125)
      END IF
      RETURN
    9 CALL DWRT('DBTSCL: Read error.')
      CLOSE(UNIT=M)
      END
*DK DBTFR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTFR
CH
      SUBROUTINE DBTFR(MO,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DIMENSION HS(4),VS(4),IC(4)
      DATA HS/-1.2, 0. , 1. , 1.5/
      DATA VS/ 0. ,-0.3, 0.6, 0.4/
      DATA IC/  9 , 10 , 14 , 12 /
      IF(MO.EQ.0) THEN
        PR(2, 1)=1.
        PR(2, 2)=1.
        PR(2, 6)=8.
        PR(2, 7)=1.
        PR(2,16)=3.
        PR(1,17)=4.
      END IF
C     .........................     SET P1=1,3 SZ=1 SH=1 CA=1,8 P2=4 CR=1
C     ................................................... SZ size
      DS=0.5*PR(2,1)
C     ................................................... SH sice of frame
      DF=0.5*PR(2,7)+DS
C     ................................................... CA background color
      LBG=PR(2,6)
C     ................................................... CR frame color
      LFR=PR(2,2)
C     ................................................... P1 1,2,3 type
      NTY=PR(2,16)
C     ................................................... P2 user range
      RTO=PR(2,17)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DQAR0(1.,1.)
      NANSW=-1
      DO K=1,4
        IF(     NTY.EQ.1) THEN
          CALL DGLEVL(IC(K))
          CALL DQAR(HS(K)-DS,VS(K)-DS,HS(K)+DS,VS(K)+DS)
        ELSE IF(NTY.EQ.2) THEN
          CALL DGLEVL(LFR)
          CALL DQAR(HS(K)-DF,VS(K)-DF,HS(K)+DF,VS(K)+DF)
          CALL DGLEVL(IC(K))
          CALL DQAR(HS(K)-DS,VS(K)-DS,HS(K)+DS,VS(K)+DS)
        ELSE
          CALL DGLEVL(LFR)
          CALL DQAR(HS(K)-DF,VS(K)-DF,HS(K)+DF,VS(K)+DF)
        END IF
        IF(K.NE.4.AND.NTY.LE.3.AND.NANSW.LT.0) THEN
          CALL DGCHKX
          CALL DTYANS('Step, All=<CR> ?','A',NANSW)
        END IF
      END DO
      IF(NTY.GE.3) THEN
        NANSW=-1
        DO K=1,4
          CALL DGLEVL(IC(K))
          CALL DQAR(HS(K)-DS,VS(K)-DS,HS(K)+DS,VS(K)+DS)
          CALL DGCHKX
          IF(K.NE.4.AND.NTY.LE.3.AND.NANSW.LT.0) THEN
            CALL DGCHKX
            CALL DTYANS('Step, All=<CR> ?','A',NANSW)
          END IF
        END DO
      END IF
      CALL DQFR(IAREDO)
      END
*DK DBTCBL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTCBL
CH
      SUBROUTINE DBTCBL(MO,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4),HM(999),VM(999)
      DIMENSION H1(2),H2(2),DH(2),QQ(2)
      DIMENSION V1(2),V2(2),DV(2)
      DATA IR/3249153/,D2/4./,Q2/1.2/,W2/2./,KMAX/9999/,BO/.03/
      DATA RR/.5/,R5/0.5/
      IF(MO.EQ.0) THEN
        PR(2, 1)=3.
        PR(2, 6)=4.
        PR(2,13)=12.
        PR(2,14)=8.
        PR(2, 2)=1.
        PR(4, 2)=-1.
        PR(2, 3)=555.
        PR(2,17)=10.
        PR(2, 8)=1.
        PR(2,10)=8.
        PR(2,11)=8.
        PR(2,4 )=40.
        PR(4,4 )=-1.
      END IF
      DHD=HHGHDG(IAREDO)-HMINDG(IAREDO)
C     .............................................................. SZ=3
      SZ=PR(2,1)
C     ............................ COLOR OF AREA  .................. CA=4
      LA=PR(2,6)
C     ............................ COLOR OF NOISE  ................. V1=12
      LN=PR(2,13)
C     ............................ COLOR OF SIGNAL ................. V2=8
      LS=PR(2,14)
C     .............................................................. CR=1
      LF=PR(2,2)
C     ................................ GERADE=BOSE UNGERADE=FERMI .. NU=555
      PR(2,3)=MIN(PR(2,3),999.)
      NU=PR(2, 3)
C     ...............................WIDTH OF COLORED LINES  ....... P2=20
      WI=PR(2,17)*0.005
C     ................................. EMPTY INNER CIRCLE ......... FR=4
      DI=PR(2,8)*0.05
C     ...................................... LINE SYMBOL  .......... H1=8
      IS=MIN(PR(2,10),8.)
C     ..................................... NOISE SYMBOL  .......... H2=8
      IN=MIN(PR(2,11),8.)
C     ........................................ # OF HITS  .......... RN=8
      PO=PR(2, 4)
      DCL=R5-DI
      DCH=R5+DI
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      CALL DQRER(0,-BO,-BO,1.+BO,1.+BO,HRB,VRB)
      CALL DQRU(HRB,VRB)
      D=(D2+Q2*SZ)/DHD
      H1(1)=R5+RR*(RAN(IR)-R5)
      V1(1)=R5+RR*(RAN(IR)-R5)
      H2(1)=RAN(IR)
      V2(1)=RAN(IR)
      DH(1)=H2(1)-H1(1)
      DV(1)=V2(1)-V1(1)
      H1(2)=H1(1)
      V1(2)=V1(1)
      A=H1(1)*DH(1)+V1(1)*DV(1)
      IF(ABS(DH(1)).GT.ABS(DV(1))) THEN
        V2(2)=0.
        H2(2)=A/DH(1)
      ELSE
        H2(2)=0.
        V2(2)=A/DV(1)
      END IF
      NPO=0
      DH(2)=H2(2)-H1(2)
      DV(2)=V2(2)-V1(2)
      IF(PR(4,4).GT.0.) THEN
        DO I=1,2
          RL=SQRT(DH(I)**2+DV(I)**2)
          DH(I)=DH(I)/RL
          DV(I)=DV(I)/RL
          STH=DH(I)/PO
          STV=DV(I)/PO
          DO J=1,2
            HHM=H1(I)
            VVM=V1(I)
            DO N=1,200
              IF(HHM.GT.1..OR.HHM.LT.0..OR.
     &           VVM.GT.1..OR.VVM.LT.0.) GO TO 21
              IF(HHM.LE.DCL.OR.HHM.GE.DCH.OR.
     &           VVM.LE.DCL.OR.VVM.GE.DCH) THEN
                NPO=NPO+1
                HM(NPO)=HHM
                VM(NPO)=VVM
                HHM=HHM+STH
                VVM=VVM+STV
              END IF
            END DO
   21       STH=-STH
            STV=-STV
          END DO
        END DO
        K1=NPO
      ELSE
        K1=0
      END IF
      K=0
      DO N=1,NU
    1   K=K+1
        IF(K.GT.KMAX.OR.NPO.GT.999) GO TO 2
        HHM=RAN(IR)
        VVM=RAN(IR)
        IF(MOD(NU,2).EQ.1) THEN
          DO I=1,NPO
            IF(ABS(HHM-HM(I)).LT.D.AND.
     &         ABS(VVM-VM(I)).LT.D) GO TO 1
          END DO
        END IF
        NPO=NPO+1
        HM(NPO)=HHM
        VM(NPO)=VVM
      END DO
    2 IF(PR(4,2).GT.0.) THEN
        CALL DQPD0(8,SZ+W2,0.)
        CALL DGLEVL(LF)
        DO K=1,NPO
          CALL DQARD(HM(K),VM(K))
        END DO
      END IF
      IF(K1.GT.0) THEN
        CALL DQPD0(IN,SZ,0.)
        CALL DGLEVL(LN)
        DO K=K1+1,NPO
          CALL DQPD(HM(K),VM(K))
        END DO
        CALL DQPD0(IS,SZ,0.)
        CALL DGLEVL(LS)
        DO K=1,K1
          CALL DQPD(HM(K),VM(K))
        END DO
      ELSE
        QQ(1)=1./SQRT(DH(1)*DH(1)+DV(1)*DV(1))
        QQ(2)=1./SQRT(DH(2)*DH(2)+DV(2)*DV(2))
        DO K=1,NPO
          DHP=HM(K)-H1(1)
          DVP=VM(K)-V1(1)
          IF(HM(K).LE.DCL.OR.HM(K).GE.DCH.OR.
     &       VM(K).LE.DCL.OR.VM(K).GE.DCH) THEN
            DO I=1,2
              D=( DV(I)*DHP-DH(I)*DVP ) *QQ(I)
              IF(ABS(D).LE.WI) GO TO 11
            END DO
          END IF
          CALL DQPD0(IN,SZ,0.)
          CALL DGLEVL(LN)
          GO TO 12
   11     CALL DGLEVL(LS)
          CALL DQPD0(IS,SZ,0.)
   12     CALL DQPD(HM(K),VM(K))
        END DO
      END IF
      CALL DQFR(IAREDO)
      END
*DK DBTNPR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTNPR
CH
      SUBROUTINE DBTNPR(MM,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DATA S2/110./,S1/-10./
      DLIN=DLINDD
      IF(MM.EQ.0) THEN
        PR(2,10)=0.
        PR(2,11)=100.
        PR(2,12)=10.
        PR(2,13)=0.
        PR(2,14)=100.
        PR(2,15)=10.
C
        PR(2,16)=100.
        PR(2,20)=5.
C
        PR(2,17)=3.
        PR(2,18)=0.
        PR(2,19)=0.02
C
        PR(2,7)=1.
        PR(2,1)=2.
        PR(2,3)=10.
        PR(2,5)=10.
        PR(2,2)=12.
        PR(2,6)=1.
        PR(2,8)=8.
      END IF
      H1=PR(2,10)     ! HORIZONTAL AND VERTICAL LOOP VALUES
      H2=PR(2,11)
      DH=PR(2,12)
      V1=PR(2,13)
      V2=PR(2,14)
      DV=PR(2,15)
C
      RR=PR(2,16)                   ! P1: RADIUS OF RADIAL LINES
      DF=PR(2,20)
C
      DR=PR(2,17)                   ! P2: DISTORTION SCALE
      MO=PR(2,18)                   ! PL: DISTORTION MODE
      DS=PR(2,19)                   ! QR: DISTORTION FACTOR
C
      SH=PR(2,7)                    ! SH: linewidth OF CIRCLES AND RECT. LINES
      SZ=PR(2,1)                    ! SZ: linewidth of radial lines
      NU=PR(2,3)                    ! NU: # OF INNER POINTS PER LINE
      LR=PR(2,2)                    ! CR: COLOR OF RECTANGULAR LINES
      LC=PR(2,5)                    ! CO: COLOR OF CIRCLES
      LT=PR(2,8)                    ! FR: COLOR OF  RADIAL TRACKS
      LA=PR(2,6)                    ! CA: COLOR OF AREA
C
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQRER(0,S1,S1,S2,S2,HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DQFFWI(LA)
C
      DLINDD=SH
      IF(LR.GT.0) THEN
        CALL DGLEVL(LR)
C       ................................................... VERTICAL LINES
        DO H=H1,H2,DH
          CALL DBTPP(H2,V2,MO,DS,DR,NU,H,V1,H,V2)
        END DO
C       ................................................. HORIZONTAL LINES
        DO V=V1,V2,DV
          CALL DBTPP(H2,V2,MO,DS,DR,NU,H1,V,H2,V)
        END DO
      END IF
      IF(LC.GT.0) THEN
C       ................................................... CIRCLES
        CALL DGLEVL(LC)
        DO R=DH,RR,DH
          HH1=R
          VV1=0.
          DO FI=1.,90.,1.
            HH2=R*COSD(FI)
            VV2=R*SIND(FI)
            CALL DBTPP(H2,V2,MO,DS,DR,0,HH1,VV1,HH2,VV2)
            HH1=HH2
            VV1=VV2
          END DO
        END DO
      END IF
      DLINDD=SZ
      IF(LT.GT.0) THEN
C       ................................................... RADIAL TRACKS
        CALL DGLEVL(LT)
        DO FI=0.,90.,DF
          H=RR*COSD(FI)
          V=RR*SIND(FI)
          CALL DBTPP(H2,V2,MO,DS,DR,NU,0.,0.,H,V)
        END DO
      END IF
      DLINDD=DLIN
      CALL DQFR(IAREDO)
      END
*DK DBT_SG
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_SG
CH
      SUBROUTINE DBT_SG(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DATA HRB/0.,10.,10.,0./,VRB/0.,0.,7.,7./
      DIMENSION H(5),V(5),D(5)
      DATA H/2. ,3. ,5. ,7.,7./
      DATA V/4. ,2. ,3. ,4.,2./
      DATA D/0. ,0. ,.13,.2,.5/
      DLIN=DLINDD
      LA=PR(2,6)
      LC=PR(2,5)
      LF=PR(2,2)
      LL=PR(2,9)
      LS=PR(2,8)
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      CALL DQRU(HRB,VRB)
      CALL DQAR0(1.,1.)
      IF(PR(4,2).GT.0.) THEN
        ND=PR(2,16)
        ND=MIN(5,MAX(ND,1))
        DLINDD=PR(2,1)+PR(2,7)
        CALL DGLEVL(LF)
        DO HH=1.,9.,1.
          CALL DQL2E(HH,1.,HH,6.)
        END DO
        DO VV=1.,6.,1.
          CALL DQL2E(1.,VV,9.,VV)
        END DO
        DO N=1,5
          H1=H(N)+D(ND)
          H2=H1+1.
          V1=V(N)+D(ND)
          V2=V1+1.
          CALL DQREC(H1,V1,H2,V2)
          ND=ND+1
          IF(ND.GT.5) ND=1
        END DO
      END IF
      ND=PR(2,16)
      ND=MIN(5,MAX(ND,1))
      DLINDD=PR(2,1)
      CALL DGLEVL(LL)
      DO HH=1.,9.,1.
        CALL DQL2E(HH,1.,HH,6.)
      END DO
      DO VV=1.,6.,1.
        CALL DQL2E(1.,VV,9.,VV)
      END DO
      DO N=1,5
        IF(PR(2,17).EQ.0.) THEN
          IF(D(N).NE.0.) GO TO 2
          CALL DGLEVL(LC)
        ELSE
          IF(D(N).EQ.0.) THEN
            CALL DGLEVL(LS)
          ELSE
            CALL DGLEVL(LC)
          END IF
        END IF
        H1=H(N)+D(ND)
        H2=H1+1.
        V1=V(N)+D(ND)
        V2=V1+1.
        CALL DQREC(H1,V1,H2,V2)
        ND=ND+1
        IF(ND.GT.5) ND=1
    2 END DO
      CALL DQFR(IAREDO)
      DLINDD=DLIN
      END
*DK DBT_WRW
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_WRW
CH
      SUBROUTINE DBT_WRW(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DATA HRB/0.,10.,10.,0./,VRB/0.,0.,11.,11./
      DATA V1,V4/1.,9./,H1/1./
      LA=PR(2,6)   ! CA
      LC=PR(2,5)   ! CO
      LF=PR(2,2)   ! CR
      DV=PR(2,15)  ! =1
      DH=PR(2,12)  ! =1
      V2=PR(2,13)  !  =4
      V3=PR(2,14)  !  =6
      DLINDD=PR(2,1)
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      CALL DQRU(HRB,VRB)
      CALL DGLEVL(LF)
      H=H1
      DO K=1,99
        CALL DQL2E(H,V2,H,V3)
        H=H+DH
        IF(H.GT.9.) GO TO 10
      END DO
   10 CALL DGLEVL(LC)
      IF(PR(4,13).EQ.1.) THEN
        H=H1
        DH=DH*2.
        DO K=1,99
          CALL DQL2E(H,V1,H,V2-DV)
          H=H+DH
          IF(H.GT.9.) GO TO 20
        END DO
      END IF
   20 IF(PR(4,14).EQ.1.) THEN
        H=H1
        DO K=1,99
          CALL DQL2E(H,V3+DV,H,V4)
          H=H+DH
          IF(H.GT.9.) GO TO 30
        END DO
      END IF
   30 CALL DQFR(IAREDO)
      END
*DK DBT_TR
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT_TR
CH
      SUBROUTINE DBT_TR(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB1(4),VRB1(4)
      DIMENSION HRB2(4),VRB2(4)
      DIMENSION H(2),V(2)

C                     14                               8
C           6                     22                   4
C      2        10.         18          26             2
C   0     4   8    12    16    20    24    28          1
      DIMENSION X1(14),Y1(14),X2(14),Y2(14),LC(14)

      DATA X1/14.,14., 6., 6.,22.,22., 2., 2.,10.,10.,18.,18.,26.,26./
      DATA Y1/ 8., 8., 4., 4., 4., 4., 2., 2., 2., 2., 2., 2., 2., 2./
      DATA X2/ 6.,22., 2.,10.,18.,26., 0., 4., 8.,12.,16.,20.,24.,28./
      DATA Y2/ 4., 4., 2., 2., 2., 2., 1., 1., 1., 1., 1., 1., 1., 1./
      DATA LC/ 1 , 9 , 1 ,12 , 9 ,10 , 1 ,15 ,12 ,13 , 9 ,14 ,10 , 8 /
      CHARACTER *1 TC(8)
      DATA TC/'K','B','R','M','G','C','Y','W'/
      DATA DH1/-6./,DV1/-12./
      DATA HRB1/-2.,30.,30.,-2./,VRB1/0.,0.,9.,9./
      DATA HRB2/0.,10.,10.,0./,VRB2/-2.,-2.,30.,30./,Y/9./
      DATA D2/2./
      LOGICAL FIN
      DLIN=DLINDD
      DLINDD=D2
      LA=PR(2,6)
      LF=PR(2,2)
      LT=PR(2,5)
      DL=PR(2,1)
      DF=PR(2,7)+PR(2,1)
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      CALL DGLEVL(LF)
      IF(PR(2,16).NE.1.) THEN
        CALL DQRU(HRB1,VRB1)
        DO K=1,14
          CALL DQPOC(X1(K),Y1(K),HH1,V(1),FIN)
          CALL DQPOC(X2(K),Y2(K),HH2,V(2),FIN)
          DO D=-DF,DF,1.
            H(1)=HH1+D
            H(2)=HH2+D
            CALL DGDRAW(2,H,V)
          END DO
        END DO
        DO K=1,14
          CALL DQPOC(X1(K),Y1(K),HH1,V(1),FIN)
          CALL DQPOC(X2(K),Y2(K),HH2,V(2),FIN)
          DO D=-DL,DL,1.
            CALL DGLEVL(LC(K))
            H(1)=HH1+D
            H(2)=HH2+D
            CALL DGDRAW(2,H,V)
          END DO
        END DO
        X=0.
        CALL DGLEVL(LT)
        DO L=1,8
          CALL DQPOC(X,1.,HT,VT,FIN)
          CALL DGTEXT(HT+DH1,VT+DV1,TC(L),1)
          X=X+4.
        END DO
      ELSE
        CALL DQRU(HRB2,VRB2)
        DO K=1,14
          CALL DQPOC(Y-Y1(K),X1(K),H(1),VV1,FIN)
          CALL DQPOC(Y-Y2(K),X2(K),H(2),VV2,FIN)
          DO D=-DF,DF,1.
            V(1)=VV1+D
            V(2)=VV2+D
            CALL DGDRAW(2,H,V)
          END DO
        END DO
        DO K=1,14
          CALL DQPOC(Y-Y1(K),X1(K),H(1),VV1,FIN)
          CALL DQPOC(Y-Y2(K),X2(K),H(2),VV2,FIN)
          DO D=-DL,DL,1.
            CALL DGLEVL(LC(K))
            V(1)=VV1+D
            V(2)=VV2+D
            CALL DGDRAW(2,H,V)
          END DO
        END DO
      END IF
      DLIN=DLINDD
      END
*DK DBT_FK
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBT_FK
CH
      SUBROUTINE DBT_FK(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C   DRAW COLOR CIRCLE = FARB KREIS
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DATA HRB/-10.,10.,10.,-10./,VRB/-10.,-10.,10.,10./
      DIMENSION H(6),V(6),DA(2)
      DATA DA/30.,60./
      DIMENSION LCOL(12),RCOL(12),GCOL(12),BCOL(12)
      DATA LCOL/9, 2,10, 3,12, 4,13, 5,15, 6,14, 7/
      DATA RCOL/0.,9.,1.,1.,1.,1.,1.,9.,0.,0.,0.,0./
      DATA GCOL/1.,1.,1.,9.,0.,0.,0.,0.,0.,9.,1.,1./
      DATA BCOL/0.,0.,0.,0.,0.,9.,1.,1.,1.,1.,1.,9./
      LA=PR(2,6)
      L1=PR(2,5)
      R1=PR(2,8)
      R2=PR(2,9)
      A0=PR(2,20)
      M=PR(2,16)
      M=1+MOD(M,2)
      G=MAX(0.,MIN(1.,PR(2,17)))
      IF(G.GT.0.) THEN
        DO K=1,12
          L=LCOL(K)
          IF(RCOL(K).EQ.9.) THEN
            RDCODD(L)=G
          ELSE
            RDCODD(L)=RCOL(K)
          END IF
          IF(GCOL(K).EQ.9.) THEN
            GRCODD(L)=G
          ELSE
            GRCODD(L)=GCOL(K)
          END IF
          IF(BCOL(K).EQ.9.) THEN
            BLCODD(L)=G
          ELSE
            BLCODD(L)=BCOL(K)
          END IF
        END DO
        CALL DW_SET_CO
      END IF
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      CALL DQRU(HRB,VRB)
      CALL DGLEVL(L1)
      CALL DQPO0('AREA',L1,1,' ')
      RM=0.5*(R1+R2)
      A=A0
      DO L=1,6
        SA=SIND(A)
        CA=COSD(A)
        H(L)=RM*SA
        V(L)=RM*CA
        A=A+60.
      END DO
      CALL DQPOL(6,H,V)
      A=A0
      SA=SIND(A)
      CA=COSD(A)
      DO L=1,12,M
        CALL DQPO0('AREA',LCOL(L),LF,' ')
        CALL DGLEVL(LCOL(L))
        H(1)=R1*SA
        V(1)=R1*CA
        H(2)=R2*SA
        V(2)=R2*CA
        A=A+DA(M)
        SA=SIND(A)
        CA=COSD(A)
        H(3)=R2*SA
        V(3)=R2*CA
        H(4)=R1*SA
        V(4)=R1*CA
        CALL DQPOL(4,H,V)
      END DO
      END
*DK DBTPP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBTPP
CH
      SUBROUTINE DBTPP(HL8,VL8,MO,DS,DR,NU,HL1,VL1,HL2,VL2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
      DATA QL/0.5/
      HL9=QL*HL8
      VL9=QL*VL8
      QN=NU+1
      DHL=(HL2-HL1)/QN
      DVL=(VL2-VL1)/QN
      HL=HL1
      VL=VL1
      DO N=0,NU+1
        IF(     MO.EQ.0) THEN
          HP2=HL
          VP2=VL
        ELSE IF(MO.EQ.1) THEN
          HP2=HL*DR/(1.+DS*HL)
          VP2=VL
        ELSE IF(MO.EQ.2) THEN
          HP2=HL
          VP2=VL*DR/(1.+DS*VL)
        ELSE IF(MO.EQ.3) THEN
          HP2=HL*DR/(1.+DS*HL)
          VP2=VL*DR/(1.+DS*VL)
        ELSE IF(MO.EQ.4) THEN
          IF(HL.EQ.0..AND.VL.EQ.0.) THEN
            HP2=0.
            VP2=0.
          ELSE
            HP2=HL*DR/(1.+DS*HL)
            VP2=VL*DR/(1.+DS*VL)
            RP2=SQRT(HP2**2+VP2**2)
            IF(HL.GT.VL) THEN
C             ............................................. END CAP
              HLE=HL9
              VLE=HLE*VL/HL
            ELSE
C             ............................................. BARREL
              VLE=VL9
              HLE=VLE*HL/VL
            END IF
            HPE=HLE*DR/(1.+DS*HLE)
            VPE=VLE*DR/(1.+DS*VLE)
            FPE=ATAN2(VPE,HPE)
            HP2=RP2*COS(FPE)
            VP2=RP2*SIN(FPE)
          END IF
        ELSE
          R=SQRT(HL**2+VL**2)
          HP2=HL*DR/(1.+DS*R)
          VP2=VL*DR/(1.+DS*R)
        END IF
        IF(N.GT.0) CALL DQL2E(HP1,VP1,HP2,VP2)
        HP1=HP2
        VP1=VP2
        HL=HL+DHL
        VL=VL+DVL
      END DO
      END
*DK DBT_MAGIC_EYE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_MAGIC_EYE
CH
      SUBROUTINE DBT_MAGIC_EYE_1(J,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION DH(0:2)
      DIMENSION IC(0:1,0:1),SZ(0:1)
C
      IF(J.EQ.0) PR(2,1)=9.
      SZ(0)=PR(2,1)                   ! SZ  size 1
C
      IF(J.EQ.0) PR(2,7)=12.
      SZ(1)=PR(2,7)                   ! SH  size 2
C
      IF(J.EQ.0) PR(2,6)=7.
      LA=PR(2,6)                      ! CA  areac color
C
      IF(J.EQ.0) THEN
        PR(2,2)=1.
        PR(4,2)=1.
      END IF
      LR=PR(2,2)                      ! CR  frame color
      IF(PR(4,2).LT.0) LR=-1
C
      IF(J.EQ.0) PR(2,15)=10.
      DV=PR(2,15)+MAX(SZ(0),SZ(1))    ! DV  VERTICAL DISTANCE
C
      IF(J.EQ.0) PR(2,10)=60.
      DH(0)=PR(2,10)                  ! H1  HORIZONTA DISTANCE 1
C
      IF(J.EQ.0) PR(2,11)=66.
      DH(1)=PR(2,11)                  ! H2                     2
C
      IF(J.EQ.0) PR(2,12)=63.
      DH(2)=PR(2,12)                  ! DH                     3
C
      IF(J.EQ.0) PR(2,16)=12.
      IC(0,0)=PR(2,16)                ! P1  ROW N COLOR 1
C
      IF(J.EQ.0) PR(2,17)=9.
      IC(0,1)=PR(2,17)                ! P2  ROW N COLOR 2
C
      IF(J.EQ.0) PR(2,18)=14.
      IC(1,0)=PR(2,18)                ! PL  ROW N+1 COLOR 1
      IC(1,1)=PR(2,18)                ! PL  ROW N+2 COLOR 2
C
      H1=HMINDG(IAREDO)
      V1=VMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      V=V1
      DO IV=1,99
        V=V+DV
        IF(V.GT.V2-DV) GO TO 8
        H=H1
        MH=MOD(IV,3)
        MC=MOD(IV,2)
        DO IH=1,99
          H=H+DH(MH)
          IF(H.GT.H2-DV) GO TO 9
          NC=MOD(IH,2)
          CALL DGLEVL(IC(MC,NC))
          CALL DQFAR(H,V,H+SZ(MC),V+SZ(MC))
          IF(LR.GE.0) CALL DGLEVL(LR)
          CALL DQDAR(H,V,H+SZ(MC),V+SZ(MC))
        END DO
    9 END DO
    8 CALL DQFR(IAREDO)
      END
*DK DBT_MAGIC_EYE_2
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_MAGIC_EYE_2
CH
      SUBROUTINE DBT_MAGIC_EYE_2(J,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION IC(6)
      CHARACTER *60 TC
C
      IF(J.EQ.0) PR(2,1)=1.
      FR=PR(2,1)                      ! SZ=FRAME SIZE
C
      IF(J.EQ.0) THEN
        PR(2,7)=10.
        PR(4,7)=-1.
      END IF
      IF(PR(4,7).GT.0.) THEN          ! SH=SIZE
        SIZ=PR(2,7)
      ELSE
        SIZ=-1.
      END IF
C
      IF(J.EQ.0) PR(2,6)=7.
      LA=PR(2,6)                      ! CA  areac color
C
      IF(J.EQ.0) THEN
        PR(2,2)=1.
        PR(4,2)=-1.
      END IF
      IF(PR(4,2).GT.0) THEN
        LR=PR(2,2)                    ! CR  frame color
      ELSE
        LR=-1
      END IF
C
      IF(J.EQ.0) PR(2,16)=1.
      IND=PR(2,16)                    ! P1  INDEX
C
      DO N=1,6                        ! H1 - DV COLORS
        I=9+N
        IF(J.EQ.0) PR(2,I)=I-1
        IF(PR(2,I).GE.0..AND.PR(2,I).LE.16.) THEN
          IC(N)=PR(2,I)
        ELSE
          IC(N)=8
        END IF
      END DO
C
      H1=HMINDG(IAREDO)
      V1=VMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      V=V1
      CALL DGOPEN(NUNIDU,TFILDC//'MAGIC_EYE',2,*99,IER)
    1 READ(NUNIDU,1002,END=9) TXTADW
 1002 FORMAT(A)
      IF(TXTADW.EQ.' ') GO TO 1
      READ(TXTADW,1000,ERR=1) IN,H0,DH,DV,SZ,TC
C 1000 FORMAT(I,4F,1X,A)
 1000 FORMAT(I1,2X,4(1X,F3.0),1X,A)
      IF(IN.NE.IND) GO TO 1
      IF(SIZ.GT.0.) SZ=SIZ
      SS=SZ+2.*FR
      DV=DV+SS
      DH=DH+SS
      V=V+DV
      IF(V.GE.V2-SS) GO TO 9
      SS=SS+3.
      H=H1+H0+SS
      DO L=1,60
        IF(TC(L:L).EQ.' ') GO TO 1
        READ(TC(L:L),1001,ERR=1) JC
 1001   FORMAT(I1)
        IF(JC.GE.1.AND.JC.LE.6) THEN
          IF(LR.GE.0) THEN
            CALL DGLEVL(LR)
            CALL DQFAR(H-FR,V-FR,H+SZ+FR,V+SZ+FR)
          END IF
          CALL DGLEVL(IC(JC))
          CALL DQFAR(H,V,H+SZ,V+SZ)
        END IF
        H=H+DH
        IF(H.GE.H2-SS) GO TO 1
      END DO
      GO TO 1
    9 CLOSE(UNIT=NUNIDU)
   99 CALL DQFR(IAREDO)
      END
*DK DBT_RANDOM_STEREO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_RANDOM_STEREO
CH
      SUBROUTINE DBT_RANDOM_STEREO(J,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DATA IRAN/735289/
      DATA DD/20./
      IF(J.EQ.0) THEN
        PR(2,1)=8.                    ! SZ  step size
        PR(2,7)=6.                    ! SH  SYMBOL SIZE
        PR(2,5)=7.                    ! CO  area color
        PR(2,6)=12.                   ! CA  symbol color
        PR(2,2)=10.                   ! CR  inner symbol color
        PR(4,2)=1.
        PR(2,8)=8.                    ! FR  window frame color
        PR(2,9)=0.                    ! TO  inner window frame color
        PR(2,10)=240.                 ! H1  eye distance
        PR(2,3)=8.                    ! NU  stereo size
        PR(2,12)=1.                   ! DH  depth = #of shifts
        PR(2,4)=0.4                   ! RN  RANDOM CUT
      END IF
      SZ=PR(2,1)                      ! SZ  step size
      SY=PR(2,7)                      ! SY  symbol size
      LA=PR(2,5)                      ! CO  areac color
      LS=PR(2,6)                      ! CA  symbol color
      IF(PR(4,2).GT.0.) THEN
        LI=PR(2,2)                    ! CR  inner symbol color
      ELSE
        LI=LS
      END IF
      LF=PR(2,8)                      ! FR  window frame color
      JF=PR(2,9)                      ! TO  inner window frame color
      HD=PR(2,10)-DD                  ! H1  eye distance
      NU=PR(2,3)/2.                   ! NU  stereo size
      G3=PR(2,12)                     ! DH  depth = #of shifts
      RC=PR(2,4)                      ! RN  RANDOM CUT
      IF(IAREDO.NE.7.OR.IAREDO.NE.8) IAREDO=8
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      CALL DQFFWI(LA)
      H1=HMINDG(IAREDO)
      V1=VMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      V=V1+SZ
      NV=0
      CALL DGLEVL(LS)
      DO V=V1+SZ,V2-SZ,SZ
        NV=NV+1
        NH=0
        DO HL=H1+SZ,H1+HD-SZ,SZ
          IF(RAN(IRAN).GE.RC) THEN
            HR=HL+HD+DD
            CALL DQFAR(HL,V,HL+SY,V+SY)
            CALL DQFAR(HR,V,HR+SY,V+SY)
          END IF
          NH=NH+1
        END DO
      END DO
      IF(LF.GT.0) THEN
        CALL DGLEVL(LF)
        CALL DQDAR(H1,V1,H1+HD,V2)
        CALL DQDAR(H1+HD+DD,V1,H1+2.*HD+DD,V2)
      END IF
      NH=NH/2
      NV=NV/2
      NUV=FLOAT(NU)*(V2-V1)/HD
      IH=MIN(NH/2,NU )
      IV=MIN(NV/2,NUV)
      HM=H1+(NH+1)*SZ
      VM=H1+(NV+1)*SZ
      H1=HM-IH*SZ
      H2=HM+IH*SZ
      V1=VM-IV*SZ
      V2=VM+IV*SZ
      S3=SZ*33
      CALL DGLEVL(LA)
      CALL DQFAR(H1,V1,H2+SZ,V2+SZ)
      CALL DGLEVL(LI)
      IR=IRAN
      DO V=V1,V2,SZ
        DO H=H1,H2,SZ
          IF(RAN(IRAN).GE.RC) CALL DQFAR(H,V,H+SY,V+SY)
        END DO
      END DO
      IF(JF.GT.0) THEN
        CALL DGLEVL(JF)
        CALL DQDAR(H1,V1,H2+SZ,V2+SZ)
      END IF
      H1=H1+HD+DD-G3*SZ
      H2=H2+HD+DD-G3*SZ
      CALL DGLEVL(LA)
      CALL DQFAR(H1,V1,H2+SZ,V2+SZ)
      CALL DGLEVL(LI)
      IRAN=IR
      DO V=V1,V2,SZ
        DO H=H1,H2,SZ
          IF(RAN(IRAN).GE.RC) CALL DQFAR(H,V,H+SY,V+SY)
        END DO
      END DO
      IF(JF.GT.0) THEN
        CALL DGLEVL(JF)
        CALL DQDAR(H1,V1,H2+SZ,V2+SZ)
      END IF
      CALL DQFR(IAREDO)
      END
*DK DBCBSQ
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBCBSQ
CH
      SUBROUTINE DBCBSQ(MO,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 T
      PARAMETER (MC=8)
      DIMENSION HS1(9),VS1(9),HS2(9),VS2(9),LS(9),JS(9)
      DIMENSION HY1(9),VY1(9),HY2(9),VY2(9),LY(9),JY(9)
C               1   2   3    4   5    6  7,8,9
      DATA HY1/ 2., 7.,13.3,13.5, 3.5, 4., 3*0./
      DATA VY1/ 2., 9.,14.3, 4.5,13.5,14., 3*0./
      DATA HY2/ 6.,11.,17.3,18.5, 8.5, 8., 3*0./
      DATA VY2/ 5.,12.,17.3, 8.5,17.5,17., 3*0./
      DATA  LY/ 7 , 7., 7. , 1  , 1  , 7 , 3*0 /
      DATA  JY/ 2., 1., 1. , 2  , 2  , 1 , 3*0 /
      DIMENSION RD(MC),GR(MC),BL(MC),IC(MC)
      DATA IC/ 8 , 9 , 10, 14, 7 , 13, 12, 15/
      DATA RD/.86, 0.,.89, 0.,.5 ,.55,.65, 0./
      DATA GR/.86, 1.,.89,.95,.5 , 0., 0., 0./
      DATA BL/.86, 0., 0.,.95,.5 ,.55, 0., 1./
      DATA NS/0/,MS/0/,DT/4./,D2/2./,DR/0.5/
      DATA HSOLD,VSOLD,DHOLD,DVOLD/4*-9/
      LOGICAL FIN
      IF(MO.EQ.0) THEN
        DO L=1,MC
          I=IC(L)
          RDCODD(I)=RD(L)
          GRCODD(I)=GR(L)
          BLCODD(I)=BL(L)
        END DO
        CALL DW_SET_CO
        PR(2, 1)=2. ! SZ=WIDTH
        PR(2, 2)=1. ! CR=FRAME
        PR(4, 2)=-1.
        PR(2, 5)=7.! CO=SQUARE COLOR
        PR(2, 6)=1. ! CA=AREA C.
        PR(2, 7)=8. ! SH=LINE COLOR
        PR(2,10)=20.! H1,V1 = RANGE
        PR(2,13)=20.!
        PR(2,11)=5. ! H2,V2 = SQAURE CORNER
        PR(2,14)=7. !
        PR(2,12)=4. ! DH=SQAURE LENGTH
        PR(2,15)=3. ! DV=SQUARE HIGHT
        PR(2,16)=6. ! P1=# OF SQUARES
        PR(2,17)=1. ! P2: 1=LINES 2=AREA
        MS=6
        NS=6
        DO K=1,MS
          HS1(K)=HY1(K)
          VS1(K)=VY1(K)
          HS2(K)=HY2(K)
          VS2(K)=VY2(K)
          LS( K)=LY( K)
          JS( K)=JY( K)
        END DO
      ELSE IF(MO.EQ.2) THEN
        DO K=1,MS
          IF(LS(K).NE.1) LS(K)=PR(2,5)
        END DO
      END IF
C     ................................................... SZ line width
      DS=PR(2,1)
C     ................................................... SH line color
      LIN=PR(2,7)
C     ................................................... CA background color
      LBG=PR(2,6)
C     ................................................... CR frame color
      LFR=PR(2,2)
C     ................................................... CO square color
      LSQ=PR(2,5)
C     ................................................... P1 # OF SQUARES<9
      NSQ=MIN(9.,PR(2,16))
C     ................................................... H1,V1 =RANGE
      HR=PR(2,10)
      VR=PR(2,13)
C     ................................................... H2,V2 SQUARE CORNER
      HS=PR(2,11)
      VS=PR(2,14)
C     ................................................... DH,DV SQUARE SIZE
      DH=PR(2,12)
      DV=PR(2,15)
C     .................................................... LINES OR AREA
      IS=PR(2,17)
C
      CALL DQWIL(0.)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      CALL DQRER(0,-DR,-DR,HR+DR,VR+DR,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(NSQ.LE.0) THEN
        NS=0
        MS=0
      ELSE
        IF(HSOLD.NE.HS.OR.
     &     VSOLD.NE.VS.OR.
     &     DHOLD.NE.DH.OR.
     &     DVOLD.NE.DV) THEN
          HSOLD=HS
          VSOLD=VS
          DHOLD=DH
          DVOLD=DV
          NS=MOD(NS,NSQ)+1
          MS=MIN(MS+1,NSQ)
          HS1(NS)=HS
          VS1(NS)=VS
          HS2(NS)=HS+DH
          VS2(NS)=VS+DV
        END IF
        LS(NS)=LSQ
        JS(NS)=IS
      END IF
      CALL DQAR0(0.,0.)
      DLIN=DLINDD
C
      IF(PR(4,2).GT.0.) THEN
        DLINDD=DS+D2
        CALL DGLEVL(LFR)
        DO H=1.,HR-1.
          CALL DQL2E(H,0.,H,VR)
        END DO
        DO V=1.,VR-1.
          CALL DQL2E(0.,V,HR,V)
        END DO
        IF(PR(2,17).LE.1.) THEN
          DO M=1,MS
            CALL DQAR(HS1(M),VS1(M),HS2(M),VS2(M))
          END DO
        END IF
      END IF
C
      DLINDD=DS
      CALL DGLEVL(LIN)
      NT=0
      DO H=0.,HR
        CALL DQL2E(H,0.,H,VR)
        NT=NT+1
        WRITE(T,1000) NT
 1000   FORMAT(I2)
        CALL DQPOC(H,0.,HD,VD,FIN)
        CALL DGTEXT(HD+DT,VD+DT,T,2)
      END DO
      NT=0
      DO V=0.,VR
        CALL DQL2E(0.,V,HR,V)
        NT=NT+1
        WRITE(T,1000) NT
        CALL DQPOC(0.,V,HD,VD,FIN)
        CALL DGTEXT(HD+DT,VD+DT,T,2)
      END DO
      DO M=1,MS
        CALL DGLEVL(LS(M))
        IF(JS(M).LE.1.) THEN
          CALL DQREC(HS1(M),VS1(M),HS2(M),VS2(M))
        ELSE
          CALL DQAR( HS1(M),VS1(M),HS2(M),VS2(M))
        END IF
      END DO
      PR(2,3)=MS
      DLINDD=DLIN
      END
*DK DBCBTX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBCBTX
CH
      SUBROUTINE DBCBTX(MO,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DATA H1,V1,H2,V2/0.,0.,22.,7./,V0/0./
      DIMENSION HS1(9),HS2(9)
      DATA HS1/ 7., 1., 7., 9.,17.,11.,13., 9.,23./
      DATA HS2/ 0., 8., 7., 9., 3.,11.,13.,23., 9./
      DATA VS1/0./,VS2/7./,D1/1./,D2/2./,I8/8/,DL1/1./
      DIMENSION ICOL(7)
      DATA ICOL/8.,14.,9.,10.,8.,14.,9./
      DIMENSION RD(5),GR(5),BL(5),IC(5)
      DATA IC/ 8 , 9 , 10, 14, 7 /
      DATA RD/.86, 0.,.89, 0.,.3 /
      DATA GR/.86, 1.,.89,.95,.3 /
      DATA BL/.86, 0., 0.,.95,.3 /
      IF(MO.EQ.0) THEN
        DO L=1,5
          I=IC(L)
          RDCODD(I)=RD(L)
          GRCODD(I)=GR(L)
          BLCODD(I)=BL(L)
        END DO
        CALL DW_SET_CO
        PR(2, 1)=2. ! SZ=WIDTH
        PR(2, 2)=1. ! CR=FRAME
        PR(4, 2)=-1.
        PR(2, 5)=7. ! CO=REGION COLOR
        PR(2, 6)=1. ! CA=AREA C.
        PR(2, 7)=8. ! SH=HORIZONTAL LINE COLOR
        PR(4, 7)=-1.
        PR(2,16)=0. ! P1=MODE
        PR(2,17)=8. ! P2=# OF POINTS
      END IF
C     ................................................... SZ line width
      DS=PR(2,1)
C     ................................................... CA background color
      LBG=PR(2,6)
C     ................................................... CR frame color
      LFR=PR(2,2)
C     ................................................... CO region color
      LRG=PR(2,5)
C     ................................................... SH h. line color
      LHL=PR(2,7)
C     ................................................... P1 1,2,3 mode
      NTY=PR(2,16)
C     ................................................... P1 1,2,3 symbol
      PNT=PR(2,17)
      NPR=PNT
      CALL DQWIL(0.)
      CALL DQCL(IAREDO)
      CALL DQFFWI(LBG)
      CALL DQRER(0,H1,V1,H2,V2,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(NTY.GE.2) THEN
        CALL DQAR0(0.,0.)
        CALL DGLEVL(LRG)
        DO V=V0,6.,2.
          CALL DQAR(H1,V,H2,V+1.)
        END DO
      END IF
      DLIN=D1
      IF(PR(4,7).GT.0.) THEN
        CALL DGLEVL(LHL)
        DO V=1.,6.,1.
          CALL DQL2E(H1,V,H2,V)
        END DO
      END IF
      IF(PNT.GT.1.) THEN
        IF(PR(4,2).GT.0.) THEN
          CALL DQPD0(I8,DS+D2,0.)
          CALL DGLEVL(LFR)
          DO K=1,9
            Q=(HS2(K)-HS1(K))/(VS2-VS1)
            VR1=0.
            VR2=1.
            DO N=1,7
              HR1=HS1(K)+Q*(VR1-VS1)
              HR2=HS1(K)+Q*(VR2-VS1)
              DH=(HR2-HR1)/PNT
              DV=(VR2-VR1)/PNT
              HP1=HR1
              VP1=VR1
              DO I=1,NPR
                CALL DQPD(HP1,VP1)
                HP1=HP1+DH
                VP1=VP1+DV
              END DO
              VR1=VR1+1.
              VR2=VR2+1.
            END DO
          END DO
        END IF
        CALL DQPD0(I8,DS,0.)
        CALL DGLEVL(ICOL(1))
        DO K=1,9
          Q=(HS2(K)-HS1(K))/(VS2-VS1)
          VR1=0.
          VR2=1.
          DO N=1,7
            IF(NTY.EQ.1) CALL DGLEVL(ICOL(N))
            HR1=HS1(K)+Q*(VR1-VS1)
            HR2=HS1(K)+Q*(VR2-VS1)
            DH=(HR2-HR1)/PNT
            DV=(VR2-VR1)/PNT
            HP1=HR1
            VP1=VR1
            DO I=1,NPR
              IF(NTY.EQ.1.AND.N.EQ.5.AND.I.EQ.1) THEN
                CALL DGLEVL(ICOL(4))
                CALL DQPD(HP1,VP1)
                CALL DGLEVL(ICOL(5))
              ELSE
                CALL DQPD(HP1,VP1)
              END IF
              HP1=HP1+DH
              VP1=VP1+DV
            END DO
            VR1=VR1+1.
            VR2=VR2+1.
          END DO
        END DO
      ELSE
        DLIN=DLINDD
        IF(PR(4,2).GT.0.) THEN
          DLINDD=DS+D2
          CALL DGLEVL(LFR)
          DO K=1,9
            CALL DQL2E(HS1(K),VS1,HS2(K),VS2)
          END DO
        END IF
        DLINDD=DS
        CALL DGLEVL(ICOL(1))
        DO K=1,9
          Q=(HS2(K)-HS1(K))/(VS2-VS1)
          VR1=0.
          VR2=1.
          DO N=1,7
            IF(NTY.EQ.1) CALL DGLEVL(ICOL(N))
            HR1=HS1(K)+Q*(VR1-VS1)
            HR2=HS1(K)+Q*(VR2-VS1)
            CALL DQL2E(HR1,VR1,HR2,VR2)
            VR1=VR1+1.
            VR2=VR2+1.
          END DO
        END DO
      END IF
      CALL DQFR(IAREDO)
      DLINDD=DLIN
      END
*DK DBT_SMOOTH_ROT
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_SMOOTH_ROT
CH
      SUBROUTINE DBT_SMOOTH_ROT(ISTRT,PR)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
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
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DATA L8/8/,DS/20./,DA/5./,DL1/1./,DL2/3./
      DATA Q1/0.0/,Q2/0.000000005/,DC/20./
      DATA JEND/99999/,JPIC/20/,WLONG/0.04/
      DIMENSION LTR(8:14)
      DATA LTR/8,9,10,11,12,13,14/
      DATA FSQ,TSQ,ASQ/30.,45.,10./
      DATA LAX/8/,DLAX/1./
      DIMENSION XSQ(8),YSQ(8),ZSQ(8),NSQ(12,2),LSQ(12)
      DIMENSION HSR(8),USR(8),WSR(8)
      DIMENSION HSQ(2,12),VSQ(2,12),USQ(2,12),WSQ(2,12),DSQ(2,12)
C               1   2   3   4   5   6   7   8
      DATA XSQ/ 1., 1.,-1.,-1., 1., 1.,-1.,-1./
      DATA YSQ/ 1.,-1.,-1., 1., 1.,-1.,-1., 1./
      DATA ZSQ/-1.,-1.,-1.,-1., 1., 1., 1., 1./
      DATA NSQ/ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4,
     &          2, 3, 4, 1, 6, 7, 8, 5, 5, 6, 7, 8/
C     DATA LSQ/ 7, 8, 8, 7, 8, 8, 8, 8, 7, 8, 8, 8/
      DATA LSQ/ 9, 9, 9, 9,10,10,10,10,13,14,12,8/
C               1  2  3  4  5  6  7  8  9 10 11 12
      DIMENSION NSIDE(4,2),LCS(0:4)                ! ?SIDE(side , # of line)
C                GB BC CM MG
      DATA NSIDE/ 9,10,11,12,
     &           10,11,12, 9/
      DATA LCS/6,1,5,1,5/,AL0/0/
      DIMENSION LBK(-1:1)
      DATA ICUBE/1/,ICS/1/,ICF/1/
      DATA IRAN/0/,IRAN0/945631/,RLI/1.8/,RLP/2./,RT1/0.4/,RT2/0.2/
      PARAMETER (MLI=99)
      DIMENSION ULI(MLI),WLI(MLI),HLI(MLI),VLI(MLI),LS(MLI)
      DIMENSION NDIR(2,MLI),HD(8),VD(8),HH(2),VV(2)
      DATA LIMO/3/,LEND/-1/,QX/0.03/
      DATA GL/100./
      LOGICAL F1,FBUT(3),F2PIC,FHAX,DGPNTR
      DATA FHAX/.TRUE./
      DIMENSION GSA(10)
      DATA GSA/0.5,1.,2.,3.,4.,6.,10.,14.,22.,38./
      DIMENSION RDCO(2),GRCO(2),BLCO(2)
      DATA RDCO/0.35,0.  /
      DATA GRCO/0.  ,0.  /
      DATA BLCO/0.  ,0.4 /
      CALL DCDST0
      CALL UCOPY(RDCO,RDCODD(5),2)
      CALL UCOPY(GRCO,GRCODD(5),2)
      CALL UCOPY(BLCO,BLCODD(5),2)
      CALL DW_SET_CO
      IF(ISTRT.EQ.0) THEN
        PR(2,1)=4.
        PR(2,2)=1.
        PR(2,3)=10.
        PR(2,4)=1.
        PR(4,4)=-1.
        PR(2,6)=7.
        PR(2,7)=2.
        PR(2,16)=2.
        PR(2,17)=300.
      END IF
      DAR=PR(2,1)       ! SZ
      LBK(-1)=PR(2,2)   ! CR color if no cube
      LBK( 1)=PR(2,6)   ! CA color if cube
      PR(2,3)=MIN(FLOAT(MLI),PR(2,3))
      NLI=PR(2,3)       ! NU
      DLT=PR(2,7)
      DLIN=DLINDD
      GS=GSA(2)
      IF(PR(4,4).EQ.-1.) PR(2,4)=PR(2,4)+1.
      JRA=PR(2,4)       ! RN
      IRAN=IRAN0
      DO J=1,JRA
        R=RAN(IRAN)
      END DO
      JRAN=IRAN
      IDEMO=-1
      IF(ICF.GT.0.AND.ICS.GT.0) THEN
        LBG=LBK(ICUBE)
      ELSE
        LBG=LBK(-1)
      END IF
      IF(PR(2,16).EQ.2.) THEN
        F2PIC=.TRUE.
        FHAX=.TRUE.
      ELSE
        F2PIC=.FALSE.
      END IF
C     IF(IAREDO.NE.12.AND.IAREDO.NE.2.AND.IAREDO.NE.3) FHAX=.TRUE.
      JSTOP=0
      V1=VMINDG(0)
      V2=VHGHDG(13)
      VM=0.5*(V2+V1)
      CALL DGSCUR(DC,VM)
    2 H1=HMINDG(0)
      H2=HHGHDG(13)
      IF(F2PIC) THEN
        H3=H2
        H2=0.5*(H1+H2)
        HMPIC=0.5*(H2+H3)
      END IF
      HM=0.5*(H2+H1)
      DH=0.5*(H2-H1)
      DV=0.5*(V2-V1)
      IF(F2PIC) THEN
        DV=DH
        V0=VM-DV
        V3=VM+DV
      END IF
      CALL DQ_DRAW_HV_IN(HM,VM,FHAX)
      CALL DQ_AREA_HV_IN(HM,VM,FHAX,DAR)
      DRMIN=MIN(DH,DV)
C     .................................. Setup artificial display CUBE
    1 CALL DGLEVL(LBG)
      IF(F2PIC) THEN
        CALL DQFAR(H1,V1,H2,V2)
        CALL DGLEVL(LBK(-1))
        CALL DQFAR(H2,V0,H3,V3)
        CALL DGLEVL(L8)
        CALL DQDAR(H2,V0,H3,V3)
      ELSE
        CALL DQFAR(H1,V1,H2,V2)
      END IF
      SF=SIND(FSQ)
      CF=COSD(FSQ)
      ST=SIND(TSQ)
      CT=COSD(TSQ)
      SG=SIND(ASQ)
      CG=COSD(ASQ)
      RMAX=0.
      HMAX=0.
      DO N=1,8
        XS1=XSQ(N)
        YS1=YSQ(N)
        ZS1=ZSQ(N)
        X2    = CF*XS1+SF*YS1
        Y2    =-SF*XS1+CF*YS1
        HSR(N)= CT*X2 +ST*ZS1
        Z3    =-ST*X2 +CT*ZS1
        USR(N)= CG*Y2 +SG*Z3
        WSR(N)=-SG*Y2 +CG*Z3
        HMAX=MAX(HMAX,ABS(HSR(N)))
        RMAX=MAX(RMAX,USR(N)**2+WSR(N)**2)
      END DO
      IF(FHAX) THEN
        RMAX=DV/SQRT(RMAX)
        HMAX=DH/HMAX
      ELSE
        RMAX=DH/SQRT(RMAX)
        HMAX=DV/HMAX
      END IF
C    1 RSQ=DRMIN*(RSQST+GS*RSQST*GZOOM)
C      DATA RSQST/0.29/
      RSQ=MIN(HMAX,RMAX)*GS
      DO N=1,12
        DO I=1,2
C          XS1=XSQ(NSQ(N,I))*RSQ
C          YS1=YSQ(NSQ(N,I))*RSQ
C          ZS1=ZSQ(NSQ(N,I))*RSQ
C          X2      = CF*XS1+SF*YS1
C          Y2      =-SF*XS1+CF*YS1
C          HSQ(I,N)= CT*X2 +ST*ZS1
C          Z3      =-ST*X2 +CT*ZS1
C          USQ(I,N)= CG*Y2 +SG*Z3
C          WSQ(I,N)=-SG*Y2 +CG*Z3
          HSQ(I,N)=HSR(NSQ(N,I))*RSQ
          USQ(I,N)=USR(NSQ(N,I))*RSQ
          WSQ(I,N)=WSR(NSQ(N,I))*RSQ
        END DO
      END DO
C     .................................. setup tracks
      IF(NLI.GT.0) THEN
        IRAN=JRAN
        DO N=1,NLI
          RT=RAN(IRAN)
          IF(RT.GT.RT1) THEN
            XL1=RLI*(RAN(IRAN)-0.5)*RAN(IRAN)*RSQ
            YL1=RLI*(RAN(IRAN)-0.5)*RAN(IRAN)*RSQ
            IF(RAN(IRAN).GT.0.5) THEN
              ZL1= RSQ
              LS(N)=LTR(10)
            ELSE
              ZL1=-RSQ
              LS(N)=LTR(9)
            END IF
            NDIR(1,N)=1
            NDIR(2,N)=2
          ELSE IF(RT.GT.RT2) THEN
            ZL1=RLP*(RAN(IRAN)-0.5)*RSQ
            YL1=RLP*(RAN(IRAN)-0.5)*RSQ
            IF(RAN(IRAN).GT.0.5) THEN
              XL1= RSQ
              LS(N)=LTR(12)
            ELSE
              XL1=-RSQ
              LS(N)=LTR(11)
            END IF
            NDIR(1,N)=1
            NDIR(2,N)=9
          ELSE
            XL1=RLP*(RAN(IRAN)-0.5)*RSQ
            ZL1=RLP*(RAN(IRAN)-0.5)*RSQ
            IF(RAN(IRAN).GT.0.5) THEN
              YL1= RSQ
              LS(N)=LTR(13)
            ELSE
              YL1=-RSQ
              LS(N)=LTR(14)
            END IF
            NDIR(1,N)=2
            NDIR(2,N)=9
          END IF
          X2    = CF*XL1+SF*YL1
          Y2    =-SF*XL1+CF*YL1
          HLI(N)= CT*X2 +ST*ZL1
          Z3    =-ST*X2 +CT*ZL1
          ULI(N)= CG*Y2 +SG*Z3
          WLI(N)=-SG*Y2 +CG*Z3
        END DO
      END IF
      VLS=VM-DS
      VHS=VM+DS
      VLA=VM-DA
      VHA=VM+DA
      AL= AL0
      CALL DWRT('Move mouse.')
   11 IF(ICF.GT.0.AND.ICS.GT.0) THEN
        LBG=LBK(ICUBE)
      ELSE
        LBG=LBK(-1)
      END IF
   10 SG=SIND(AL)
      CG=COSD(AL)
      IF(ICUBE.GT.0) THEN
        DMIN=999.
        DO N=1,12
          DO I=1,2
            VSQ(I,N)= CG*USQ(I,N)+SG*WSQ(I,N)
            DSQ(I,N)=-SG*USQ(I,N)+CG*WSQ(I,N)
            DMIN=MIN(DSQ(I,N),DMIN)
          END DO
        END DO
        IF(ICS.GT.0) THEN
          DO N=1,4
            DEPTH=DSQ(1,NSIDE(N,1))+DSQ(2,NSIDE(N,2))
            IF(DEPTH.LT.0.) THEN
              HD(1)=HSQ(1,NSIDE(N,1))
              HD(2)=HSQ(2,NSIDE(N,1))
              HD(3)=HSQ(2,NSIDE(N,2))
              HD(4)=HSQ(1,NSIDE(N,2))
              VD(1)=VSQ(1,NSIDE(N,1))
              VD(2)=VSQ(2,NSIDE(N,1))
              VD(3)=VSQ(2,NSIDE(N,2))
              VD(4)=VSQ(1,NSIDE(N,2))
              CALL DGLEVL(LCS(N))
              CALL DQ_AREA_HV(4,HD,VD)
            END IF
          END DO
        END IF
        IF(ICF.GT.0) THEN
          CALL DGLEVL(LCS(0))
          DEPTH=DSQ(1,1)+DSQ(1,3)
          IF(DEPTH.LT.0.) THEN
            DO K=1,4
              HD(K)=HSQ(1,K)
              VD(K)=VSQ(1,K)
            END DO
            CALL DQ_AREA_HV(4,HD,VD)
          ELSE
            DO K=5,8
              HD(K)=HSQ(1,K)
              VD(K)=VSQ(1,K)
            END DO
            CALL DQ_AREA_HV(4,HD(5),VD(5))
          END IF
        END IF
        DO N=1,12
          CALL DGLEVL(LSQ(N))
          DLINDD=DL2
          DO I=1,2
            IF(DSQ(I,N).EQ.DMIN) DLINDD=DL1
          END DO
          CALL DQ_DRAW_HV(2,HSQ(1,N),VSQ(1,N))
        END DO
      END IF
      DLINDD=DLT
      IF(NLI.GT.0) THEN
        CALL DQ_DRAW_HV_0(0.,0.)
        LLI=9
        HH(1)=HMPIC
        VV(1)=VM
        DO N=1,NLI
          IF(LLI.GE.15) THEN
            LLI=9
          ELSE
            LLI=LLI+1
          END IF
          CALL DGLEVL(LLI)
          VLI(N)= CG*ULI(N)+SG*WLI(N)
          IF(LEND.LE.0) THEN
            CALL DQ_DRAW_HV_1(HLI(N),VLI(N))
          ELSE
            CALL DQ_DRAW_HV_1(GL*HLI(N),GL*VLI(N))
          END IF
          IF(F2PIC) THEN
            HHL=GL*HLI(N)
            VVL=GL*VLI(N)
            IF(ABS(HHL).LT.DH.AND.ABS(VVL).LT.DV) THEN
              HH(2)=HHL+HMPIC
              VV(2)=VVL+VM
            ELSE
              IF(HHL.GT.0.) THEN
                IF(VVL.GT.0.) THEN
                  IF(HHL.GT.VVL) THEN
                    HH(2)=DH
                    VV(2)=DH*VVL/HHL
                  ELSE
                    VV(2)=DV
                    HH(2)=DV*HHL/VVL
                  END IF
                ELSE
                  IF(HHL.GT.-VVL) THEN
                    HH(2)=DH
                    VV(2)=DH*VVL/HHL
                  ELSE
                    VV(2)=-DV
                    HH(2)=-DV*HHL/VVL
                  END IF
                END IF
              ELSE
                IF(VVL.GT.0.) THEN
                  IF(-HHL.GT.VVL) THEN
                    HH(2)=-DH
                    VV(2)=-DH*VVL/HHL
                  ELSE
                    VV(2)=DV
                    HH(2)=DV*HHL/VVL
                  END IF
                ELSE
                  IF(-HHL.GT.-VVL) THEN
                    HH(2)=-DH
                    VV(2)=-DH*VVL/HHL
                  ELSE
                    VV(2)=-DV
                    HH(2)=-DV*HHL/VVL
                  END IF
                END IF
              END IF
              HH(2)=HH(2)+HMPIC
              VV(2)=VV(2)+VM
            END IF
            CALL DGDRAW(2,HH,VV)
          END IF
          IF(LIMO.NE.0) THEN
            IF(LIMO.GE.2) CALL DGLEVL(LS(N))
            HAR=HLI(N)
            VAR=VLI(N)
            IF(LIMO.LT.3) THEN
              CALL DQ_AREA_HV_1(HAR,VAR)
            ELSE
              DO I=1,2
                DHS=QX*( HSQ(2,NDIR(I,N))-HSQ(1,NDIR(I,N)) )
                DVS=QX*( VSQ(2,NDIR(I,N))-VSQ(1,NDIR(I,N)) )
                CALL DQ_DRAW_HV_2(HAR-DHS,VAR-DVS,HAR+DHS,VAR+DVS)
              END DO
            END IF
          END IF
        END DO
      END IF
      CALL DGCHKX
      CALL DWAIT1(WLONG)
   20 IF(JSTOP.GT.JEND) GO TO 99
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
C       ...... BUTTON 1 DOWN = WAIT1               PRIORITY 3
C       ...... BUTTON 2 DOWN = INVERSE DIRECTION   PRIORITY 2
C       ...... BUTTON 3 DOWN = STOP                PRIORITY 1
        IF(          FBUT(3)) THEN
   22     CALL DTYANS(
     &      'Cube (B,F,S; 0-9), Long ,NTPX, Axis,RD, <CR>=stop',
     &      '0123456789NTPXCBFSLARD',NANSW)
C            123456789 123456789 12
          IF(     NANSW.EQ.0 ) THEN         ! <CR>
            GO TO 99
          ELSE IF(NANSW.LT.0 ) THEN         ! *
            GO TO 10
          ELSE IF(NANSW.LE.10) THEN         !0 1 2 3 4 5 6 7 8 9
            GS=GSA(NANSW)
            IF(F2PIC) GS=GSA(1)
            GO TO 1
          ELSE IF(NANSW.LE.14) THEN         ! N T P X
            LIMO=NANSW-11
          ELSE IF(NANSW.EQ.15) THEN         ! C
            ICUBE=-ICUBE
            GO TO 11
          ELSE IF(NANSW.EQ.16) THEN         ! B
           IF(ICF.EQ.-1.OR.ICS.EQ.-1) THEN
              ICF=1
              ICS=1
            ELSE
              ICF=-1
              ICS=-1
            END IF
            GO TO 11
          ELSE IF(NANSW.EQ.17) THEN         ! F
            ICF=-ICF
            GO TO 11
          ELSE IF(NANSW.EQ.18) THEN         ! S
            ICS=-ICS
            GO TO 11
          ELSE IF(NANSW.EQ.19) THEN         ! L
            LEND=-LEND
            IF(F2PIC) LEND=-1
          ELSE IF(NANSW.EQ.20) THEN         ! A
            IF(FHAX) THEN
              FHAX=.FALSE.
            ELSE
              FHAX=.TRUE.
            END IF
            IF(F2PIC) FHAX=.TRUE.
            CALL DQ_DRAW_HV_IN(HM,VM,FHAX)
            CALL DQ_AREA_HV_IN(HM,VM,FHAX,DAR)
          ELSE IF(NANSW.EQ.21) THEN         ! R
            GO TO 23
          ELSE IF(NANSW.EQ.22) THEN         ! D
            IF(IDEMO.LT.0) THEN
              GO TO 23
            ELSE IF(IDEMO.LT.6) THEN
              IDEMO=IDEMO+1
              GS=GSA(IDEMO+1)
            ELSE IF(IDEMO.EQ.6) THEN
              IDEMO=IDEMO+1
              GS=GSA(10)
            ELSE IF(IDEMO.EQ.7) THEN
              IDEMO=IDEMO+1
              LEND=1
              ICUBE=-1
              GO TO 10
            ELSE IF(IDEMO.EQ.8) THEN
              IDEMO=IDEMO+1
              ICUBE=1
              ICF=1
              ICS=1
              LIMO=4
              LEND=-1
              GS=GSA(1)
            ELSE IF(IDEMO.EQ.9) THEN
              IDEMO=IDEMO+1
              FHAX=.TRUE.
              F2PIC=.TRUE.
              GO TO 2
            ELSE IF(IDEMO.EQ.10) THEN
              GO TO 23
            END IF
            GO TO 1
          END IF
          GO TO 10
   23     F2PIC=.FALSE.
          FHAX=.TRUE.
          IDEMO=0
          ICUBE=1
          ICF=-1
          ICS=-1
          LEND=-1
          LIMO=1
          GS=GSA(1)
          GO TO 2
        ELSE IF(FBUT(2)) THEN
          JSTOP=0
          GO TO 21
        ELSE IF(FBUT(1)) THEN
          IF(FBUT(3)) GO TO 99
          JSTOP=0
          F1=.TRUE.
          GO TO 20
        END IF
      ELSE
        GO TO 99
      END IF
   21 HC=JHC
      VC=JVC
      IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
        JSTOP=0
        HCOLD=HC
        VCOLD=VC
      END IF
C     ............................................................. SPEED
      IF(VC.GT.VHS) THEN
        D=(VC-VHS)*(H2-HC)
        IF(FBUT(2)) THEN
          DA=-Q1*D-Q2*D*D
        ELSE
          DA= Q1*D+Q2*D*D
        END IF
      ELSE IF(VC.LT.VLS) THEN
        D=(VLS-VC)*(H2-HC)
        IF(FBUT(2)) THEN
          DA= Q1*D+Q2*D*D
        ELSE
          DA=-Q1*D-Q2*D*D
        END IF
      ELSE
        JSTOP=JSTOP+1
        GO TO 20
      END IF
      AL=AL+DA
      IF(AL.GE.360.) THEN
        AL=AL-360.
      ELSE IF(AL.LT.0.) THEN
        AL=AL+360.
      END IF
      WRITE(TXTADW,1001) AL
 1001 FORMAT('AL=',F6.2,'m')
      CALL DWR_OVER_PRINT(10)
      CALL DPARSV(15,'RAL',2,AL)
      CALL DGLEVL(LBG)
      IF(F2PIC) THEN
        CALL DQFAR(H1,V1,H3,V2)
        CALL DGLEVL(LBK(-1))
        CALL DQFAR(H2,V0,H3,V3)
        CALL DGLEVL(L8)
        CALL DQDAR(H2,V0,H3,V3)
      ELSE
        CALL DQFAR(H1,V1,H2,V2)
      END IF
      IF(LAX.GE.0) THEN
        DLINDD=DLAX
        CALL DGLEVL(LAX)
        CALL DQ_DRAW_HV_2(-DH,0.,3.*DH,0.)
      END IF
   98 JSTOP=JSTOP+JPIC
      GO TO 10
   99 DLINDD=DLIN
      CALL DCDSTC
      END
*DK DQ_DRAW_HV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQ_DRAW_HV
CH
      SUBROUTINE DQ_DRAW_HV(NN,HH,VV)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION HH(*),VV(*)
      DIMENSION H(20),V(20),HD(2),VD(2)
      LOGICAL FHAX,FH
      IF(FHAX) THEN
        DO N=1,NN
          H(N)=HM+HH(N)
          V(N)=VM+VV(N)
        END DO
      ELSE
        DO N=1,NN
          H(N)=HM+VV(N)
          V(N)=VM+HH(N)
        END DO
      END IF
      CALL DGDRAW(NN,H,V)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQ_DRAW_HV_0
CH
      ENTRY DQ_DRAW_HV_0(H1,V1)
CH
CH --------------------------------------------------------------------
      IF(FHAX) THEN
        HD(1)=HM+H1
        VD(1)=VM+V1
      ELSE
        HD(1)=HM+V1
        VD(1)=VM+H1
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQ_DRAW_HV_1
CH
      ENTRY DQ_DRAW_HV_1(H2,V2)
CH
CH --------------------------------------------------------------------
      IF(FHAX) THEN
        HD(2)=HM+H2
        VD(2)=VM+V2
      ELSE
        HD(2)=HM+V2
        VD(2)=VM+H2
      END IF
      CALL DGDRAW(2,HD,VD)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQ_DRAW_HV_2
CH
      ENTRY DQ_DRAW_HV_2(H1,V1,H2,V2)
CH
CH --------------------------------------------------------------------
      IF(FHAX) THEN
        H(1)=HM+H1
        V(1)=VM+V1
        H(2)=HM+H2
        V(2)=VM+V2
      ELSE
        H(1)=HM+V1
        V(1)=VM+H1
        H(2)=HM+V2
        V(2)=VM+H2
      END IF
      CALL DGDRAW(2,H,V)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQ_DRAW_HV_IN
CH
      ENTRY DQ_DRAW_HV_IN(H0,V0,FH)
CH
CH --------------------------------------------------------------------
      FHAX=FH
      HM=H0
      VM=V0
      END
*DK DQ_AREA_HV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQ_AREA_HV
CH
      SUBROUTINE DQ_AREA_HV(NN,HH,VV)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION HH(*),VV(*)
      DIMENSION H(20),V(20)
      LOGICAL FHAX,FH
      IF(FHAX) THEN
        DO N=1,NN
          H(N)=HM+HH(N)
          V(N)=VM+VV(N)
        END DO
      ELSE
        DO N=1,NN
          H(N)=HM+VV(N)
          V(N)=VM+HH(N)
        END DO
      END IF
      CALL DGAREA(NN,H,V)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQ_AREA_HV_1
CH
      ENTRY DQ_AREA_HV_1(H1,V1)
CH
CH --------------------------------------------------------------------
      IF(FHAX) THEN
        HC=HM+H1
        VC=VM+V1
      ELSE
        HC=HM+V1
        VC=VM+H1
      END IF
      H(1)=HC-D
      V(1)=VC-D
      H(3)=HC+D
      V(3)=VC+D
      GO TO 2
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQ_AREA_HV_2
CH
      ENTRY DQ_AREA_HV_2(H1,V1,H2,V2)
CH
CH --------------------------------------------------------------------
      IF(FHAX) THEN
        H(1)=HM+H1
        H(3)=HM+H2
        V(1)=VM+V1
        V(3)=VM+V2
      ELSE
        H(1)=HM+V1
        H(3)=HM+V2
        V(1)=VM+H1
        V(3)=VM+H2
      END IF
    2 H(2)=H(3)
      V(2)=V(1)
      H(4)=H(1)
      V(4)=V(3)
      CALL DGAREA(4,H,V)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQ_AREA_HV_IN
CH
      ENTRY DQ_AREA_HV_IN(H0,V0,FH,DD)
CH
CH --------------------------------------------------------------------
      FHAX=FH
      HM=H0
      VM=V0
      D=DD
      END
*DK DBT_FRAMED_AREAS
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DBT_FRAMED_AREAS
CH
      SUBROUTINE DBT_FRAMED_AREAS(IST,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION ICOL(12),LFR(2),LBG(2),V1(2),V2(2)
      DATA ICOL/3,4,5,6,7,9,10,11,12,13,14,15/,LFR/1,8/
      IF(IST.EQ.0) THEN
        PR(4,1)=-1.   ! SIZE
        PR(2,7)=1.
        PR(2,19)=0.4
        PR(2,5)=8.    ! COLOR ABOVE FRAME=1
        PR(2,6)=1.    ! COLOR BELOW FRAME=8
      END IF
      LBG(1)=PR(2,5)
      LBG(2)=PR(2,6)
      DLINDD=PR(2,7)
      H1=HMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      DH=(H2-H1)/12.
      V1(1)=VMINDG(IAREDO)
      V2(2)=VHGHDG(IAREDO)
      V2(1)=0.5*(V1(1)+V2(2))
      V1(2)=V2(1)
      DV=(V2(1)-V1(1))/2.
      IF(PR(4,1).LE.0.) THEN
        HD=0.5*DH*PR(2,19)
        VD=0.5*DV*PR(2,19)
      ELSE
        HD=PR(2,1)
        VD=PR(2,1)
      END IF
      DO M=1,2
        CALL DGLEVL(LBG(M))
        CALL DQFAR(H1,V1(M),H2,V2(M))
        H=H1+0.5*DH
        DO L=1,12
          V=V1(M)+0.5*DV
          CALL DGLEVL(ICOL(L))
          CALL DQFAR(H-HD,V-VD,H+HD,V+VD)
          IF(M.EQ.1) THEN
            CALL DGLEVL(LFR(M))
            CALL DQDAR(H-HD,V-VD,H+HD,V+VD)
            CALL DGLEVL(ICOL(L))
          END IF
          V=V+DV
          CALL DQFAR(H-HD,V-VD,H+HD,V+VD)
          IF(M.EQ.2) THEN
            CALL DGLEVL(LFR(M))
            CALL DQDAR(H-HD,V-VD,H+HD,V+VD)
          END IF
          H=H+DH
        END DO
      END DO
      END
C
      SUBROUTINE AAAA(PR)
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA Q1/0.0/,Q2/0.000000005/,DS/20./,WLONG/0.04/,LBG/0/,L8/8/
      DIMENSION PR(4,*)
      DIMENSION NSQ(12,2),XAX(0:3),YAX(0:3),ZAX(0:3),HAX(0:3),VAX(0:3),
     &  HSQ(8),VSQ(8),
     &  HD(2),VD(2),SX(8),SY(8),SZ(8)

C               0    X    Y    Z
      DATA XAX/ 0.,  1.,  0.,  0./
      DATA YAX/ 0.,  0.,  1.,  0./
      DATA ZAX/ 0.,  0.,  0.,  1./
      DATA NSQ/ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4,
     &          2, 3, 4, 1, 6, 7, 8, 5, 5, 6, 7, 8/
      DATA SX/ 1., 1.,-1.,-1.,  1., 1.,-1.,-1./
      DATA SY/-1., 1., 1.,-1., -1., 1., 1.,-1./
      DATA SZ/-1.,-1.,-1.,-1.,  1., 1., 1., 1./
      LOGICAL FBUT(4),DGPNTR
      DATA IDEB/0/

      X0=-PR(2,10) !H1 1000
      Y0=-PR(2,11) !H2 10
      Z0=-PR(2,12) !DH 40
      XE=-PR(2,13) !V1 30
      R0= PR(2,14)*.01 !V2 ??
      AL= PR(2,16)   !P1
      H1=HMINDG(IAREDO)
      V1=VMINDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      HM=0.5*(H2+H1)
      VM=0.5*(V2+V1)
      VHS=VM+DS
      VLS=VM-DS
      DV=V2-V1
      RS=R0*DV
      XE0=XE-X0
      DLINDD=2.
      CALL DQCL(IAREDO)
      CALL DGSCUR(HM,VM)
    1 CALL DGLEVL(LBG)
      CALL DQFAR(H1,V1,H2,V2)
      CALL DGLEVL(L8)
      SA=SIND(AL)
      CA=COSD(AL)
      DO K=0,3
        XS= CA*XAX(K)+SA*YAX(K)
        YS=-SA*XAX(K)+CA*YAX(K)
        ZS= ZAX(K)
        Q=XE0/(XS-X0)
        HAX(K)=Z0+Q*(ZS-Z0)
        VAX(K)=Y0+Q*(YS-Y0)
      END DO
      DO K=3,0,-1
        HAX(K)=RS*(HAX(K)-HAX(0))
        VAX(K)=RS*(VAX(K)-VAX(0))
      END DO
      DO K=1,8
        HSQ(K)=SX(K)*HAX(1)+SY(K)*HAX(2)+SZ(K)*HAX(3)
        VSQ(K)=SX(K)*VAX(1)+SY(K)*VAX(2)+SZ(K)*VAX(3)
      END DO
      DO N=1,12
        I1=NSQ(N,1)
        I2=NSQ(N,2)
        HD(1)=HSQ(I1)+HM
        HD(2)=HSQ(I2)+HM
        VD(1)=VSQ(I1)+VM
        VD(2)=VSQ(I2)+VM
        CALL DGDRAW(2,HD,VD)
      END DO
      CALL DGCHKX
   20 IF(IDEB.EQ.1) CALL DWAIT1(WLONG)
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
        IF(FBUT(3)) THEN
          PR(2,16)=AL
          RETURN
        END IF
        HC=JHC
        VC=JVC
        IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
          HCOLD=HC
          VCOLD=VC
        END IF
        IF(VC.GT.VHS) THEN
          D=(VC-VHS)*(H2-HC)
          IF(FBUT(2)) THEN
            DA=-Q1*D-Q2*D*D
          ELSE
            DA= Q1*D+Q2*D*D
          END IF
        ELSE IF(VC.LT.VLS) THEN
          D=(VLS-VC)*(H2-HC)
          IF(FBUT(2)) THEN
            DA= Q1*D+Q2*D*D
          ELSE
            DA=-Q1*D-Q2*D*D
          END IF
        ELSE
          GO TO 20
        END IF
        AL=AL+DA
        IF(AL.GE.360.) THEN
          AL=AL-360.
        ELSE IF(AL.LT.0.) THEN
          AL=AL+360.
        END IF
        WRITE(TXTADW,1001) AL
 1001   FORMAT('AL=',F6.2,'m')
        CALL DWR_OVER_PRINT(10)
        GO TO 1
      END IF
      END
*DK DBT_STEREO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++  DBT_STEREO
CH
      SUBROUTINE DBT_STEREO(NSTRT,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4)
      DIMENSION HRB2(4)
      DATA HRB/-5.,5.,5.,-5./,VRB/-10.,-10.,10.,10./
      DATA HRB2/-10.,10.,10.,-10./
      PARAMETER (NLIN=9)
      DIMENSION XYZ(2,3,NLIN),H(2),V(2)
      DATA XYZ/ -10., 10.,    -3., -3.,   -10., 10.,  ! in horizonta plane
     &          -10., 10.,    -9.,  9.,   -10., 10.,  ! 45D line
     &          -2.5,  5.,  -2.25, 4.5,   -2.5,  5.,  ! 45D line
     &           -3.,  3.,    -9.,  9.,    -5., 10.,  ! vertical line
     &           -3.,  5.,     4.,  7.,     2., -3.,  ! SMALL LINE
     &           -3.,  2.,     3.,  3.,    -2.,  3.,  ! SMALL LINE
     &           -3.,  4.,     2.,  2.,    -4.,  1.,  ! SMALL LINE
     &            0.,  0.,     0.,  0.,     0.,  0.,
     &            0.,  0.,     0.,  0.,     0.,  0./
      DIMENSION LL(NLIN)
      DATA LL/12,10,9,14,13,11,8,12,15/


C              123456789 123456789 123456789 123456789 123456789
      CALL DQCL(IAREDO)
      HLOW=HLOWDG(0)
      HHGH=HHGHDG(0)
      VLOW=VLOWDG(0)
      VHGH=VHGHDG(0)
      DLIN=DLINDD
      IARE=IAREDO
C
C
      IF(NSTRT.EQ.0) THEN
        PR(2,7)=2.     ! width
        PR(2,16)=3.5   ! eye to nose
        PR(2,17)=30.   ! nose to screen
        PR(2,8)=1.
        PR(2,9)=1.
        PR(2,5)=8.
        PR(2,6)=1.
        PR(2,18)=1.
      END IF
      LA=PR(2,6)
      EN=PR(2,16) ! P1
      SN=PR(2,17) ! P2
      K1=MIN(NLIN,IFIX(PR(2,8))) ! FR
      K2=MIN(NLIN,IFIX(PR(2,9))) ! TO
      MO=PR(2,18)                ! MO =2 SIDE VIEW
C
      CALL DQFFWI(LA)
      IF(MO.NE.2) THEN
        HLOWDG(0)=HLOWDG(IARE)
        HHGHDG(0)=0.5*(HHGHDG(IARE)+HLOWDG(IARE))
        VLOWDG(0)=VLOWDG(IARE)
        VHGHDG(0)=VHGHDG(IARE)
        IAREDO=0
C
        DO E=-EN,EN,2.*EN
          CALL DQRU(HRB,VRB)
          DLINDD=PR(2,7) ! SH
          DO K=K1,K2
            CALL DGLEVL(LL(K))
            DO L=1,2
              H(L)=XYZ(L,1,K)+(E-XYZ(L,1,K))*XYZ(L,3,K)/(SN+XYZ(L,3,K))
              V(L)=XYZ(L,2,K)*SN/(SN+XYZ(L,3,K))
            END DO
            CALL DQLIE(H,V)
          END DO
          CALL DQFR(IAREDO)
          HLOWDG(0)=HHGHDG(0)
          HHGHDG(0)=HHGHDG(IARE)
        END DO
      ELSE
        CALL DQRU(HRB2,VRB)
        DLINDD=PR(2,7) ! SH
        DO K=K1,K2
          CALL DGLEVL(LL(K))
          DO L=1,2
            H(L)=XYZ(L,3,K)
            V(L)=XYZ(L,2,K)
          END DO
          CALL DQLIE(H,V)
        END DO
        CALL DQFR(IAREDO)
      END IF
      WRITE(TXTADW,1001) (N,LL(N),N=1,NLIN)
 1001 FORMAT(9(I1,':',I2))
      CALL DWRC
      HLOWDG(0)=HLOW
      HHGHDG(0)=HHGH
      VLOWDG(0)=VLOW
      VHGHDG(0)=VHGH
      DLINDD=DLIN
      IAREDO=IARE
      END
*DK DBT_STEREO_INT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++  DBT_STEREO_INT
CH
      SUBROUTINE DBT_STEREO_INT(NSTRT,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4),H(2),V(2)
      DATA HRB/-5.,5.,5.,-5./,VRB/-10.,-10.,10.,10./
      DIMENSION HL(33,2),HR(33,2),VV(33,2),KL(2,2)
      CALL DQCL(IAREDO)
      HMIN=HMINDG(0)
      HLOW=HLOWDG(0)
      HHGH=HHGHDG(0)
      VMIN=VMINDG(0)
      VLOW=VLOWDG(0)
      VHGH=VHGHDG(0)
      DLIN=DLINDD
      IARE=IAREDO
C
C
      IF(NSTRT.EQ.0) THEN
        PR(2,7)=3.     ! HIT SIZE
        PR(4,7)=1.
        PR(2,16)=3.5   ! eye to nose
        PR(2,17)=30.   ! nose to screen
        PR(2,5)=10.    ! hit color
        PR(2,2)=12.    ! track color
        PR(4,2)=1.
        PR(2,1)=2.     ! TRACK SIZE
        PR(2,6)=1.     ! CA
        PR(2,10)=0.    ! x
        PR(2,11)=0.    ! y
        PR(2,12)=0.    ! z
        PR(2,13)=55.   ! FI
        PR(2,14)=30.   ! THETA
        PR(2,15)=1.07  ! HIT DISTANCE = RR
        PR(2,18)=0.    ! 0 = corresponding points, 1 = no c.p.
      END IF
C
      CALL DWRT('SZ,SH,CO,CR,CA,P1=en,P2=es,PL=1,2')
      CALL DWRT('H1=x,H2=y,DH=z,V1=fi,V2=te,DV=r')
      LH1=PR(2,5)
      LL1=PR(2,2)
      IF(PR(4,7).GT.0.) THEN
        LL2=LL1
      ELSE
        LL2=LH1
      END IF
      IF(PR(4,2).GT.0.) THEN
        LH2=LH1
      ELSE
        LH2=LL1
      END IF
      LA=PR(2,6)
      EN=PR(2,16) ! P1
      SN=PR(2,17) ! P2
      MO=PR(2,18) ! PL =1,2
      MO=1+MOD(MO,2)
C
      CALL DQFFWI(LA)
C
      CALL DQPD0(8,PR(2,7),0.) ! SH
      FI=PR(2,13)
      TE=PR(2,14)
      RR=PR(2,15)
      SF=SIND(FI)
      CF=COSD(FI)
      ST=SIND(TE)
      CT=COSD(TE)
      DZ=RR*ST
      RO=RR*CT
      DX=RO*CF
      DY=RO*SF
      X=PR(2,10)
      Y=PR(2,11)
      Z=PR(2,12)
      DO I=1,2
        KL(I,1)=33
        KL(I,2)=33
        DO K=1,33
          HL(K,I)=X+(-EN-X)*Z/(SN+Z)
          HR(K,I)=X+( EN-X)*Z/(SN+Z)
          VV(K,I)=Y*SN/(SN+Z)
          IF(KL(I,1).EQ.33) THEN
            IF(VV(K,I).LT.VRB(1).OR.VV(K,I).GT.VRB(3)) THEN
              KL(I,1)=K-1
              KL(I,2)=K
              GO TO 2
            END IF
            IF(HL(K,I).LT.HRB(1).OR.HR(K,I).LT.HRB(1)) KL(I,1)=K-1
            IF(HL(K,I).GT.HRB(3).OR.HR(K,I).GT.HRB(3)) KL(I,1)=K-1
          END IF
          IF(     HL(K,I).LT.HRB(1).AND.HR(K,I).LT.HRB(1)) THEN
            KL(I,2)=K
            GO TO 2
          ELSE IF(HL(K,I).GT.HRB(3).AND.HR(K,I).GT.HRB(1)) THEN
            KL(I,2)=K
            GO TO 2
          END IF
          X=X+DX
          Y=Y+DY
          Z=Z+DZ
        END DO
    2   S=-1.
        X=PR(2,10)
        Y=PR(2,11)
        Z=PR(2,12)
        DX=-DX
        DY=-DY
        DZ=-DZ
      END DO
      IAREDO=0
      HLOWDG(0)=HMINDG(IARE)
      HMINDG(0)=HMINDG(IARE)
      HHGHDG(0)=0.5*(HHGHDG(IARE)+HLOWDG(IARE))
      VLOWDG(0)=VMINDG(IARE)
      VMINDG(0)=VMINDG(IARE)
      VHGHDG(0)=VHGHDG(IARE)
      CALL DQRU(HRB,VRB)
      DLINDD=PR(2,1) ! SZ
      IF(PR(4,2).GT.0.) THEN
        CALL DGLEVL(LL1)
        H(1)=HL(KL(1,MO),1)
        V(1)=VV(KL(1,MO),1)
        H(2)=HL(KL(2,MO),2)
        V(2)=VV(KL(2,MO),2)
        CALL DQLIE(H,V)
      END IF
      IF(PR(4,7).GT.0.) THEN
        CALL DGLEVL(LH1)
        DO I=1,2
          DO K=1,KL(I,MO)
            CALL DQPD(HL(K,I),VV(K,I))
          END DO
        END DO
      END IF
      CALL DQFR(IAREDO)
C
      HLOWDG(0)=HHGHDG(0)
      HMINDG(0)=HHGHDG(0)
      HHGHDG(0)=HHGHDG(IARE)
      CALL DQRU(HRB,VRB)
      DLINDD=PR(2,1) ! SZ
      IF(PR(4,2).GT.0.) THEN
        CALL DGLEVL(LL2)
        H(1)=HR(KL(1,MO),1)
        V(1)=VV(KL(1,MO),1)
        H(2)=HR(KL(2,MO),2)
        V(2)=VV(KL(2,MO),2)
        CALL DQLIE(H,V)
      END IF
      IF(PR(4,7).GT.0.) THEN
        CALL DGLEVL(LH2)
        DO I=1,2
          DO K=1,KL(I,MO)
            CALL DQPD(HR(K,I),VV(K,I))
          END DO
        END DO
      END IF
      CALL DQFR(IAREDO)
C
      HMINDG(0)=HMIN
      HLOWDG(0)=HLOW
      HHGHDG(0)=HHGH
      VMINDG(0)=VMIN
      VLOWDG(0)=VLOW
      VHGHDG(0)=VHGH
      DLINDD=DLIN
      IAREDO=IARE
      END
*DK DBT_STEREO_RANDOM_POINTS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++  DBT_STEREO_RANDOM_POINTS
CH
      SUBROUTINE DBT_STEREO_RANDOM_POINTS(NSTRT,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION HRB(4),VRB(4),LC(2)
      DATA IR,IRAN/2*84523711/,DX/5./,DY/10./,MA/7/
      CALL DQCL(IAREDO)
      HMIN=HMINDG(0)
      HLOW=HLOWDG(0)
      HHGH=HHGHDG(0)
      VMIN=VMINDG(0)
      VLOW=VLOWDG(0)
      VHGH=VHGHDG(0)
      IARE=IAREDO
C
C
      IF(NSTRT.EQ.0) THEN
        PR(2,1)=2.     ! HIT SIZE
        PR(2,16)=3.5   ! eye to nose
        PR(2,17)=30.   ! nose to screen
        PR(2,5)=8.     ! hit color LEFT
        PR(2,2)=8.     ! hit color RIGHT
        PR(2,6)=1.     ! CA
        PR(2,10)=6.5   ! BOX SIZE
        PR(2,11)=1.    ! SCALE
        PR(2,12)=3.    ! DZ
      END IF
C
      CALL DWRT('SZ,CO,NU,CA,P1=en,P2=es,H1=scale,H2=zoom,DH=dz')
      LC(1)=PR(2,5)
      LC(2)=PR(2,2)
      LA=PR(2,6)
      EN=-PR(2,16) ! P1
      SN=PR(2,17) ! P2
      ZF=PR(2,11)
      DZ=PR(2,12)
      S=PR(2,10)
      S2=S
      CALL DQRER(0,-S,-S2,S,S2,HRB,VRB)
      NUM=PR(2,3)
C
      CALL DQFFWI(LA)
C
      CALL DQPD0(8,PR(2,1),0.) ! SZ
      IAREDO=0
      HLOWDG(0)=HMINDG(IARE)
      HMINDG(0)=HMINDG(IARE)
      HHGHDG(0)=0.5*(HHGHDG(IARE)+HLOWDG(IARE))
      VLOWDG(0)=VMINDG(IARE)
      VMINDG(0)=VMINDG(IARE)
      VHGHDG(0)=VHGHDG(IARE)
      IF(PR(4,4).GT.0.) IR=IRAN
      DO I=1,2
        CALL DQRU(HRB,VRB)
        CALL DGLEVL(LC(I))
        IRAN=IR
        DO K=1,NUM
          IF(PR(4,5).LT.0.) CALL DGLEVL(8+MOD(K,MA))
          X=(RAN(IRAN)-0.5)*2.*DX*ZF
          Y=(RAN(IRAN)-0.5)*2.*DY*ZF
          Z=(RAN(IRAN)-0.5)*2.*DZ*ZF
          H=X+Z*(EN-X)/(SN+Z)
          V=Y*SN/(SN+Z)
          CALL DQPD(H,V)
        END DO
        CALL DQFR(IAREDO)
        EN=-EN
        HLOWDG(0)=HHGHDG(0)
        HMINDG(0)=HHGHDG(0)
        HHGHDG(0)=HHGHDG(IARE)
      END DO
C
      HMINDG(0)=HMIN
      HLOWDG(0)=HLOW
      HHGHDG(0)=HHGH
      VMINDG(0)=VMIN
      VLOWDG(0)=VLOW
      VHGHDG(0)=VHGH
      IAREDO=IARE
      END
*DK DBT_2_COL_LINES
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++ DBT_2_COL_LINES
CH
      SUBROUTINE DBT_2_COL_LINES(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION ICOL(-1:1),H(2),V(2)
      CHARACTER *1 TC(8:15)
      DATA TC/'W','G','Y','O','R','M','C','B'/
      DATA DV1/4./,DV2/16./,Q41/65./
      CALL DQCL(IAREDO)
      CALL DQFFWI(IFIX(PR(2,6)))
      HMIN=HMINDG(IAREDO)
      HHGH=HHGHDG(IAREDO)
      VMIN=VMINDG(IAREDO)
      VHGH=VHGHDG(IAREDO)
      HMID=0.5*(HMIN+HHGH)
      DH=(HHGH-HMIN)/Q41
      DLINDD=PR(2,1)
      V(1)=VMIN
      V(2)=VHGH
      HH=0.
      VT1=VMIN+DV1
      VT2=VMIN+DV2
      NUM=PR(2,3)
      IF(NUM.LT.2.OR.NUM.GT.5) NUM=2
      DO K=8,15
        ICOL(-1)=K
        DO L=8,15
          ICOL(1)=L
          HH=HH+DH
          H(1)=HH
          H(2)=HH
          I=1
          DO N=1,NUM
            CALL DGLEVL(ICOL(I))
            CALL DGDRAW(2,H,V)
            H(1)=H(1)+DLINDD
            H(2)=H(1)
            I=-I
          END DO
          CALL DGLEVL(IFIX(PR(2,5)))
          CALL DGTEXT(H(1),VT1,TC(K),1)
          CALL DGTEXT(H(1),VT2,TC(L),1)
        END DO
      END DO
      CALL DGEXEC
      END
*DK DBT_POINT_COLORS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_POINT_COLORS
CH
      SUBROUTINE DBT_POINT_COLORS(MO,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      DIMENSION ICOL(8),LF(7),LA(7)
      DATA ICOL/15,9,13,14,10,12,8,11/
      DATA QL/0.1/
      DLIN=DLINDD
      IF(MO.EQ.0) THEN
        PR(2,1)=3.
        PR(2,7)=9.
        PR(2,2)=1.
        PR(4,2)=-1.
        PR(2,16)=1.
        PR(2,17)=789543.
        PR(2,18)=1.
      END IF
      SZ=PR(2,1)
      SH=PR(2,7)
      LAR=PR(2,16)
      LAF=PR(2,17)
      LPF=PR(2,2)
      POL=PR(2,18)
      HL=HMINDG(IAREDO)
      HH=HHGHDG(IAREDO)
      VL=VMINDG(IAREDO)
      VH=VHGHDG(IAREDO)
      NA=0
      NF=0
      DO K=1,7
        LCC=LAR/10**(7-K)
        LCC=MOD(LCC,10)
        IF(LCC.NE.0) THEN
          NA=NA+1
          IF(LCC.EQ.9) THEN
            LA(NA)=0
          ELSE
            LA(NA)=LCC
          END IF
        END IF
        LCC=LAF/10**(7-K)
        LCC=MOD(LCC,10)
        IF(LCC.NE.0) THEN
          NF=NF+1
          IF(LCC.EQ.9) THEN
            LF(NF)=0
          ELSE
            LF(NF)=LCC
          END IF
        END IF
      END DO
      DP=(VH-VL)/16.
      DA=(HH-HL)/(NA+NF)
      DA2=DA/2.
      HA1=HL
      DO N=1,NA
        HA2=HA1+DA
        CALL DGLEVL(LA(N))
        CALL DQFAR(HA1,VL,HA2,VH)
        IF(POL.EQ.1.) THEN
          H1=HA1+QL*DA
          H2=HA2-QL*DA
        ELSE
          H1=HA1+DA2-SH/2.
          H2=H1+SH
        END IF
        V1=VL+DP/2
        DO K=1,8
          V2=V1+SH
          CALL DGLEVL(ICOL(K))
          CALL DQFAR(H1,V1,H2,V2)
          V1=V1+DP
        END DO
        H1=HA1+DA2-SH/2.
        H2=H1+SH
        DO K=1,8
          V2=V1+SZ
          CALL DGLEVL(ICOL(K))
          CALL DQFAR(H1,V1,H2,V2)
          V1=V1+DP
        END DO
        HA1=HA2
      END DO
      DO N=1,NF
        HA2=HA1+DA
        CALL DGLEVL(LF(N))
        CALL DQFAR(HA1,VL,HA2,VH)
        IF(POL.EQ.1.) THEN
          H1=HA1+QL*DA
          H2=HA2-QL*DA
        ELSE
          H1=HA1+DA2-SH/2.
          H2=H1+SH
        END IF
        V1=VL+DP/2
        DO K=1,8
          V2=V1+SH
          CALL DGLEVL(LPF)
          CALL DQFAR(H1-1.,V1-1.,H2+1.,V2+1.)
          CALL DGLEVL(ICOL(K))
          CALL DQFAR(H1,V1,H2,V2)
          V1=V1+DP
        END DO
        H1=HA1+DA2-SZ/2.
        H2=H1+SZ
        DO K=1,8
          V2=V1+SZ
          CALL DGLEVL(LPF)
          CALL DQFAR(H1-1.,V1-1.,H2+1.,V2+1.)
          CALL DGLEVL(ICOL(K))
          CALL DQFAR(H1,V1,H2,V2)
          V1=V1+DP
        END DO
        HA1=HA2
      END DO
      DLINDD=DLIN
      CALL DQFR(IAREDO)
      END

*DK DBT_BLUE
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_BLUE
CH
      SUBROUTINE DBT_BLUE(TANSW,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
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
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      CHARACTER *(*) TANSW

      DIMENSION COL(3,8),LCOL(8)
      DATA LCOL/1,8,9,10,12,13,14,15/
      DATA COL/0.,0.,0.,
     &         1.,1.,1.,
     &         0.,1.,0.,
     &         1.,1.,0.,
     &         1.,0.,0.,
     &         1.,0.,1.,
     &         0.,1.,1.,
     &         0.,0.,1./

      DIMENSION LW(6),LL(4),LP(4),HRB(4),VRB(4)
      DATA LW/2,13,15,12,4,7/,LL/8,9,10,14/,LP/14,10,9,8/
      DATA HRB/0.,330.,330.,0./
      DATA VRB/0.,0.,330.,330./

      DATA H1/20./,H2/ 90./,HP/200./,DH/30./,D2/2./,DL1/1./
      DATA V1/30./,V2/300./,VP/ 30./

      DATA HT/160./,VT/60./,DHT0/60./,DVT0/240./,QR/7./,IR0/23155/
      DATA PNU/20./

      IF(TANSW.EQ.'B1') THEN
        PR(2,1)=3.    ! point size
        PR(2,7)=2.    ! line width
        PR(2,2)=1.    ! frame 2
        PR(4,2)=-1.   ! frame 4
        CALL DWRT('Redraw : BP (with color setup), BR (without)')
      END IF

      PSZ=PR(2,1)    ! point size
      DLW=PR(2,7)    ! line width
      LCR=PR(2,2)    ! frame 2
      CR4=PR(4,2)    ! frame 4

      DHT=DHT0/PNU
      DVT=DVT0/PNU

      DO IAREDO=1,6
        CALL DQFWAF(LW(IAREDO))
        CALL DQRU(HRB,VRB)

        IF(CR4.GT.0.) THEN
          CALL DQPD0(8,PSZ+D2,0.)
          CALL DGLEVL(LCR)
          D=0.
          IR=IR0
          DLINDD=DLW+D2
          CALL DQPD0(8,PSZ+D2,0.)
          DO L=1,4
            CALL DQL2E(H1+D,V1,H2+D,V2)
            CALL DQPD(HP+D,VP)

            H=HT+D
            V=VT
            DO K=1,20
              H=H+DHT
              V=V+DVT
              DR=QR*(RAN(IR)-0.5)
              CALL DQPD(H+DR,V)
            END DO

            D=D+DH
          END DO
        END IF

        CALL DQPD0(8,PSZ,0.)
        D=0.
        IR=IR0
        DO L=1,4
C         ................ Attention DLINDD is changed for a symbol # area
          DLINDD=DLW
          CALL DGLEVL(LL(L))
          CALL DQL2E(H1+D,V1,H2+D,V2)
          DLINDD=DL1
          CALL DGLEVL(LP(L))
          CALL DQPD(HP+D,VP)

          H=HT+D
          V=VT
          CALL DGLEVL(LL(L))
          DO K=1,20
            DR=QR*(RAN(IR)-0.5)
            H=H+DHT
            V=V+DVT
            CALL DQPD(H+DR,V)
          END DO
          D=D+DH

        END DO

      END DO

      IF(TANSW.NE.'BR') THEN
        DO I=1,8
          L=LCOL(I)
          RDCODD(L)=COL(1,I)
          GRCODD(L)=COL(2,I)
          BLCODD(L)=COL(3,I)
        END DO
        CALL DW_SET_CO
      END IF

      END

*DK DBT_1_IN_MANY
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++ DBT_1_IN_MANY
CH
      SUBROUTINE DBT_1_IN_MANY(TANSW,PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION PR(4,*)
      CHARACTER *(*) TANSW

      DIMENSION HRB(4),VRB(4)

      DATA HRB/0.,330.,330.,0./
      DATA VRB/0.,0.,330.,330./

      DATA DL1/1./,D2/2./,DH/33./

      DATA HT/20./,VT/20./,DHT0/60./,DVT0/280./,QR/7./,IR0/23155/
      DATA IRN/231767/
      DIMENSION LT(7),KT(7)
      DATA LT/9,10,11,12,13,14,15/,ISA/8/
      DATA KT/3, 9, 4, 7, 2, 5, 6/

      IF(TANSW.EQ.'I1') THEN
        PR(2,1)=3.    ! point size
        PR(2,3)=20.   ! number of points
        PR(2,2)=1.    ! frame 2
        PR(4,2)=-1.   ! frame 4
        PR(2,5)=8.    ! color of extra symbol
        PR(2,6)=2.    ! color of area
        PR(2,12)=1.   ! reduce size of extra symbol
        PR(2,16)=6.   ! symbol 2
        PR(4,16)=1.   ! symbol 4
        CALL DWRT('Redraw : IP ')
      END IF

      PSZ=PR(2,1)    ! point size
      PNU=PR(2,3)    ! number of points
      LCR=PR(2,2)    ! frame 2
      CR4=PR(4,2)    ! frame 4
      LCO=PR(2,5)    ! color of extra symbol
      LCA=PR(2,6)    ! color of area
      PDH=PR(2,12)   ! reduce size of extra symbol
      IS2=PR(2,16)   ! symbol 2

      IS2=MAX(0,MIN(IS2,9))
      IF(PNU.LE.0) RETURN
      DHT=DHT0/PNU
      DVT=DVT0/PNU
      NUM=PNU

      DO K=1,7
        KT(K)=2.+(PNU-3.)*RAN(IRN)
      END DO
      WRITE(TXTADW,1000) (KT(K),K,K=1,7)
 1000 FORMAT(7(1X,I3,':',I2))
      CALL DWRC

      CALL DQFWAF(LCA)
      CALL DQRU(HRB,VRB)

      IF(CR4.GT.0.) THEN
        CALL DQPD0(8,PSZ+D2,0.)
        CALL DGLEVL(LCR)
        D=0.
        IR=IR0
        DO L=1,7
          H=HT+D
          V=VT
          DO K=1,NUM
            H=H+DHT
            V=V+DVT
            DR=QR*(RAN(IR)-0.5)
            CALL DQPD(H+DR,V)
          END DO
          D=D+DH
        END DO
      END IF

      D=0.
      IR=IR0
      DLINDD=DL1

      DO L=1,7

        H=HT+D
        V=VT
        DO K=1,NUM
          DR=QR*(RAN(IR)-0.5)
          H=H+DHT
          V=V+DVT
          IF(K.EQ.KT(L)) THEN
            CALL DGLEVL(LCO)
            CALL DQPD0(IS2,PSZ-PDH,0.)
          ELSE
            CALL DGLEVL(LT(L))
            CALL DQPD0(ISA,PSZ,0.)
          END IF
          CALL DQPD(H+DR,V)
        END DO

        D=D+DH
      END DO

      END
