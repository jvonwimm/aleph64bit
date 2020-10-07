*DK DPCEAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPCEAR
CH
      SUBROUTINE DPCEAR(NARE)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter Copy: Execute all pictures on ARea "nar"
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
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
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C------------------------------------------------------------------- DP
      PARAMETER (MAXPDP=120,M11PDP=131)
      COMMON /DPICT/ TPICDP(-10:MAXPDP)
      CHARACTER *2 TPICDP
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL F0,FOUT
      IF(NTVIDT.EQ.0) RETURN
      NAR=NARE
      IPOLD=IPICDO
      IAR=IAREDO
      NOCL=NOCLDT
      FOUT=.TRUE.
      DO   700  M=MPNWDW,0,-1
        IF(NWINDW(M,NAR).EQ.-2.OR.NWINDW(M,NAR).EQ.1) THEN
          CALL DPCGVA(M,F0)
          IF(.NOT.F0) THEN
            NOCLDT=NOCL
            CALL DBR2
            CALL DGCHKX
            FOUT=.FALSE.
          END IF
        END IF
  700 CONTINUE
      IF(FOUT) THEN
        CALL DWRT('The selected window '//TAREDO(IAREDO)//
     &    ' contains no full picture.')
        CALL DWRT('Select other window.')
      END IF
      IAREDO=IAR
      IPICDO=IPOLD
      TPICDO=TPICDP(IPICDO)
      PICNDO=IPICDO
      END
*DK DPCGAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPCGAR
CH
      SUBROUTINE DPCGAR(IARE,F0)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter-Copy: Get parameters of ARea, if stored
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C------------------------------------------------------------------- DP
      PARAMETER (MAXPDP=120,M11PDP=131)
      COMMON /DPICT/ TPICDP(-10:MAXPDP)
      CHARACTER *2 TPICDP
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL F0
      IWUS=0
      IF(ISTODS(4,IARE,IWUS).EQ.0) THEN
         F0=.TRUE.
         RETURN
      END IF
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DPCGVA
CH
      ENTRY DPCGVA(IARE,F0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter-Copy: Get paramters of Visible area
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IWUS=0
      IF(ISTODS(4,IARE,IWUS).LT.1) THEN
         F0=.TRUE.
         RETURN
      END IF
    1 MAR=IARE
      JAR=IARE
      GO TO 2
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DPCGVI
CH
      ENTRY DPCGAR_NEW(NWUS,IARE,F0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter-Copy: Get paramters dependent on user window
      IWUS=NWUS
      IF(ISTODS(4,IARE,IWUS).EQ.0) THEN
         F0=.TRUE.
         RETURN
      END IF
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DPCGVI
CH
      ENTRY DPCGVI(IARE,F0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter-Copy: Get paramters of Virtual area
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IWUS=0
      MAR=IARE
      JAR=IAREDO
      IF(ISTODS(1,MAR,0).EQ.0) THEN
        F0=.TRUE.
        RETURN
      END IF
    2 CALL UCOPY(ISTODS(1,MAR,IWUS),IMAXDO,8)
      TPICDO=TPICDP(IPICDO)
      IAREDO=JAR
      CALL DPACGF(1,PSTODS(1,1,MAR,IWUS))
      F0=.FALSE.
      END
*DK DPCSAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPCSAR
CH
      SUBROUTINE DPCSAR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter-Copy: Store for ARea
C!:store parameters for projection npic and used window.
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DW
      PARAMETER (MPOSDW=30,MPNWDW=12,MNUWDW=14)
      COMMON /DWINDC/NWINDW(0:MPNWDW,0:MPNWDW),
     &  NEXTDW(0:MPNWDW),LASTDW(0:MPNWDW),
     &  POSIDW(MPOSDW)
      COMMON /DWINDO/TWINDW(0:MNUWDW)
      CHARACTER *1 TWINDW
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
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
      IWUS=0
      DO   700  M=0,MPNWDW
        IF(NWINDW(M,IAREDO).LT.0.AND.ISTODS(4,M,0).NE.0)
     &     ISTODS(4,M,0)=-1
  700 CONTINUE
      IARE=IAREDO
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DPCSAV
CH
      ENTRY DPCSAV(NWUS,JARE)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter-Copy: Get paramters of Visible area
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IARE=JARE
      IWUS=NWUS
C     ............. copy last window set to area -4 = virtual window 0.
C     ............. Get back with "G0").
    1 CALL UCOPY(ISTODS(1,IARE,IWUS),ISTODS(1,-4,IWUS),8)
      ISTODS(4,-4,IWUS)=1
      CALL UCOPY(TVRUDS(1,IARE,IWUS),TVRUDS(1,-4,IWUS),4)
      CALL UCOPY(TVRUDS(5,IARE,IWUS),TVRUDS(5,-4,IWUS),2)
      CALL DPACFT(1,PSTODS(1,1,IARE,IWUS),PSTODS(1,1,-4,IWUS))
C     ....................... copy current to window
      CALL UCOPY(IMAXDO,ISTODS(1,IARE,IWUS),8)
      ISTODS(4,IARE,IWUS)=1
      CALL UCOPY(HUMIDT,TVRUDS(1,IARE,IWUS),4)
      CALL UCOPY(AROTDT,TVRUDS(5,IARE,IWUS),2)
      CALL DPACGT(1,PSTODS(1,1,IARE,IWUS))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DPCSAV
CH
      ENTRY DPCPAR(IPIC,IPAR,NPAR)
CH
CH --------------------------------------------------------------------
CH
      IF(ISTODS(5,IPAR,0).EQ.IPIC.AND.
     &  ISTODS(4,IPAR,0).EQ.1) THEN
        NPAR=IPAR
      ELSE
        DO NPAR=MPNWDW,0,-1
          IF(ISTODS(5,NPAR,0).EQ.IPIC.AND.
     &       ISTODS(4,NPAR,0).EQ.1) RETURN
        END DO
        NPAR=-9
      END IF
      END

*DK DPCGV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DPCGV
CH
      SUBROUTINE DPCGV(IARE,NGR,TGR,NP,VAL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) TGR
      IF(NP.EQ.2.OR.NP.EQ.4) THEN
        IP=NP/2
        CALL DPAR_POINTER(NGR,TGR,I)
        VAL=PSTODS(IP,I,IARE,0)
      ELSE
        CALL DPARGV(NGR,TGR,NP,VAL)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DPCSAV
CH
      ENTRY DPCIVEC(IARE,NP,IVAL)
CH
CH --------------------------------------------------------------------
CH
C     ........................................ 4=IARE, 5=IPIC, 6=IZOM
      IVAL=ISTODS(NP,IARE,0)
      END

*DK DPI_NAVIGATE
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DPI_NAVIGATE
CH
      SUBROUTINE DPI_NAVIGATE(MODUL,NHIT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INCLUDE 'DALI_EX.INC'
      DATA PIDEG/57.29577951/
      PARAMETER (MESDA=13)
      DIMENSION NESDA(MESDA)
      DIMENSION TP(6)
      CHARACTER *2 TESDA(MESDA)
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      CHARACTER *5 DT5,TV(MESDA,2)
      CHARACTER *6 DT6
      DATA NESDA/
     &    1 ,  2 ,  3 ,  4 ,  5 ,  6 , 10 , 11 ,
     &   12 , 13 , 14 , 15 , 19 /
      DATA TESDA/
     &  ' X',' Y',' Z','RO','FI','TE','DF','DT',
     &  ' E',' I',' J',' K','CL'/
      PARAMETER (MPNT=9)
      DIMENSION NPNT(MPNT)
      CHARACTER *2 TPNT(MPNT)
      DATA NPNT/
     &    1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 , 18 /
      DATA TPNT/
     &  ' X',' Y',' Z','RO','FI','TE',' R',' B','NT'/
      CHARACTER * 49 TVDCO
C                 123456789 123456789 123456789 123456789 123456789
      DATA TVDCO/'from   VDXY # 12345 row 12   VDZT # 12345 row 12'/
      PARAMETER (MMHT=10)
      DIMENSION NMHT(MMHT)
      CHARACTER *2 TMHT(MMHT)
      DATA NMHT/
     &    1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  9 , 16 , 13 /
      DATA TMHT/
     &  ' X',' Y',' Z','RO','FI','TE',' R','YL','SC','SL'/
      PARAMETER (MVDX=6)
      DIMENSION NVDX(MVDX)
      CHARACTER *2 TVDX(MVDX)
      DATA NVDX/
     &    1 ,  2  , 4 ,  5 , 10 , 12 /
      DATA TVDX/
     &  ' X',' Y','RO','FI','UC','PH'/
      PARAMETER (MVDR=7)
      DIMENSION NVDR(MVDR)
      CHARACTER *2 TVDR(MVDR)
      DATA NVDR/
     &    3 ,  4 ,  9  , 5 ,  6 , 10 , 12 /
      DATA TVDR/
     &  ' Z','R1','R2','FI','TE','WC','PH'/
      DATA IDEB/0/

      LOGICAL FOUT
      IF(MODUL.EQ.ESDADB) THEN
        CALL DV0(ESDADB,NUM1,NUM2,FOUT)
        IF(FOUT) RETURN
        IF(NHIT.LT.NUM1.OR.NHIT.GT.NUM2) RETURN
        DO NV=1,MESDA
          TV(NV,1)=DT5(DVEC(NESDA(NV),NHIT))
        END DO
        IF(IDEB.EQ.0) THEN
          WRITE(TXTADW,1001) (TESDA(K),'=',TV(K,1),K=1,4)
          CALL DWRC
          WRITE(TXTADW,1001) (TESDA(K),'=',TV(K,1),K=5,8)
          CALL DWRC
          WRITE(TXTADW,1001) (TESDA(K),'=',TV(K,1),K=9,13)
          CALL DWRC
 1001     FORMAT(5(1X,3A))
        ELSE
C         ........................... check ESDA from VIDEAU
          CALL=DVVEC0(NUM)
          DO NV=1,4
            TV(NV,2)=DT5(DVVEC(NESDA(NV),NHIT))
          END DO
          DO NV=5,6
            TV(NV,2)=DT5(DVVEC(NESDA(NV),NHIT)*PIDEG)
          END DO
          DO NV=7,8
            TV(NV,2)=DT5(DVVEC(NESDA(NV),NHIT)*PIDEG*0.5)
          END DO
          WRITE(TXTADW,1002) (TESDA(K),'=',TV(K,1),
     &                                 '=',TV(K,2),K=1,8)
 1002     FORMAT(3(1X,5A))
          CALL DWRC
        END IF
      ELSE IF (MODUL.EQ.TPCODB) THEN
        CALL DPITBK(TPCODB,DVTP  ,MPNT,NPNT,TPNT,NHIT,'TPCO')
      ELSE IF (MODUL.EQ.TBCODB) THEN
        CALL DPITBK(TBCODB,DVTP  ,MPNT,NPNT,TPNT,NHIT,'TBCO')
      ELSE IF (MODUL.EQ.MHITDB) THEN
        CALL DPITBK(MHITDB,DVMD  ,MMHT,NMHT,TMHT,NHIT,'MHIT')
      ELSE IF (MODUL.EQ.VDZTDB) THEN
        CALL DPITBK(VDZTDB,DVVDRZ,MVDR,NVDR,TVDR,NHIT,'VDZT')
      ELSE IF (MODUL.EQ.VDCODB) THEN
        CALL=DVVDC1(NHIT,IWFXY,IHTXY,IWFZT,IHTZT)
        IF(IHTXY.GT.0) THEN
          CALL DTINT(IWFXY,15,19,TVDCO)
          CALL DTINT(IHTXY,25,26,TVDCO)
        ELSE
          TVDCO(15:19)=' '
          TVDCO(25:26)=' '
        END IF
        IF(IHTZT.GT.0) THEN
          CALL DTINT(IWFZT,37,41,TVDCO)
          CALL DTINT(IHTZT,47,48,TVDCO)
        ELSE
          TVDCO(37:41)=' '
          TVDCO(47:48)=' '
        END IF
        CALL DWRT(TVDCO)
      ELSE IF (MODUL.EQ.VDXYDB) THEN
        CALL DPITBK(VDXYDB,DVVDXY,MVDX,NVDX,TVDX,NHIT,'VDXY')
      ELSE IF (MODUL.EQ.FRFTDB) THEN
        CALL DVTRSX(NHIT,TP(1),TP(2),TP(3),TP(4),TP(5),TP(6))
        WRITE(TXTADW,1008)
     &    'IR=',DT6(TP(1)),
     &    'TL=',DT4(TP(2)),
     &    'P0=',DT3(TP(3)),
     &    'D0=',DT4(TP(4)),
     &    'Z0=',DT4(TP(5)),
     &    'AL=',DT5(TP(6))
 1008   FORMAT(2A,5(1X,2A))
        CALL DWRC
      ELSE
        CALL DWRT('No further output for this bank.')
      END IF
      END
*DK DPITBK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPITBK
CH
      SUBROUTINE DPITBK(MB,DFU,M,N,T,K,TBNK)
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
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *2 T(*)
      CHARACTER *(*) TBNK
      DIMENSION N(*)
      CHARACTER *5 DT5,TP(20)
      LOGICAL FOUT
      EXTERNAL DFU
      CALL DV0(MB,NUM1,NUM2,FOUT)
      IF(FOUT.OR.K.LT.NUM1.OR.K.GT.NUM2) THEN
        CALL DWRT('Hit not found or bank not available')
      ELSE
        IF(MB.NE.VDZTDB) THEN
          DO L=1,M
            TP(L)=DT5(DFU(N(L),K))
          END DO
        ELSE
          DO L=1,M
            TP(L)=DT5(DFU(N(L),K,1))
          END DO
        END IF
        DO I=1,M,5
          I4=MIN(M,I+4)
          WRITE(TXTADW,1000) (T(L),TP(L),L=I,I4)
 1000     FORMAT(5(1X,A,'=',A))
          CALL DWRC
        END DO
        TXTADW=TBNK//': # '//DT5(FLOAT(K))
        CALL DWRC
      END IF
      END

*DK DPI_ENABLE_HITS_TRACKS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++ DPI_ENABLE_HITS_TRACKS
CH
      SUBROUTINE DPI_ENABLE_HITS_TRACKS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      END

*DK DPI_ENABLE_ALL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++ DPI_ENABLE_ALL
CH
      SUBROUTINE DPI_ENABLE_ALL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      END

*DK DPA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPA
CH
      SUBROUTINE DPA
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
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C------------------------------------------------------------------- DH
      COMMON /DHELPT/ TAN1DH,TPRGDH,TLT3DH
      CHARACTER *1 TPRGDH,TLT3DH
      CHARACTER *3 TAN1DH
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TP
      CHARACTER *8 TCOM
      CHARACTER *28 TMAC
      CHARACTER *49 T1
C
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?: Type 3 letter pagename ,    GB=<CR>'/
      CALL DTYPT('TYPE',TPICDO, 0    ,  0   ,  0   ,  ' ' ,T1)
      CALL DQHL_MENU('PA0')
      CALL DGETLN(TP,LN,3)
      IF(     LN.LE.1) THEN
        TPICDO='GB'
        RETURN
      END IF
      IF(LN.EQ.2) TP(3:3)='0'
      CALL DQH_PAGE(TP,TCOM)
      IF(TCOM.EQ.' ') THEN
        TPICDO=TP(1:2)
        IF(TP(1:2).EQ.'GT') THEN
          IF(TP(3:3).EQ.'0') CALL DWRT(
     &      'Second part of page is found in DALI with "GTC".#')
          TPRGDH=TP(3:3)
        ELSE
          TAN1DH=TP(1:3)
        END IF
      ELSE
        TPICDO=TCOM(1:2)
        CALL DGINMA(TCOM(3:8))
      END IF
      END
*DK DPCEWI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPCEAR
CH
      SUBROUTINE DPCEWI(NAR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter Copy: Execute all pictures on ARea "nar"
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
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
      DATA NOCL/0/
      LOGICAL F0
      CALL DPCGAR(NAR,F0)
      IF(F0) THEN
        CALL DWRT(' No picture on '//TAREDO(NAR))
      ELSE
        NOCLDT=NOCL
        CALL DBR2
      END IF
      END
*DK DPLANE_0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DPLANE_0
CH
      SUBROUTINE DPLANE_0(NPOS,TA,NEXEC,FYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     .............. This routine returns NPACT=1 in case of ATLANTIS;
C     .............. so it is practically dummy for ATLANTIS.

      CHARACTER *(*) TA
      LOGICAL FYES

      FYES=.FALSE.
      IF(NPOS.NE.0) THEN
        CALL DDRLOG(NPOS,TA,FYES)
        IF(FYES) RETURN
        CALL DPLANE(NPOS,TA,FYES,NEXEC)
        IF(FYES) RETURN
        CALL DPAR_POINTER(18,'PLA',JPLA)
        IF(NPOS.EQ.JPLA) FYES=.TRUE.
      END IF
      END
*DK DPLANE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPLANE
CH
      SUBROUTINE DPLANE(NP,TA,NYES,NEXEC)
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
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL NYES
      CHARACTER *2 TA
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PLA,J_PP1,J_PP2,J_PTO'
      CALL DPARAM(11
     &  ,J_PLA,J_PP1,J_PP2,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TA.EQ.'EO') THEN
        IF(NP.EQ.J_PP1) THEN
          PARADA(2,J_PP1)=1011.
          NYES=.TRUE.
          RETURN
        ELSE IF(NP.EQ.J_PLA) THEN
          PARADA(2,J_PP1)=1011.
          PARADA(2,J_PP2)=1011.
          NYES=.TRUE.
          RETURN
        END IF
      END IF
      IF(NP.GE.J_PLA.AND.NP.LE.J_PTO) THEN
         IF(TA.EQ.'TP') THEN
            NYES=.TRUE.
            PARADA(2,NP)=1004.
            IF(NP.EQ.J_PLA) THEN
               PARADA(2,J_PP1)=1001.
               PARADA(2,J_PP2)=1004.
            END IF
            RETURN
         END IF
         DO M=-2,11
            IF(TA.EQ.TPLNDO(M)) THEN
               NYES=.TRUE.
               PARADA(2,NP)=1000.+M
               IF(NP.EQ.J_PLA) THEN
                  PARADA(2,J_PP1)=PARADA(2,J_PLA)
                  PARADA(2,J_PP2)=PARADA(2,J_PLA)
               END IF
               RETURN
            END IF
         END DO
      END IF
      NYES=.FALSE.
      RETURN
      END
*DK DPCOR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPCOR
CH
      SUBROUTINE DPCOR(NP,TAP)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER TAP*2
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT,J_PDD,J_PCC'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT,J_PDD,J_PCC)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(     NP.EQ.J_PDD) THEN
         PARADA(2,J_PDF)=PARADA(2,NP)
         PARADA(2,J_PDT)=PARADA(2,NP)
      ELSE IF(NP.EQ.J_PCC) THEN
         PARADA(2,J_PCF)=PARADA(2,NP)
         PARADA(2,J_PCT)=PARADA(2,NP)
      END IF
      IF(NP.EQ.J_PFI) PARADA(2,J_PFI)=MOD(3600.+PARADA(2,J_PFI),360.)
      IF(NP.EQ.J_PTE) PARADA(2,J_PTE)=DMIX(0.,PARADA(2,J_PTE),180.)
      END
*DK DPOLCO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOLCO
CH
      SUBROUTINE DPOLCO(X,Y,Z,RO,T,F)
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
      RO=SQRT(X*X+Y*Y)
      T=DATN2D(RO,Z)
      F=DATN2D(Y ,X)
      END
*DK DPOLCR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOLCR
CH
      SUBROUTINE DPOLCR(X,Y,Z,R,T,F)
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
      R2=X*X+Y*Y
      R=SQRT(R2+Z*Z)
      IF(R.EQ.0.) THEN
        F=0.
        T=0.
      ELSE
        RO=SQRT(R2)
        T=DATN2D(RO,Z)
        IF(RO.EQ.0.) THEN
          F=0.
        ELSE
          F=DATN2D(Y,X)
        END IF
      END IF
      END
*DK DPOLCRV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOLCRV
CH
      SUBROUTINE DPOLCRV(XYZ,RFT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C ---------------------------------------------------------------------
      DIMENSION XYZ(3),RFT(3)
      R2=XYZ(1)*XYZ(1)+XYZ(2)*XYZ(2)
      RFT(1)=SQRT(R2+XYZ(3)*XYZ(3))
      IF(RFT(1).EQ.0.) THEN
        RFT(2)=0.
        RFT(3)=0.
      ELSE
        RO=SQRT(R2)
        RFT(3)=DATN2D(RO,XYZ(3))
        IF(RO.EQ.0.) THEN
          RFT(2)=0.
        ELSE
          RFT(2)=DATN2D(XYZ(2),XYZ(1))
        END IF
      END IF
      END
*DK DPOS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOS
CH
      SUBROUTINE DPOS(TANSW,FI,TE,NEXEC,CHG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  16-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *2 TPIC,TANSW
      LOGICAL CHG
      CALL DWRT('DPOS was called#')

C      CALL DO_STR('PI')
C      IF(TANSW.NE.'PI') RETURN
C      TPIC=TPICDO
C      IARE=IAREDO
C      CALL DPIPOS(FI,TE,NEXEC,CHG,TANSW)
C      IF(TPICDO.EQ.'GB') NEXEC=3
C      IF(NEXEC.EQ.3) TPICDO=TPIC
C      IF(NEXEC.NE.1) IAREDO=IARE
      END
*DK D_PERSONAL_MACRO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++ D_PERSONAL_MACRO
CH
      SUBROUTINE D_PERSONAL_MACRO
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Read personal macro file. *.PERSONAL.MAC .
C     Store parameters and  execute macro.
C                             PSTODS IS different from ATLANTIS.
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
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
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *40 TNEXT

      PARAMETER (MMAC=47)
      CHARACTER *47 TMAC

      CHARACTER *3 TPM,TPAR
      DATA TPM/'M$:'/

      CHARACTER *6 TFI
      DATA TFI/'GG:FI:'/

      DATA NUNIT/59/
      LOGICAL FNOP
      DATA FNOP/.TRUE./

      FMACDM=.FALSE.
      IF(FNOP) THEN
        CALL DGOPEN(NUNIT,TFILDC//'PERSONAL_MAC',2,*99,ISTAT)
        FNOP=.FALSE.
   10   READ(NUNIT,1000,END=9) TNEXT
        IF(TNEXT(1:1).EQ.' ') GO TO 10
      END IF
   40 TMAC=TNEXT

   20 READ(NUNIT,1000,END=1) TNEXT
 1000 FORMAT(1X,A)
      IF(TNEXT(1:1).EQ.' ') GO TO 20

      IF(TMAC(1:2).NE.'p:'.AND.TMAC(1:2).NE.'a:') THEN
        L=INDEX(TMAC,' ')
        IF(TMAC(L-1:L-1).EQ.':') L=L-1
        TMAC=TMAC(1:L)//TPM
      END IF
      GO TO 2

    1 CLOSE(UNIT=59)
      FNOP=.TRUE.

    2 IF(TMAC(1:2).NE.'p:'.AND.TMAC(1:2).NE.'a:') THEN
        CALL DGINMA(TMAC)
        RETURN

      ELSE
        L1=3
        CALL DTR_I(TMAC,L1,MMAC,NGR)
        CALL DTR_T(TMAC,L1,MMAC,3,TPAR)
        CALL DTR_I(TMAC,L1,MMAC,N1234)

        IF(N1234.GE.1.AND.N1234.LE.4) THEN
          CALL DTR_F(TMAC,L1,MMAC,VAL)
          CALL DPAR_POINTER(NGR,TPAR,NP)
          PARADA(N1234,NP)=VAL
          IF(TMAC(1:2).EQ.'a:') THEN
            IF(NP.LE.MPNPDS) THEN
              IF(     N1234.EQ.2) THEN
                I12=1
              ELSE IF(N1234.EQ.4) THEN
                I12=2
              ELSE
                GO TO 40
              END IF
              DO K=MVIRDS,MPARDS
                PSTODS(I12,NP,K,0)=VAL
              END DO
            END IF
          END IF
        END IF
        GO TO 40
      END IF

    9 CLOSE(UNIT=59)
      RETURN

   99 CALL DGINMA(TFI)
      END
