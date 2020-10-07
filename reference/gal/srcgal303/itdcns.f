      SUBROUTINE ITDCNS
C.
C...ITDCNS  2.11  930721  18:03                          R.Beuselinck
C.
C!  Get constants controlling drift relation and resolution.
C.
C.  Called by: ITIRUN                                  from this .HLB
C.      Calls: NAMIND                                  from BOS77
C.
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      COMMON /ITELEC/TDCRIT,TOFFIT,TFINIT(8),ITTDCL(8),ITTDCH(8),
     +               TDCZIT,ITZTZB,ZEXPIT,ZPARIT(3),ITZTDL(8),
     +               ITZTDH(8),ZLOFIT(8),ZRESIT(2,8)
C
      COMMON/ITPARC/DVELIT(8,5),HWEFIT
      REAL DVELIT,HWEFIT
C
      COMMON /ITRESC/RESITP(5,8),RESITN(5,8)
      REAL RESITP,RESITN
C
C
      EXTERNAL NAMIND
      PARAMETER(JIDRID=1,JIDRVR=2,JIDRTO=4,JIDRCO=5,LIDRPA=9)
      PARAMETER(JIEFMX=1,LIEFFA=1)
      PARAMETER(JIRRID=1,JIRRVR=2,JIRRMX=4,JIRRMN=5,JIRRCP=6,JIRRCN=11,
     +          LIRRFA=15)
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
C--  Get constants for parametrization and efficiency.
C--
      IND = NAMIND('IEFF')
      IND = IW(IND)
      HWEFIT = RTABL(IND,1,JIEFMX)
C
C--  Get resolution parameters from database.
C--
      IND = IW(NAMIND('IRRF'))
      NR = LROWS(IND)
      IF (NR.NE.8)
     +  CALL ALTELL('ITDCNS: Wrong number of layers in IRRF',0,'STOP')
      DO 40 I=1,NR
        JROW = KROW(IND,I)
        LAY = I
        DO 35 J=1,5
          RESITP(J,I) = RW(JROW+JIRRCP+J-1)
          RESITN(J,I) = RW(JROW+JIRRCN+J-1)
   35   CONTINUE
   40 CONTINUE
      IND = IW(NAMIND('IDRP'))
      NR = LROWS(IND)
      IF (NR.NE.8)
     +  CALL ALTELL('ITDCNS: Wrong number of layers in IDRP',0,'STOP')
      DO 50 I=1,NR
        JROW = KROW(IND,I)
        LAY = I
        TFINIT(LAY) = RW(JROW+JIDRTO)
        DVELIT(LAY,1) = RW(JROW+JIDRCO)
        DVELIT(LAY,2) = RW(JROW+JIDRCO+1)
        DVELIT(LAY,3) = RW(JROW+JIDRCO+2)
        DVELIT(LAY,4) = RW(JROW+JIDRCO+3)
        DVELIT(LAY,5) = RW(JROW+JIDRCO+4)
   50 CONTINUE
C
C--  Initialise interpolation table for drift time.
C--
      CALL ITDTGO
C
      END
