      SUBROUTINE MUFLHT (JPLAN,IS,NS)
C
C***********************************************************************
C
C T.Wang -860108
C
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C       Routine to fill a induced strip signal into BOS bank 'MUHT'
C
C       Input:
C         JPLAN -- the tube plane where the signal is ( 1 or 2 );
C         IS    -- the index of X- or Y- strip;
C                1 -- X
C                2 -- Y
C         NS    -- the strip number in the current strip plane;
C
C       Output:
C         Elements in BOS bank 'MUHT'
C         RETURN 1 if not enough space to enlarge MUHT
C
C       Called by MUSGNL
C       Calls ALBOS               in this .HLB
C
C***********************************************************************
C
      SAVE
      PARAMETER(JMUHTN=1,JMUHEL=2,JMUHSP=3,JMUHSA=4,LMUHTA=4)
C! The current constants to create MU signals
      COMMON/MUSGCN/WDMDMU,XLMDMU,NMEIMU,XLEIMU,KPMDMU,ZPSWMU(2),
     *              NASLMU,NUSLMU,NUEMMU,NYSTMU,NXSTMU,XSOFMU(2),
     *              YSOFMU(2),GP16MU,WD16MU,YCUTSL(2),YCUTSB(2)
      COMMON/MUSGKN/   TMUCVO,TMU3VO
      CHARACTER*4 TMUCVO,TMU3VO
C
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
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
C       Look up the list of dead strips, decide if a hit will be
C       written into the bank.
C
      IF( ICMUJO(2) .NE. 0 )CALL MUDSTP
      IF(IS.EQ.1) NSTCU = NXSTMU
      IF(IS.EQ.2) NSTCU = NYSTMU
      IF(NS.GT.NSTCU) RETURN
      IF(NS.LE.0) RETURN
C
C       Get index of BOS bank 'MUHT'
C
      JMUHT = IW(NAMUHT)
      IF (JMUHT.EQ.0) RETURN
C
      IF (LFRROW(JMUHT) .LE. 0) THEN
         ND = IW(JMUHT) + IW(JMUHT)/2
         CALL ALBOS ('MUHT',0,ND,JMUHT,IGARB)
      ENDIF
C
      KMUHT = KNEXT(JMUHT)
      IW(KMUHT + JMUHTN) = ITRKEL(1)
C
C       In BOS bank 'MUHT', index of strip plane from 1-4
C         Strip # 1 -- X1
C                 2 -- Y1
C                 3 -- Y2
C                 4 -- X2
C
      IW(KMUHT + JMUHSP) = (JPLAN-1)*(5-2*IS) + IS
      IW(KMUHT + JMUHEL) = NUEMMU
      IW(KMUHT + JMUHSA) = NS - 1
C
      IW(JMUHT + LMHROW) = LROWS(JMUHT) + 1
C
      END
