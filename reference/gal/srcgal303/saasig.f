      SUBROUTINE SAASIG
C-----------------------------------------------------------------------
C!    Delete multihits and reduce SAHI Bank to actual size
C   called at the end of the event before SADIGI by ASASIG
C                                   H.Burkhardt   October 1986
C-----------------------------------------------------------------------
      SAVE
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
      PARAMETER(LCSAHT=1,MXSAHT=500,LCSADI=1)
      COMMON/SANAMC/NASAHT,NASADI
      COMMON/SATRCO/NSECSA,NCRASA,NCARSA,NTDCSA,NSIDSA,NLAYSA,NBRASA,
     .      RMINSA,RMAXSA,BRTHSA,ZLUMSA,DEADSA,DGASSA,COSTSA,
     .      NHITSA,NLOSSA,NWIRSA,XHITSA,XLOSSA,XWIRSA,XEVTSA,
     .      NHLASA(9),NHLDSA(9),PHIBSA(9)
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
C if multihits are in the same brass tube (same address)
C keep only the hit which is the closest to the wire
      KSAHT=IW(NASAHT)
      IF(KSAHT.EQ.0) GOTO 900
      NHIT=LROWS(KSAHT)
      KSA=KSAHT+LMHLEN
      DO 1 IT1=1,NHIT-1
        ILOW=IT1+1
        IF(IW(KSA+IT1).EQ.0) GOTO 1
        IAD1=IBITS(IW(KSA+IT1),16,16)
        DO 2 IT2=ILOW,NHIT
          IF(IW(KSA+IT2).EQ.0) GOTO 2
          IAD2=IBITS(IW(KSA+IT2),16,16)
          IF(IAD1.EQ.IAD2) THEN
C           look for shorter dist, mark other hit for deletion
C           by putting the word to zero
            IF(IBITS(IW(KSA+IT1),0,16).LT.
     &         IBITS(IW(KSA+IT2),0,16)) THEN
              IW(KSA+IT2)=0
            ELSE
              IW(KSA+IT1)=0
              GOTO 1
            ENDIF
          ENDIF
    2   CONTINUE
    1 CONTINUE
      IND=0
      DO 3 IT3=1,NHIT
C       copy, skip the hits to be deleted
        IF(IW(KSA+IT3).NE.0) THEN
          IND=IND+1
          IW(KSA+IND)=IW(KSA+IT3)
        ENDIF
    3 CONTINUE
      IF(IND.LT.NHIT) THEN
C       only change bank size if multihits have been found
C       number of rows
        IW(KSAHT+2)=IND
C       now reduce SAHT bank to actual size
        CALL AUBPRS('SAHT')
      ENDIF
      IF(FDEBJO.AND.IPRIJO(6).EQ.1) THEN
         WRITE (LOUTIO,'(/1X,''+++SAASIG+++ # of deleted double hits= ''
     &                      ,I4)') NHIT-IND
         CALL SAPRHI
      ENDIF
  900 CONTINUE
      END
