      SUBROUTINE MUFLDT (NDIGI,NHTCL,IADHT)
C
C***********************************************************************
C
C T.Wang -860117
C
C       Routine to fill a digit-track reference of one digit
C       to BOS bank 'MUDT'
C
C       Input:
C         NDIGI -- the current # of digits
C         NHTCL -- the # of hits that contribute to the current digit(cl
C         IADHT -- array to contain the addresses of NHTCL hits in 'MUHT
C
C       Output:
C         in BOS bank 'MUDT'
C         RETURN 1 if not enough space to enlarge MUDT-3
C
C       Called by MUDGTZ
C       Calls ALBOS            in this .HLB
C
C***********************************************************************
C
      SAVE
      PARAMETER (MXPHT = 200, MXDHT =  40, MXEMD = 94)
      PARAMETER (MXTRK = 100, MXTDI = 100)
C
      PARAMETER(JMUHTN=1,JMUHEL=2,JMUHSP=3,JMUHSA=4,LMUHTA=4)
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
      INTEGER IADHT(*),IDTRK(MXDHT)
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
C       Get index for banks needed
C
      JMUHT = IW(NAMUHT)
      JMDT1 = IW(NAMUDT)
      JMDT2 = IW(JMDT1-1)
      JMDT3 = IW(JMDT2-1)
C
      KMDT1 = JMDT1 + LMHLEN
      KMDT2 = JMDT2 + LMHLEN
C
      NTRCK = 0
      DO 20 IHTCL=1,NHTCL
         KMUHT = KROW ( JMUHT,IADHT(IHTCL))
         JTRCK = IW(KMUHT + JMUHTN)
         IF(NTRCK .EQ. 0)THEN
            IDTRK(1) = JTRCK
            NTRCK = 1
         ENDIF
         DO 10 IT=1,NTRCK
            IF(JTRCK .EQ. IDTRK(IT))GOTO 20
   10    CONTINUE
         NTRCK = NTRCK + 1
         IF (NTRCK .GT. MXDHT) GOTO 98
         IDTRK(NTRCK)= JTRCK
   20 CONTINUE
      IW(KMDT1 + NDIGI) = NTRCK
      IF (NDIGI .GT. 1) IW(KMDT2+NDIGI) = IW(KMDT2+NDIGI-1)
     &                                   + IW(KMDT1+NDIGI-1)
C
      IW(KMDT1) = IW(KMDT1) + 1
      IW(KMDT2) = IW(KMDT2) + 1
C
C       Fill 'MUDT'-3
C
      IF (LFRWRD(JMDT3).LT. NTRCK) THEN
         CALL ALBOS ('MUDT',3,IW(JMDT3)+NAVTD*NTRCK,JMDT3,IGARB)
      ENDIF
      KMDT3 = JMDT3 + LMHLEN
      DO 30 IT=1,NTRCK
         IW(KMDT3 + IW(KMDT2+NDIGI) + IT) = IDTRK(IT)
   30 CONTINUE
      IW(KMDT3) = IW(KMDT3) + NTRCK
C
      RETURN
C
   98 CALL ALTELL ('MUFLDT: too many tracks for local array',2,'RETURN')
C
      END
