      SUBROUTINE MUFLTD
C
C***********************************************************************
C
C T.Wang -860118
C
C! make cross reference between digit and tracks,
C       then fill BOS bank 'MUTD'.
C       This routine is called once per event.
C
C       Input:
C         NDIGI -- the current # of digi in BOS bank 'MUDG' and 'MUDT'
C
C       Output:
C         NTRK  -- # of tracks which generate these digi
C       RETURN 1 if not enough space to book MUTD
C
C       Called by MUDGTZ
C       Calls ALBOS                   in this .HLB
C       Calls BKFMT,BLIST             in BOS
C
C***********************************************************************
C
      SAVE
      PARAMETER (MXPHT = 200, MXDHT =  40, MXEMD = 94)
      PARAMETER (MXTRK = 100, MXTDI = 100)
C
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER NTDI(MXTRK),ITRK(MXTRK),ITDI(MXTDI,MXTRK)
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
C       Get index for BOS bank 'MUDT'
C
      KMDT1 = IW(NAMUDT)
      KMDT2 = IW(KMDT1 - 1)
      KMDT3 = IW(KMDT2 - 1)
      KMDT1 = KMDT1 + LMHLEN
      KMDT2 = KMDT2 + LMHLEN
      KMDT3 = KMDT3 + LMHLEN
      MTRK = IW(KMDT3)
C - Check local array lengths
      IF (MTRK .GT. MXTRK) GOTO 98
C
C       Cross reference for track-digi
C
      NTRK = 0
      IDG = 0
      DO 40 IT=1,MTRK
         JTRK = IW(KMDT3 + IT)
         IF( IT .GE. IW(KMDT2 + IDG + 1) )THEN
            IDG = IDG + 1
         ENDIF
         IF( NTRK .EQ.  0)GOTO 20
         DO 10 JT=1,NTRK
            IF( JTRK .EQ. ITRK(JT) )THEN
               NTDI(JT) = NTDI(JT) + 1
               IF (NTDI(JT) .GT. MXTDI) GOTO 98
               ITDI(NTDI(JT),JT) = IDG
               GOTO 30
            ENDIF
   10    CONTINUE
   20    NTRK = NTRK + 1
         ITRK(NTRK) = JTRK
         NTDI(NTRK) = 1
         ITDI(1,NTRK) = IDG
   30    CONTINUE
   40 CONTINUE
C
C       Create BOS bank 'MUTD'
C
      CALL ALBOS ('MUTD',1,NTRK+LMHLEN,JMTD1,IGAR1)
      IW(JMTD1+1) = 1
      CALL ALBOS ('MUTD',2,NTRK+LMHLEN,JMTD2,IGAR2)
      IW(JMTD2+1) = 1
      CALL ALBOS ('MUTD',3,NTRK+LMHLEN,JMTD3,IGAR3)
      IW(JMTD3+1) = 1
      CALL ALBOS ('MUTD',4,MTRK+LMHLEN,JMTD4,IGAR4)
      IW(JMTD4+1) = 1
      CALL BLIST (IW,'E+','MUTD')
      IF (IGAR1+IGAR2+IGAR3+IGAR4 .NE. 0) THEN
         JMTD1 = IW(NAMUTD)
         JMTD2 = IW(JMTD1-1)
         JMTD3 = IW(JMTD2-1)
         JMTD4 = IW(JMTD3-1)
      ENDIF
      KMTD1 = JMTD1 + LMHLEN
      KMTD2 = JMTD2 + LMHLEN
      KMTD3 = JMTD3 + LMHLEN
      KMTD4 = JMTD4 + LMHLEN
C
C       Fill BOS bank 'MUTD'
C
      IW(KMTD3 + 1) = 0
      DO 60 JT=1,NTRK
         IW(KMTD1 + JT) = ITRK(JT)
         IW(KMTD2 + JT) = NTDI(JT)
         IF( JT .NE. 1)THEN
            IW(KMTD3 + JT) = IW(KMTD3 + JT - 1) + NTDI(JT - 1)
         ENDIF
         DO 50 JDI=1,NTDI(JT)
            IW(KMTD4 + IW(KMTD3+JT) + JDI) = ITDI(JDI,JT)
   50    CONTINUE
   60 CONTINUE
      IW(KMTD1) = NTRK
      IW(KMTD2) = NTRK
      IW(KMTD3) = NTRK
      IW(KMTD4) = IW(KMTD3+NTRK) + NTDI(NTRK)
      RETURN
C
 98   CALL ALTELL ('MUFLTD: local arrays are too small',2,'RETURN')
C
      END
