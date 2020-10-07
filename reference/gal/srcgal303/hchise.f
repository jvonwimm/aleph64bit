      SUBROUTINE HCHISE
C --------------------------------------------------------------
C - F.RANJARD - 860619            modified - 890801
C! order HTHT in increasing order of storey address
C  modify HCSE accordingly
C - Called by   HCASIG                                from this .HLB
C --------------------------------------------------------
      SAVE
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      PARAMETER(JHTHSA=1,JHTHPT=2,JHTHTT=3,JHTHED=4,LHTHTA=4)
      PARAMETER(JHCSTN=1,JHCSLN=2,JHCSMN=3,JHCSPN=4,JHCSTA=5,JHCSTX=6,
     +          JHCSTY=7,JHCSPW=8,JHCSXA=9,JHCSYA=10,JHCSZA=11,
     +          JHCSTR=12,JHCSHT=13,JHCSFT=14,JHCSFS=15,LHCSEA=15)
      INTEGER JDCKEY
      DATA JDCKEY /0/
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
C -------------------------------------------------------------------
      JHTHT = IW(NAHTHT)
      IF (JHTHT.EQ.0) GOTO 999
      NHTHT = LROWS(JHTHT)
      IF (NHTHT.EQ.0) GOTO 999
      IF (FHCDB1) THEN
         KHCSE = JDHCSE + LMHLEN
         WRITE (IW(6),802) (IW(K+JHCSFS),K=KHCSE,KHCSE+LROWS(JDHCSE)*
     &                      LHCSEA-1,LHCSEA)
      ENDIF
C
C - get the positions in HTHT of storey addresses
      CALL WBANK (IW,JDCKEY,NHTHT,*998)
      DO 1 I=1,NHTHT
         IW(JDCKEY+I) = (I-1)*LHTHTA + JHTHSA
 1    CONTINUE
C
C - get the position in HTHT of storey addresses when they are in
C   increasing order
      CALL SORTZV (IW(JHTHT+LMHLEN+1),IW(JDCKEY+1),NHTHT,-1,0,1)
C
C - get the new row # instead of the new position
      DO 2 I=1,NHTHT
         IW(JDCKEY+I) = (IW(JDCKEY+I) - JHTHSA)/LHTHTA + 1
 2    CONTINUE
C
C - update HCSE
      KHCSE = JDHCSE + LMHLEN
      NHCSE = LROWS(JDHCSE)
      DO 3 I=1,LROWS(JDHCSE)
         IF (IW(KHCSE+JHCSFS).EQ.0) GOTO 3
         IW(KHCSE+JHCSFS) = IUCOMP(IW(KHCSE+JHCSFS),IW(JDCKEY+1),NHTHT)
 3    KHCSE = KHCSE + LHCSEA
      KHCSE = JDHCSE + LMHLEN
C
C - reoder HTHT in increasing order of storey address
      CALL SORTIQ (IW(JHTHT+LMHLEN+1),LHTHTA,NHTHT,JHTHSA)
C
C - debug
      IF (FHCDB1) THEN
         WRITE (IW(6),'(/1X,''+++HCHISE+++ HTHT in increasing order'')')
         KHTHT = JHTHT + LMHLEN
         WRITE (IW(6),801) ((IW(K+I),I=1,3),RW(K+4),K=KHTHT,KHTHT+
     &                      NHTHT*LHTHTA-1,LHTHTA)
         KHCSE = JDHCSE + LMHLEN
         WRITE (IW(6),802) (IW(K+JHCSFS),K=KHCSE,KHCSE+LROWS(JDHCSE)*
     &                      LHCSEA-1,LHCSEA)
      ENDIF
 999  CONTINUE
      CALL WDROP (IW,JDCKEY)
      RETURN
 801  FORMAT (3(I10,I4,I4,F10.5,5X))
 802  FORMAT (/1X,'+++HCHISE+++ HCSE word(15): HTHT row n0.'/(5X,25I4))
C - NOT enough space to enlarge work bank
 998  CALL WDROP (IW,JDCKEY)
      CALL ALTELL ('HCHISE: BOS array too small',1,'NEXT')
      END
