      SUBROUTINE HCASIG
C --------------------------------------------
C
C! -   Create analog signal
C!
C!     Author G. Zito  - 86/05/21
C!     Modified by G.Catanesi 87/10/27
C!     Modified by F.Ranjard  89/02/10
C!
C!     Input bank: JDHCSE
C!     Output banks:JDHCHI,HWHT,HTHT       'E' list
C!
C!    -Called by ASASIG
C!       -Calls : HCGEST, HCCRDI, HCINDU, HCCRTO, HCCRPL
C!           HCHISE,HCHIST,HCSTAT          from this .HLB
C!           SORTIQ                        from CERNLIB
C!
C -------------------------------------------
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
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      PARAMETER(JHCSTN=1,JHCSLN=2,JHCSMN=3,JHCSPN=4,JHCSTA=5,JHCSTX=6,
     +          JHCSTY=7,JHCSPW=8,JHCSXA=9,JHCSYA=10,JHCSZA=11,
     +          JHCSTR=12,JHCSHT=13,JHCSFT=14,JHCSFS=15,LHCSEA=15)
      PARAMETER(JHCHTS=1,JHCHSY=2,JHCHSP=3,JHCHNS=4,LHCHIA=4)
      PARAMETER(JHCTTN=1,JHCTPN=2,JHCTEH=3,JHCTEV=4,JHCTEM=5,JHCTVF=6,
     +          LHCTHA=6)
      PARAMETER(JHTHSA=1,JHTHPT=2,JHTHTT=3,JHTHED=4,LHTHTA=4)
      PARAMETER(JHWHTA=1,LHWHTA=1)
      PARAMETER(JHPHPA=1,JHPHED=2,LHPHTA=2)
      PARAMETER(JHLWWA=1,JHLWHP=2,LHLWDA=2)
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
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
C --------------------------------------------------------
C        Do some statistics and print track elements
C
      IF (JDHCSE.EQ.0) RETURN
      NHCSE = LROWS (JDHCSE)
      IF (NHCSE .EQ. 0) GOTO 30
      KHCSE = JDHCSE + LMHLEN
C - order HCSE in increasing order of tube address
      CALL SORTIQ(IW(KHCSE+1),LHCSEA,NHCSE,5)
C
      IF(FHCDEB)THEN
C
         WRITE(LOUTIO,500)NHCSE
         DO 10 I = 1, NHCSE
            WRITE(LOUTIO,510)I,(IW(KHCSE+II),II=1,5),(RW(KHCSE+II),II
     +      =6,11) ,IW(KHCSE+12)
            KHCSE = KHCSE + LHCSEA
   10    CONTINUE
C
         IF (FHCDB2 .AND. JDHCTH.GT.0) THEN
            CALL SORTIQ(IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH),LROWS(JDHCTH),
     &                  1)
         ENDIF
C
      ENDIF
C
C
C      Generate streamers:  JDHCSE -> JDHCHI (1-4)
C
      CALL WBANK(IW,JDHCHI,NHCSE*LHCHIA+LMHLEN,*20)
      IW(JDHCHI-3) = INTCHA ('HCHI')
      IW(JDHCHI+1) = LHCHIA
      IW(JDHCHI+2) = 0
      CALL HCGEST
C
C     Create digital pattern: JDHCHI -> HWHT
C     and fill word(14) of HCSE with the HWHT row #
C
      CALL ALBOS ('HWHT',0,NHCSE*LHWHTA+LMHLEN,JHWHT,IGARB)
      IW(JHWHT+1) = LHWHTA
      IW(JHWHT+2) = 0
      CALL BLIST(IW,'E+','HWHT')
      CALL HCCRDI
C
C     Remember the hit position along the tube
C     JDHCHI + HCSE -> HLWD
      NHCHI = LROWS(JDHCHI)
      CALL ALBOS ('HLWD',0,NHCHI*LHLWDA+LMHLEN,JHLWD,IGARB)
      IW(JHLWD+1) = LHLWDA
      IW(JHLWD+2) = 0
      CALL BLIST(IW,'E+','HLWD')
      CALL HCHLWD
C
C     Take in account the induction effect on digital pattern
C
      IF(ICHCJO(4).EQ.0)THEN
         CALL HCINDU
         IF(FHCDEB)THEN
            JHWHT = IW(NAHWHT)
            NHWHT = LROWS (JHWHT)
            KHWHT = JHWHT + LMHLEN
            WRITE(LOUTIO,520)NHWHT
            WRITE(LOUTIO,530) (IW(KHWHT+I), I=1,NHWHT*LHWHTA,LHWHTA)
         ENDIF
      ENDIF
C
C
C     Create tower storey pattern: JDHCSE + JDHCHI -> 'HTHT'
C     and fill HCSE word(15) with the HTHT row #
C
      CALL ALBOS ('HTHT',0,NHCSE*LHTHTA+LMHLEN,JHTHT,IGARB)
      IW(JHTHT+1) = LHTHTA
      IW(JHTHT+2) = 0
      CALL BLIST(IW,'E+','HTHT')
      CALL HCCRTO
C
C     Create planes signals: JDHCSE + JDHCHI -> 'HPHT'
C
      CALL ALBOS ('HPHT',0,NHCSE*LHPHTA+LMHLEN,JHPHT,IGARB)
      IW(JHPHT+LMHCOL) = LHPHTA
      IW(JHPHT+LMHROW) = 0
      CALL BLIST(IW,'E+','HPHT')
      CALL HCCRPL
C
C     order HTHT in increasing order of  storey address
C     modify HCSE accordingly
C
      CALL HCHISE
C
C     Do some statistics on analog signals
C
      CALL HCSTAT
C
C - if GALEPH flag write history banks
      IF (FGALJO) THEN
C
C - create history banks 'HWTD' and 'HTTD' which give the correlation
C   between track# and fired tubes / fired storeys.
C
         CALL HCHIST ('HWTD',4,NAHWTD,12,14)
         CALL HCHIS1 ('HTTD',4,NAHTTD,12,15)
      ENDIF
C
C - compress output banks
      CALL AUBPRS ('HTHTHWHTHWTDHTTDHPHT')
C
      GOTO 30
C
C - no enough space to book working bank
C
   20 CONTINUE
      CALL WDROP (IW,JDHCSE)
      CALL WDROP (IW,JDHCHI)
      CALL ALTELL('HCASIG: not enough space to book work bank',1,'NEXT')
C
   30 CONTINUE
C       Drop here temporary banks
C
      CALL WDROP (IW,JDHCSE)
      CALL WDROP(IW,JDHCHI)
C
      RETURN
  500 FORMAT (/1X,'+++HCASIG+++ HCSE  McHcTubeSegments ',I5/ 1X,
     +'seg#  Tu#  La#  Mo#  Po#  Tub.add.    XTube     YTube  ',
     +' Proj.on.wi  XAleph    YAleph    ZAleph  Trk#')
  510 FORMAT (5I5,I10,6F10.2,I5)
  520 FORMAT (1X,'+++ HCASIG +++ HWHT with induction effect ',I5)
  530 FORMAT(1X,10I10)
      END
