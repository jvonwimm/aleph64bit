      SUBROUTINE HCHIST (NAME,MR,NAMI,IELE,JELE)
C ----------------------------------------------------------
C - F.Ranjard - 860613
C! - Fill the history bank NAME,(NR=1,MR) with namindex NAMI
C   for that the bank HCSE is sorted in increasing order of
C   element# IELE and then for a each value of element# IELE
C   in increasing order of element# JELE.
C - If the element# IELE is the same for 2 concecutive calls to
C   HCHIST the entry point HCHIS1 can be used instead to save
C   the 1st sort.
C -       NAME, NR=MR-2   # of element JELE belonging to 1 element IELE
C               NR=MR-1   pointer to 1st element JELE belonging to 1
C                         element IELE in the list NR=MR
C               NR=MR     list of elements JELE belonging to each elemen
C                         IELE
C - If MR=4 the correlation is between tracks and hits. In this case
C         NAME, NR=1      list of track#s which have contributed to hits
C - Called by    HCASIG                                 from this .HLB
C - Calls        SORTIQ                                 from GENLIB
C                ALBOS                                  from this .HLB
C ----------------------------------------------------------
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
      PARAMETER(JHCSTN=1,JHCSLN=2,JHCSMN=3,JHCSPN=4,JHCSTA=5,JHCSTX=6,
     +          JHCSTY=7,JHCSPW=8,JHCSXA=9,JHCSYA=10,JHCSZA=11,
     +          JHCSTR=12,JHCSHT=13,JHCSFT=14,JHCSFS=15,LHCSEA=15)
      DIMENSION IPELE(1000), KDX(4)
      CHARACTER NAME*4  , TITL(4)*7
C
      DATA TITL /'tracks ','hits   ','tubes  ','storeys'/
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
C ----------------------------------------------------------------------
C
      KHCSE = JDHCSE + LMHLEN
      NHCSE = LROWS (JDHCSE)
      CALL SORTIQ (IW(KHCSE+1),LHCSEA,NHCSE,IELE)
C
      NELE = 0
      LELE = 0
      DO 10 I = 1,NHCSE
         IF (IW(KHCSE+IELE) .EQ. LELE) GOTO 10
         NELE = NELE + 1
         IPELE(NELE) = I
         LELE = IW(KHCSE+IELE)
   10 KHCSE = KHCSE + LHCSEA
      IPELE(NELE+1) = NHCSE + 1
C
      IF (FHCDB1) THEN
         IF (NLINK('DBHC',0).NE.0) THEN
            KHCSE = JDHCSE + LMHLEN
            WRITE (LOUTIO,'(/'' +++HCHIST+++''/'' trk# hit# tub# ''
     +            ,''sto#'')')
            WRITE (LOUTIO,500)((IW(K+M),M=12,15), K=KHCSE,KHCSE+NHCSE
     +      *LHCSEA-1,LHCSEA)
         ENDIF
      ENDIF
C
C ---------------------------------------------------------------------
C
      ENTRY HCHIS1 (NAME,MR,NAMI,IELE,JELE)
C
C - Book bank NAME , NR=1,MR
      DO 20 J=1,MR-1
         CALL ALBOS (NAME,J,NELE+LMHLEN,KIND,IGARB)
         IW(KIND+1) = 1
         IW(KIND+2) = NELE
   20 CONTINUE
      CALL ALBOS (NAME,MR,NHCSE+LMHLEN,KIND,IGARB)
      IW(KIND+1) = 1
      CALL BLIST (IW,'E+',NAME)
C
C - Get indices
      KDX(1) = IW(NAMI) + LMHLEN
      DO 30 J=2,MR
         KDX(J) = IW(KDX(J-1)-LMHLEN-1) + LMHLEN
   30 CONTINUE
C
C - FIll banks
      K0MR = KDX(MR)
      DO 50 N=1,NELE
         NHIT = IPELE(N+1) - IPELE(N)
         IW(KDX(MR-1)+N) = KDX(MR)-K0MR
         KHCSE = KROW(JDHCSE,IPELE(N))
         IF (MR.EQ.4) IW(KDX(1)+N) = IW(KHCSE+IELE)
C
         CALL SORTIQ (IW(KHCSE+1),LHCSEA,NHIT,JELE)
C
         LJELE = 0
         NJELE = 0
         DO 40 L=1,NHIT
            IF (IW(KHCSE+JELE) .EQ. LJELE) GOTO 40
            LJELE = IW(KHCSE+JELE)
            NJELE = NJELE + 1
            IW (KDX(MR)+NJELE) = LJELE
   40    KHCSE = KHCSE + LHCSEA
         KDX(MR) = KDX(MR) + NJELE
         IW(KDX(MR-2)+N) = NJELE
   50 CONTINUE
C - fill # of rows of bank NAME , NR=MR
      IW(KIND+LMHROW) = KDX(MR)-K0MR
      CALL AUBPRS (NAME)
C
C - Debug
      IF (FHCDB1) THEN
         KDX(MR) = K0MR
         WRITE (LOUTIO,510) NAME,MR,TITL(IELE-11),TITL(JELE-11)
         IF (MR.EQ.4) THEN
            WRITE (LOUTIO,520) TITL(JELE-11)
         ELSE
            WRITE (LOUTIO,530) TITL(IELE-11)
         ENDIF
         DO 60 I=1,NELE
            N=IW(KDX(MR-2)+I)
            L=IW(KDX(MR-1)+I)
            IF (MR.EQ.4) THEN
               WRITE (LOUTIO,540) I,IW(KDX(1)+I),N,L,(IW(KDX(MR)+L+M),
     +         M =1,N)
            ELSE
               WRITE (LOUTIO,550) I,N,L,(IW(KDX(MR)+L+M),M=1,N)
            ENDIF
   60    CONTINUE
      ENDIF
C
C - END
   70 CONTINUE
      RETURN
  500 FORMAT (1X,4I5)
  510 FORMAT (/1X,'+++HCHIST+++ Bank ',A4,' NR=1-',I1,
     +' giving correlations between ',A7,' and ',A7/)
  520 FORMAT (5X,'trk#  nhit iadd  list of address of fired ',A7)
  530 FORMAT (5X,'nhit iadd  list of tracks which fired this ',A7)
  540 FORMAT (1X,I4,3I5,2X,20I4/(22X,20I4))
  550 FORMAT(1X,I4,2I5,2X,20I4/(17X,20I4))
      END
