      SUBROUTINE HCFOPL
C--------------------------------------------------------------------
C
C!  Implements the module FORMAT PLANE SIGNAL
C!
C!          Author : G.Catanesi 03/02/1989
C!
C!          Input bank : HPHT
C!          Output bank: HPDI
C!
C!
C!        -Called by : HCDIGI
C!        -Calls     : SORTIQ,MVBITS from CERNLIB
C ------------------------------------------
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
      PARAMETER(JHPHPA=1,JHPHED=2,LHPHTA=2)
      PARAMETER(JHPDPA=1,JHPDED=2,LHPDIA=2)
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
C ------------------------------------------------------------
C
      JHPHT = IW(NAHPHT)
      IF (JHPHT.EQ.0) RETURN
      NHPHT = LROWS (JHPHT)
      IF (NHPHT.EQ.0) RETURN
C
C      Loop on Planes
C
      KHPHT = JHPHT +LMHLEN
C
      CALL SORTIQ (IW(KHPHT+1),LHPHTA,NHPHT,1)
C
      JHPDI = IW(NAHPDI)
      KHPDI = JHPDI + LMHLEN
C
      NHPDI = 0
      IPL1 = 0
      IMO1 = 0
      DO 10 I = 1, NHPHT
         N = IW(KHPHT+1)
         IPL = MOD(N,100)
         N = N / 100
         IMO = MOD(N,100)
         N = N / 100
         IPO = N
         NED = INT(RW(KHPHT+2)*1000.)
         IF(IMO.NE.IMO1.OR.IPL.NE.IPL1) THEN
            IMO1 = IMO
            IPL1 = IPL
            NHPDI = NHPDI + 1
            KHPDI = KROW(JHPDI,NHPDI)
            IPLM = IPL
            CALL MVBITS(IPLM,0,8,IW(KHPDI+JHPDPA),0)
            IMOM = IMO
            CALL MVBITS(IMOM,0,8,IW(KHPDI+JHPDPA),8)
C
            CALL MVBITS(IPO,0,8,IW(KHPDI+JHPDPA),16)
         ENDIF
C
            IW(KHPDI+JHPDED) = NED
         KHPHT = KHPHT + LHPHTA
   10 CONTINUE
C
      IW(JHPDI+LMHROW) = NHPDI
C
      IF(FHCDEB)THEN
         WRITE(LOUTIO,500) NHPDI
         KHPDI = JHPDI + LMHLEN
         DO 20 I = 1, NHPDI
            IPL = IBITS(IW(KHPDI+JHPDPA),0,8)
            IMO = IBITS(IW(KHPDI+JHPDPA),8,8)
            IPO = IBITS(IW(KHPDI+JHPDPA),16,8)
            IEPLA = IW(KHPDI+JHPDED)
            WRITE(LOUTIO,510)I,IPL,IMO,IPO,IEPLA
            KHPDI = KHPDI + LHPDIA
   20    CONTINUE
      ENDIF
      RETURN
  500 FORMAT (/1X,'+++HCFOPL+++ HPDI McHcPlanesDigitising ',I5/
     +' Plan#  Pla  Mod  Por     Energy ')
  510 FORMAT(4I5,I10)
      END
