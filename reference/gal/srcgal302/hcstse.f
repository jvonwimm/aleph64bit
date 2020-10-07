      SUBROUTINE HCSTSE(VIN,IPOR,IMOD,ILAY,XTUB,YTUB,DYTUB,ITUB,NTUB)
C ----------------------------------------------------
C - F.Ranjard - 860529
C-  mod by G.Catanesi 871021
C - mod by F.Ranjard  890523 : do not keep negative tube #
C! - Fill the bank JDHCSE with the NTUB fired tubes with
C!   numbers listed in ITUB from plane # IPLAN
C
C ------------------------------------------
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
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      PARAMETER(JHCSTN=1,JHCSLN=2,JHCSMN=3,JHCSPN=4,JHCSTA=5,JHCSTX=6,
     +          JHCSTY=7,JHCSPW=8,JHCSXA=9,JHCSYA=10,JHCSZA=11,
     +          JHCSTR=12,JHCSHT=13,JHCSFT=14,JHCSFS=15,LHCSEA=15)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      DIMENSION VIN(*),XTUB(*),YTUB(*),DYTUB(*),ITUB(*)
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
C
      ND = NTUB*LHCSEA
      IF (LFRWRD(JDHCSE) .LT. ND) THEN
         ND = MAX (ND,IW(JDHCSE)/2)
         CALL WBANK (IW,JDHCSE,IW(JDHCSE)+ND,*20)
      ENDIF
      KHCSE = KNEXT (JDHCSE)
      DO 10 J = 1, NTUB
         IF (ITUB(J) .LT. 0) GOTO 10
         IW(KHCSE+1) = ITUB(J)
         IW(KHCSE+2) = ILAY
         IW(KHCSE+3) = IMOD
         IW(KHCSE+4) = IPOR
         ITADD = ITUB(J) + ILAY*1000+IMOD*100000+IPOR*10000000
         IW(KHCSE+5) = ITADD
         RW(KHCSE+6) = XTUB(J)
         RW(KHCSE+7) = YTUB(J)
         RW(KHCSE+8) = DYTUB(J)
         RW(KHCSE+9) = VIN(1)
         RW(KHCSE+10) = VIN(2)
         RW(KHCSE+11) = VIN(3)
         IW(KHCSE+12) = ITRKEL(1)
C
         IW(JDHCSE+LMHROW) = IW(JDHCSE+LMHROW) + 1
         KHCSE = KHCSE + LHCSEA
 10   CONTINUE
      RETURN
C
C       not enough space to extend JDHCSE
C
   20 CONTINUE
      CALL WDROP (IW,JDHCSE)
      CALL ALTELL ('HCSTSE: not enough space to enlarge JDHCSE ',1,
     &'NEXT')
      RETURN
      END
