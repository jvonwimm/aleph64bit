      SUBROUTINE HCHLWD
C --------------------------------------------
C
C!    Fill HLWD bank to remember the hit position along the tube
C!
C!       Author      : F.Ranjard - 911121
C!
C!       Input bank  :       HWHT and HCSE
C!       Output bank :       HLWD
C!
C!      -Called by : HCASIG
C!      -Calls     : MVBITS  from CERNLIB
C-------------------------------------------------------
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
      PARAMETER(JHWHTA=1,LHWHTA=1)
      PARAMETER(JHLWWA=1,JHLWHP=2,LHLWDA=2)
      PARAMETER (LPHCTR= 12,LPHCTF=24,LHCRT=62)
      COMMON /HCTRIG/ NHCEPR,NHCBPR,NHCETR,NHCBTR , IHCTRG(LHCRT),NHCBTS
     +(LHCRT),NHCETS(LHCRT) , IYHCFI(LPHCTF),IXHCFI(LPHCTR),IXHCSE
     +(LPHCTR) , MHCETR,MHCBTR

      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
      PARAMETER (LHIT=3)
      DIMENSION ITUPR(LHIT),LCLUP(LHIT),ILAPR(LHIT),MODPR(LHIT),
     +          YHIT(LHIT)
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
C ----------------------------------------------
C
C - fill the word(2) with the hit position along the tube (Y-coord)
C
      IF (JDHCHI.EQ.0) RETURN
      JHLWD = IW(NAHLWD)
      IF (JHLWD.EQ.0) RETURN
      NHCHI = LROWS(JDHCHI)
      DO 16 I=1,NHCHI
         IHCSE = ITABL(JDHCHI,I,1)
         KHLWD = KROW(JHLWD,I)
         IW(KHLWD+JHLWWA) = ITABL(JDHCSE,IHCSE,JHCSTA)
         RW(KHLWD+JHLWHP) = RTABL(JDHCSE,IHCSE,JHCSTY)
 16   CONTINUE
      IW(JHLWD+LMHROW) = LROWS(JDHCHI)
C
      NHLWD = LROWS (JHLWD)
      KHLWD = JHLWD + LMHLEN
C
C          Loop on tubes
C
      LCLU = 1
C
C - loop over hits
C
      DO 10 I = 1, NHLWD
         N = IW(KHLWD+JHLWWA)
         ITU = MOD(N,1000)
         N = N/1000
         ILA = MOD(N,100)
         N = N/100
         IMO = MOD(N,100)
         N = N/100
         IPO = N
C
C          new tube
            ILN = ILA - 1
            CALL MVBITS(ILN,0,8,IW(KHLWD+1),16)
            CALL MVBITS(LCLU,0,8,IW(KHLWD+1),8)
            IMN = IMO
C
            IF(IPO.EQ.LPBAR) THEN
C          Barrel
               IMN = (IMO-1)/ 2+ 1
               IMN = IMN + LPHCTR
               IF (MOD(IMO,2) .EQ. 0) THEN
                  ITN = NHCTU1(ILA)*2 - ITU
               ELSE
                  ITN = ITU - 1
               ENDIF
            ELSE
C          End-cap
               IMOD = IMO
               IF(IPO.EQ.LPECB) IMOD = IMO + LPHCTR/2
               IF(ITU.GT.NHCTRE) THEN
                  IMN = IXHCSE(IMOD)
                  ITN = ITU - NHCTRE - 1
               ELSE
                  IMN = IXHCFI(IMOD)
                  ITN = ITU - 1
               ENDIF
C
            ENDIF
C
C -      fill tube and module parts of the digit
            CALL MVBITS(ITN,0,8,IW(KHLWD+1),0)
            CALL MVBITS(IMN,0,8,IW(KHLWD+1),24)
C
         KHLWD = KHLWD + LHLWDA
   10 CONTINUE
C
      IF(FHCDEB)THEN
         WRITE(LOUTIO,500) LROWS(JHLWD)
         KHLWD = JHLWD + LMHLEN
         DO 30 I=1,LROWS(JHLWD),LHIT
            JMAX = MIN (LHIT,NHLWD-I+1)
            DO 20 J=1,JMAX
               N       = IW(KHLWD+1)
               ITUPR(J)= IBITS (N,0,8)
               ILAPR(J)= IBITS (N,16,8)
               MODPR(J)= IBITS (N,24,8)
               YHIT (J)= RW(KHLWD+2)
               IHCSE   = ITABL(JDHCHI,I+J-1,1)
               LCLUP(J)= ITABL(JDHCSE,IHCSE,JHCSTA)
   20       KHLWD = KHLWD + LHLWDA
            WRITE (LOUTIO,510) I,I+2,(LCLUP(J),MODPR(J),ILAPR(J),
     +      ITUPR(J),YHIT(J),J=1,JMAX)
   30    CONTINUE
      ENDIF
      RETURN
  500 FORMAT (/1X,'+++HCHLWD+++ HLWD hit position along the tube ',
     +         I5/ 8X,3(4X,'  Address   Mo  La  Tu  position '))
  510 FORMAT (1X,I3,'-',I3,3(4X,I10,3I4,F10.2,1X))
      END
