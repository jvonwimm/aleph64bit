      SUBROUTINE ASRKIN
C ----------------------------------------------------------
C - F.Ranjard - 851023
C - Transfer contents of 'VERT' banks to Geant3 'VERT' banks
C   and do the same for 'KINE' banks
C - Drop 'VERT' and 'KINE' BOS banks
C   release space.
C - Called from    ASRETP                    from this .HLB
C - Calls          MZBOOK                    from ZEBRA.lib
C                  BDROP                     from BOS77.hlb
C ----------------------------------------------
      SAVE
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
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C
      COMMON/GCNUM/NGMATE,NGVOLU,NGROTM,NGTMED,NGTMUL,NGTRAC,NGPART
     +     ,NGSTMA,NGVERT,NGHEAD,NGBIT
      COMMON/GCNUMX/ NGALIV,NGTMST
C
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
      DATA IFI /0/
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
C - index of the next vertex/track to be stored in KINE/VERT
C   bank known by its index JVK
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)
C - # of vertices/tracks which could be stored in KINE/VERT
C   bank known by its index JVK
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))
C - index of the 1st parameter of KINE/VERT bank known by its
C   index JVK
      KPARVK(JVK) = JVK + IW(JVK+1)
C - index of 1st vertex/track # contained into the list of
C   bank KINE/VERT known by its index JVK
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)
C - charge of ALEPH particle# JPA
      CHARGE(JPA) = RTABL(IW(NAPART),JPA,7)
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPART),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPART),JPA,8)
C - # of vertices on a track known by its BOS index /
C   # of outgoing tracks of a vertex known by its BOS index
      NOFVK(JVK)  = IW(JVK+3)
C - Particle type of a track known by its BOS index
      KINTYP(JVK) = IW(KPARVK(JVK)+5)
C - incoming track # of a vertex known by its BOS index
      INPTRK(JVK) = IW(KPARVK(JVK)+5)
C - origin vertex # of a track known by its BOS index
      INPVRT(JVK) = IW(KLISVK(JVK)+1)
C - momentum of a track known by its BOS index
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2
     &                   +RW(KPARVK(JVK)+3)**2)
C - mass of a track known by its BOS index
      PMASVK(JVK) = RW(KPARVK(JVK)+4)
C - energy of a track known by its BOS index
      ENERVK(JVK) = SQRT (PMODVK(JVK)**2 + PMASVK(JVK)**2)
C - time of flight of the icoming particle to the vertex known by its
C   BOS index
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)
C - radius of the vertex known by its BOS index
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)
C - mother track # of a track known by its BOS index
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))
C - alephlib version# used in KINGAL (NKJ is the name-index of KJOB)
      ALEKIN(NKJ) = ITABL(IW(NKJ),1,JKJOAV)
C
C --------------------------------------------------------------
C
C - Get the ALEPHLIB version # used in KINGAL or GALEPH
      IF (IFI .EQ. 0) THEN
         IFI = 1
         CALL KIAVER (AVER,IPROG)
      ENDIF
C - Get PART bank index
      JPART = IW(NAPART)
C
C - Transfer 'VERT' banks
C
      CALL MZBOOK (IGXDIV,JGVERT,JGVERT,1,'VERT',NIVXKI,NIVXKI,1,2,0)
      IGB(JGVERT-5) = 0
      KV = NLINK ('VERT',1)
 6    CONTINUE
      IF (KV.NE.0) THEN
         NVX  = IW(KV-2)
         IF (NVX .GT. NIVXKI) GOTO 69
         KV1 = KPARVK(KV)
         KV2 = KLISVK(KV)
         CALL MZBOOK (IGXDIV,JV,JGVERT,-NVX,'VERT',0,0,IW(KV+3)+7,3,0)
         GB(JV+1) = RW(KV1+1)
         GB(JV+2) = RW(KV1+2)
         GB(JV+3) = RW(KV1+3)
         GB(JV+4) = RW(KV1+4)
         GB(JV+5) = IW(KV1+5)
         GB(JV+6) = 0.
         GB(JV+7) = NOFVK(KV)
         DO 61 N=1,NOFVK(KV)
            GB(JV+7+N) = IW(KV2+N)
 61      CONTINUE
         NGVERT = MAX (NVX,NGVERT)
         IGB(JGVERT+1) = NGVERT
         KV = IW(KV-1)
         GOTO 6
      ENDIF
 69   CONTINUE
C
C - Transfer 'KINE' banks
C
      CALL MZBOOK (IGXDIV,JGKINE,JGKINE,1,'KINE',NITRKI,NITRKI,1,2,0)
      IGB(JGKINE-5) = 0
      KK = NLINK ('KINE',1)
7     CONTINUE
      IF (KK.NE.0) THEN
         NKI  = IW(KK-2)
         IF (NKI .GT. NITRKI) GOTO 79
         KK1  = KPARVK(KK)
         KK2  = KLISVK(KK)
         CALL MZBOOK (IGXDIV,JK,JGKINE,-NKI,'KINE',0,0,IW(KK+3)+6,3,0)
         GB(JK+1) = RW(KK1+1)
         GB(JK+2) = RW(KK1+2)
         GB(JK+3) = RW(KK1+3)
C        In the ALEPHLIB 9.0 the KINE and FKIN banks have been modified
C        word(4) has been changed to contain mass instead of energy.
C        to be backward compatible the following test is necessary.
C        when files created with an ALEPHLIB earlier than 9.0 will have
C        disappeared only the statment GB(JK+4)=ENERVK(KK) will be neces
         IF (AVER .LT. 9.0) THEN
            GB(JK+4) = RW(KK1+4)
         ELSE
            GB(JK+4) = ENERVK(KK)
         ENDIF
         GB(JK+5) = ITABL (JPART,IW(KK2),1)
         GB(JK+6) = IW(KK2+1)
         GB(JK+7) = NOFVK(KK)-1
         DO 71 N=2,NOFVK(KK)
            GB(JK+6+N) = IW(KK2+N)
 71      CONTINUE
         NGTRAC = MAX (NKI,NGTRAC)
         IGB(JGKINE+1) = NGTRAC
         KK = IW(KK-1)
         GOTO 7
      ENDIF
 79   CONTINUE
C
C ----------- end -------------------------------------
 999  CONTINUE
      RETURN
      END
