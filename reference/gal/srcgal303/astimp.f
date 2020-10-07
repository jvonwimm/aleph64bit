      SUBROUTINE ASTIMP
C---------------------------------------------------------------------
C      O.Callot    11-jun-86
C                                    modified by F.Ranjard - 861216
C! Fill 'IMPA' banks
C   Store in bank IMPA, NR=Geant-track-number, the track's parameters
C   at the detector entrance, as defined by AGDIMP
C
C.  Called by GUSTEP                                   this .HLB
C.  Calls     BLIST,NLINK                              BOS77
C.            ALBOS                                    this .HLB
C---------------------------------------------------------------------
      SAVE
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
       COMMON /WRKSPC/ WSPACE(88320)
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
C
      COMMON /GCVOLU/ NGLEVE,NGAMES(15),NGUMBR(15),
     &  LGVOLU(15),LGINDX(15),IGNFRO,NGLVMX,NGLDEV(15),LGINMX(15),
     &  GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      COMMON/GCTRAK/GVECT(7),GETOT,GEKIN,GVOUT(7),NGMEC,LGMEC(30)
     &,NGAMEC(30),NGSTEP,MGXNST,GDESTP,GDESTL,GSAFET,GSLENG,GSTEP,GSNEXT
     &,GSFIEL,GTOFG,GEKRAT,GUPWGH,IGNEXT,IGNWVO,IGSTOP,IGAUTO,IGEKBI
     &,IGLOSL,IGMULL,IGNGOT,NGLDOW,NGLVIN,NGLVSA,IGSTRY
C
      COMMON/GCKINE/IGKINE,GPKINE(10),IGTRA,IGSTAK,IGVERT,IGPART,IGTRTY
     +              ,NGAPAR(5),GMASS,GCHARG,GTLIFE,GVERT(3),GPVERT(4)
     +              ,IGPAOL
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
      PARAMETER (LIMPA=8, LBKIM=5)
      LOGICAL FIMPA(LIMVOL),FECAL
      DATA IECAL /4HECAL/
      FECAL(ICAL) = LAGIMP(3,ICAL) .EQ. IECAL
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
C ---------------------------------------------------------------
C
      DO 10 IM=1,NAGIMP
        IF(NGLEVE.LT.LAGIMP(2,IM)) GO TO 10
        IF(NGAMES(LAGIMP(2,IM)).EQ.LAGIMP(1,IM)) GO TO 20
   10 CONTINUE
      RETURN
C
   20 CONTINUE
      IF (FECAL(IM) .AND. GCHARG.NE.0.) RETURN
C
      KIMPA = NLINK('IMPA',IGTRA)
      IF(KIMPA.EQ.0) THEN
        LON = LMHLEN + LBKIM * LIMPA
         CALL ALBOS ('IMPA',IGTRA,LON,KIMPA,IGARB)
        IW(KIMPA+1) = LIMPA
        IW(KIMPA+2) = 0
        DO 30 M=1,LIMVOL
 30     FIMPA(M)=.FALSE.
      ELSE
        IF (LFRWRD(KIMPA) .LT. LIMPA) THEN
            CALL ALBOS ('IMPA',IGTRA,IW(KIMPA)+LIMPA,KIMPA,IGARB)
        ENDIF
      ENDIF
C
C - IF the impact is already stored for this detector RETURN
      IF (FIMPA(IM)) GOTO 100
      FIMPA(IM) = .TRUE.
C
      KPT = KNEXT (KIMPA)
      IW(KPT+1) = LAGIMP(3,IM)
      CALL UCOPY (GVECT(1),RW(KPT+2),6)
      RW(KPT+8) = GETOT
      IW(KIMPA+2) = IW(KIMPA+2) + 1
  100 CONTINUE
      RETURN
C
      END
