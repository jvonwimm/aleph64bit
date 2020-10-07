      SUBROUTINE ASKSIN (IDEV,ISTA,NITR,NIVX,ECMS,WEIT)
C ----------------------------------------------------------------------
C. - R.BEUSELINCK - 830623   for GALEPH 3.2
C.                                       modified by F.RANJARD - 871029
C! Generate a single track particle of a given type in a given range
C.   of momentum and cosine(theta).
C - Output arguments:
C          IDEV     = particle type
C          ISTA     = event status word  ( 0 means OK )
C          NITR     = # of generated tracks
C          NIVX     = # of generated vertices
C          ECMS     = particle momentum
C          WEIT     = event weight
C.   BKINJO(4) = particle type
C.          5    minimum value of the momentum
C.          6    maximum value of the momentum
C.          7    minimum value of cos(theta)
C.          8    maximum value of cos(theta)
C. - called by    ASKINE when TKINJO = PART             from this .HLB
C. - calls        RANNOR , RNDM                         from KERNLIB
C.                KBVERT, KBKINE                        from this .HLB
C.
C -----------------------------------------------------
      SAVE
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
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
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
      EXTERNAL RNDM
      REAL VRTX(4),PLAB(4)
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
C - smear vertex position
C
      CALL RANNOR(RN1,RN2)
      CALL RANNOR(RN3,DUM)
      VRTX(1)=RN1*BKINJO(1)
      VRTX(2)=RN2*BKINJO(2)
      VRTX(3)=RN3*BKINJO(3)
      VRTX(4)=0.
      NIVX = 1
      JKVOL = IW(NAKVOL)
      IW(JKVOL+LMHROW) = NIVX
      KKVOL = KROW(JKVOL,NIVX)
      IW(KKVOL+1) = INTCHA ('BPVA')
C
C - store vertex coordinates
C
      JVERT = KBVERT (NIVX,VRTX,0)
      IF (JVERT .LE. 0) THEN
         CALL ALTELL ('ASKSIN: not enough space for VERT',1,'NEXT')
      ENDIF
C
C--  generate a single track of type BKINJO(4) with suitable
C--  momentum and direction.
C--
      PMOD = BKINJO(5) + (BKINJO(6)-BKINJO(5))*RNDM(0)
      PHI  = TWOPI*RNDM(0)
      COST = BKINJO(7) + (BKINJO(8)-BKINJO(7))*RNDM(0)
      IF (RNDM(0).GT.0.5) COST = -COST
      SINT = SQRT(1. - COST**2)
      PLAB(1)=PMOD*SINT*COS(PHI)
      PLAB(2)=PMOD*SINT*SIN(PHI)
      PLAB(3)=PMOD*COST
      PLAB(4) = 0.
C
      IDEV = NINT(BKINJO(4))
      NITR = 1
      JKINE = KBKINE (NITR,PLAB,IDEV,NIVX)
      IF ( JKINE .LE. 0) THEN
         CALL ALTELL ('ASKSIN: not enough space for VERT or KINE',1,
     &                'NEXT')
      ENDIF
      WEIT = 1.
      ISTA = 0
      ECMS = PMOD
C
C - END
C
999   RETURN
      END
