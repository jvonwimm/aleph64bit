      INTEGER FUNCTION AGMATE (NAME)
C -------------------------------------------------------------
C - F.Ranjard - 910121
C! returns the GEANT material # known by its name
CKEY GEOM MATERIAL
C - Input :   - NAME   / CHA16     = material name
C                                    if ' ' get all names in ALLNAM
C - Output :  - AGMATE / I         = Geant tracking medium #
C                                   = IAGMAT if NAME is ' '
C                                   = 0  if NAME does not exist
C                                   = -IAGMAT if ALLNAM too short
C ---------------------------------------------------------------
      SAVE
      CHARACTER*(*) NAME
      PARAMETER (NTMAT=200, LCHNAM=16, LALL=NTMAT*LCHNAM)
      CHARACTER*3200 ALLNAM
      CHARACTER*20 NAMG
      CHARACTER*80 MSG
      INTEGER INAME(5)
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
C ---------------------------------------------------------
      IF (NAME.EQ.' ') THEN
C - get all existing materials , store them into ALLNAM
C
        IF (IAGMAT*LCHNAM.GT.LALL) THEN
          ITMAT  = -IAGMAT
          WRITE (LOUTIO,*) ' AGMATE NTMAT IAGMAT=',NTMAT,IAGMAT
          CALL ALTELL ('AGMATE: ALLNAM is too short',0,'STOP')
        ELSE
          ALLNAM = ' '
          DO 200 I=1,IAGMAT
            IF (GVERSN.LT.3.15) THEN
               CALL GFMATE (I,INAME,AD,ZD,DD,RD,AL,DUM,ND)
               CALL ALSTHO (INAME,5,NAMG)
            ELSE
               CALL GFMATE (I,NAMG,AD,ZD,DD,RD,AL,DUM,ND)
            ENDIF
            L = LENOCC (NAMG)
            IND = INDEX (NAMG,'$')
            IF (IND.NE.0) L=IND-1
            L = MIN (L,LCHNAM)
            IC = (I-1)*LCHNAM
            ALLNAM(IC+1:IC+L)  = NAMG(1:L)
  200     CONTINUE
          ITMAT = IAGMAT
        ENDIF
      ELSE
C
C - get the number from the name
        IND = INDEX(ALLNAM,NAME)
        IF (IND.EQ.0) THEN
C         this material does not exist
          ITMAT = 0
          L = LENOCC(NAME)
          MSG = ' AGMATE '//NAME(1:L)//' does not exist '
          L = LENOCC(MSG)
          CALL ALTELL (MSG(1:L),0,'STOP')
        ELSE
C        this material exists
          ITMAT  = IND/LCHNAM + 1
        ENDIF
      ENDIF
      AGMATE = ITMAT
      END
