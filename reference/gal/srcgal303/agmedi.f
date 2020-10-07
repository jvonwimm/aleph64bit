      INTEGER FUNCTION AGMEDI (NAME)
C -------------------------------------------------------------
C - W.Manner  - 901203
C! returns the GEANT tracking medium # known by its name
CKEY GEOM
C - Input :   - NAME   / CHA16     = tracking medium name
C                                    if ' ' get all names in ALLNAM
C - Output :  - AGMEDI / I         = Geant tracking medium #
C                                   = IAGMED if NAME is ' '
C                                   = 0  if NAME does not exist
C                                   = -IAGMED if ALLNAM too short
C ---------------------------------------------------------------
      SAVE
      CHARACTER*(*) NAME
      PARAMETER (NTMED=200, LCHNAM=16, LALL=NTMED*LCHNAM)
      CHARACTER*3200 ALLNAM
      CHARACTER*16 NAMG
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
C - get all existing tracking media , store them into ALLNAM
C
         IF (IAGMED*LCHNAM.GT.LALL) THEN
            ITMED  = -IAGMED
            WRITE (LOUTIO,*) ' AGMEDI NTMED IAGMED=',NTMED,IAGMED
            CALL ALTELL ('AGMEDI: ALLNAM is too short',0,'STOP')
         ELSE
            ALLNAM = ' '
            DO 200 I=1,IAGMED
               IF (GVERSN .LT. 3.15) THEN
                 CALL GFTMED (I,INAME,ND,ID,JD,FD,TL,DD,ED,PD,IFD,DU,ND)
                 CALL ALSTHO (INAME,5,NAMG)
               ELSE
                 CALL GFTMED (I,NAMG,ND,ID,JD,FD,TL,DD,ED,PD,IFD,DU,ND)
               ENDIF
               L = LNBLNK (NAMG)
               IND = INDEX (NAMG,'$')
               IF (IND.NE.0) L=IND-1
               L = MIN (L,LCHNAM)
               IC = (I-1)*LCHNAM
               ALLNAM(IC+1:IC+L)  = NAMG(1:L)
 200        CONTINUE
            ITMED = IAGMED
         ENDIF
      ELSE
C
C - get the number from the name
         IND = INDEX(ALLNAM,NAME)
         IF (IND.EQ.0) THEN
C         this tracking medium does not exist
           ITMED = 0
           L = LENOCC(NAME)
           MSG = ' AGMEDI '//NAME(1:L)//' does not exist '
           L = LENOCC(MSG)
           CALL ALTELL (MSG(1:L),0,'STOP')
         ELSE
C        this tracking medium exists
           ITMED  = IND/LCHNAM + 1
         ENDIF
      ENDIF
      AGMEDI = ITMED
      END
