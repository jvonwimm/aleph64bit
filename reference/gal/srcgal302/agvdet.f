      SUBROUTINE AGVDET
C ----------------------------------------------------------
C - W.Manner - 901100
C - Modified : A. Bonissent 940215 : use geom. package
C! build VDET geometry
CKEY VDET GEOM
C - called by AGEOME
C ---------------------------------------------------------
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
C
C --------------------------------------------------------
C - get setup code and build VDET from data base banks
C
      CALL AGGEAN ('VD',IVDST)
C
C - position faces depending on the setup code
C
      IF (IVDST.LE.3) THEN
         CALL ALTELL ('AGVDET: VDET 89-91 is not implemented ',0,'STOP')
      ELSEIF (IVDST.LE.6) THEN
         CALL AGVF92
         CALL GSORD('VDET',3)
         CALL GSORD('VDBX',1)
         IF(IAGSLV+2.GT.LSENV) GO TO 992
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('VDWA')
         IAGSEN(IAGSLV,2)=    4
      ELSE
         CALL AGVF95
         IF(IAGSLV+2.GT.LSENV) GO TO 992
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('VSWA')
         IAGSEN(IAGSLV,2)=    4
      ENDIF
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
      CALL AGDIMP('VDET',3,'VDET')
      RETURN
C
C - error ---------------------------------------------------------
C
C   not enough space to SAVE sensitive module
  992 CONTINUE
      WRITE (LOUTIO,*) ' AGVDET : too many sensitive volumes, IAGSLV= '
     &   ,  IAGSLV, ' LSENV= ',LSENV
C
      CALL ALTELL ('AGVDET: cannot build VDET geometry ',0,'STOP')
C - end
C
      END
