      SUBROUTINE AGSCAL
C ----------------------------------------------------------
C - B.Bloch-Devaux and J.P.Schuller 910930
C! build SCAL geometry
CKEY SCAL GEOM
C - called by AGEOME
C - calls     AGGEAN,AGSI92,AGDIMP              from this Lib
C - calls     ALTELL                            from ALEPHLib
C ---------------------------------------------------------
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
      EXTERNAL JHOCHA
C --------------------------------------------------------
C - get setup code and build SCAL from data base banks
C
      CALL AGGEAN ('SI',ISIST)
C
      IF (ISIST.LT.1) THEN
         CALL ALTELL ('AGSCAL: SCAL<92 is not implemented ',0,'STOP')
      ELSEIF (ISIST.GE.1) THEN
C
C     Define sensitive volume flag as defined if SET 'SCAL' is selected
C
         IF(IAGSLV+1.GT.LSENV) GO TO 992
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('SSIL')
C     store number (1-NMOD) of SMOD which is level 8
         IAGSEN(IAGSLV,2)=    8
         CALL AGSI92
      ENDIF
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
      CALL AGDIMP('SACT',7,'SCAL')
      GOTO 999
C - error
C
C   not enough space to SAVE sensitive module
  992 CONTINUE
      WRITE (LOUTIO,*) ' AGSCAL : too many sensitive volumes, IAGSLV= '
     &   ,  IAGSLV, ' LSENV= ',LSENV
      GOTO 991
C
 991  CALL ALTELL ('AGSCAL: cannot build SCAL geometry ',0,'STOP')
C - end
C
  999 CONTINUE
      RETURN
      END
