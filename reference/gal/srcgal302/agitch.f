      SUBROUTINE AGITCH
C---------------------------------------------
C!   Build ITC chamber and related cables and supports geometry
C    Modified : B.Bloch-Devaux May 1993 rewritten to get ITC GEANT
C               Geometry from Data BAse banks
C.  -Called by AGEOME                        from this .HLB
C.  -Calls AGGEAN,AGDIMP,ALTELL              from this .HLB
C. -Builds geometry levels below 'CDET' level for ITC  part
C. -Initialises some search optimisation
C------------------------------------------------
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
C ----------------------------------------------------------------------
C - get setup code and build ITC  from data base banks
C
      CALL AGGEAN ('IT',ITCST)
C
C   Define where to find slot number
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('ITC ')
      IAGSEN(IAGSLV,2)=4
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
      CALL AGDIMP('ITC ',4,'ITC ')
      GOTO 999
C
C - not enough space to save sensitive module
C
 998  CONTINUE
      WRITE (LOUTIO,*) ' AGITCH : too many sensitive volumes, IAGSLV= '
     &   ,  IAGSLV, ' LSENV= ',LSENV
 991  CALL ALTELL ('AGITCH: cannot build ITC  geometry ',0,'STOP')
C
 999  CONTINUE
      RETURN
      END
