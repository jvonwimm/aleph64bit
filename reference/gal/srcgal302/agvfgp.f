      SUBROUTINE AGVFGP
C------------------------------------------------------
C!  Defines and positions VDET 92 faces in GEANT
CKEY VDET GEOM FACE
C - W.Manner - 921103
C - Modified : A. Bonissent 940215 : use geom. package
C.  -Called by AGVDET                 from this .HLB
C.  -Calls GSROTM,GSPOS               from  GEANT3
C.
C. - positions volumes VDBX
C.
C------------------------------------------------
      DIMENSION XYZ(3),DIST(3)
      INTEGER VNTFAC,VXYZFC,VPHIFC
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C     Find position of the center of box VDBX with respect to
C     center of silicon active region in the face
C
      CALL VDGDIS('VDWA','VDBX',DIST)
C
C Compute the position of box VDB2
C
         CALL VDB2DD(D2)
C
C Dist(1) is the distance.
C
      DDD = DIST(1)
C
C-----Loop over faces/slots
C
      NFAC = VNTFAC()
      DO 20 IFAC = 1,NFAC
C
C-----Define the rotation angle of each side, possibly with inversion
C
         IBID = VPHIFC(IFAC,PHI)
         ROTAN = PHI*RADEG
         IF(ROTAN.GT.D360)   ROTAN = ROTAN-D360
         ROTA2 = ROTAN+D90
         IF(ROTA2.GT.D360)   ROTA2 = ROTA2-D360
         IAGROT= IAGROT+1
         CALL GSROTM(IAGROT,D90,ROTAN,D90,ROTA2,0.,0.)
C
C-----Find the position of the centre of each face
C
         IBID = VXYZFC(IFAC,XYZ)
C
C Compute displacements for the center of the box
C
         DX = DDD*COS(PHI)
         DY = DDD*SIN(PHI)
C   DZ=0 !
         XX     = XYZ(1)-DX
         YY     = XYZ(2)-DY
         ZZ     = XYZ(3)
         DX2 = D2*COS(PHI)
         DY2 = D2*SIN(PHI)
         XX2 = XX+DX2
         YY2 = YY+DY2
         ZZ2 = ZZ
C
C?----Put in place the faces
C
         NSLOT = IFAC
         CALL GSPOS('VDBX'  ,NSLOT,'VDET',XX,YY,ZZ,IAGROT,'ONLY')
         CALL GSPOS('VDB2'  ,NSLOT,'VDET',XX2,YY2,ZZ2,IAGROT,'ONLY')
   20 CONTINUE
   30 CONTINUE
C
      RETURN
      END
