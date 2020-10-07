      SUBROUTINE AGDIMP(VOLNA,LVLVO,DETNA)
C--------------------------------------------------------------------
C      O.Callot     11-jun-86
C
C! Add the volume named 'VOLNA' at level LVLVO to the list used
C! to generate the IMPA bank, with detector name DETNA.
C
C.  Calls  INTCHA                                      BOS lib
C.  Called by various AGxxxx routines
C---------------------------------------------------------------------
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
      CHARACTER*4 VOLNA,DETNA
      EXTERNAL INTCHA , CHAINT
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
      CHARACTER*4 TDETN, CHAINT
      IDETN (TDETN) = IUCOMP (INTCHA(TDETN), IW(IW(NAIMPA)+1),
     &                          IW(IW(NAIMPA)))
C --------------------------------------------------------------------
C - if NO IMPA data card RETURN
      IF (IW(NAIMPA) .EQ. 0) RETURN
C
C - if IMPA is NOT required for this detector then RETURN
      IF (IDETN(DETNA) .EQ. 0) RETURN
C
C - store the volume name into the list
C
      IF(NAGIMP.EQ.LIMVOL) GO TO 980
      NAGIMP = NAGIMP + 1
      LAGIMP(1,NAGIMP) = INTCHA(VOLNA)
      LAGIMP(2,NAGIMP) = LVLVO
      LAGIMP(3,NAGIMP) = INTCHA(DETNA)
      RETURN
C
  980 WRITE(LOUTIO,1000) VOLNA,DETNA,NAGIMP,(CHAINT(LAGIMP(1,J)),
     &                   CHAINT(LAGIMP(2,J)),CHAINT(LAGIMP(3,J)),
     &                   J=1,NAGIMP)
 1000 FORMAT(/' +++ AGDIMP +++ called for volume ',A4,' and detector ',
     +       A4,'  max.number',I6,' exceded. List follows.'/
     +       15(10X,A4,I6,2X,A4/))
      RETURN
      END
