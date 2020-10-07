      SUBROUTINE AGVF92
C------------------------------------------------------
C!  Defines and positions VDET 92 faces in GEANT
CKEY VDET GEOM FACE
C - W.Manner - 921103
C - Modified : A. Bonissent 940215 : use geom. package
C - Modified : F.Ranjard 950419 : restore AGFV92
C.  -Called by AGVDET                 from this .HLB
C.  -Calls GSROTM,GSPOS               from  GEANT3
C.
C. - positions volumes VDBX and VDB2
C.
C------------------------------------------------
      REAL XYZ(3), XVDBX, XVDB2
      CHARACTER*4 VOLU, VDAU, CHAINT
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
      PARAMETER(JVPODA=1,JVPOMO=2,JVPOFL=3,JVPOVR=4,JVPOCN=5,JVPOPO=6,
     +          LVPOSA=8)
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
C -------------------------------------------------------------------
C
C     Find X-position of the center of box VDBX with respect to
C     X-center of silicon active region VDWA in the face
C
      KVPOS = IW(NAMIND('VPOS'))
      IF(KVPOS.EQ.0)THEN
          CALL ALTELL ('AGVF92 : VPOS bank missing',0,'STOP')
      ENDIF
      VOLU = 'VDWA'
      XVDBX = 0.
   13 CONTINUE
      IF(VOLU .NE.'VDBX')THEN
         DO  IVPOS = 1, LROWS(KVPOS)
            VDAU = CHAINT(ITABL(KVPOS,IVPOS,JVPODA))
            IF(VDAU.EQ.VOLU)THEN
              VOLU = CHAINT(ITABL(KVPOS,IVPOS,JVPOMO))
              XVDBX = XVDBX + RTABL(KVPOS,IVPOS,JVPOPO)
              GO TO 13
            ENDIF
         ENDDO
         CALL ALTELL ('AGVF92 : NO VDBX in VPOS bank',0,'STOP')
      ENDIF
C
C X-position of box VDB2
C
      XVDB2 = -0.311785
C
C-----Loop over faces
C
      NFAC = VNTFAC()
      DO IFAC = 1,NFAC
C
C-----Define the rotation angle of each side, possibly with inversion
C
         IDUM = VPHIFC(IFAC,PHI)
         ROTAN = PHI*RADEG
         IF(ROTAN.GT.D360)   ROTAN = ROTAN-D360
         ROTA2 = ROTAN+D90
         IF(ROTA2.GT.D360)   ROTA2 = ROTA2-D360
         IAGROT= IAGROT+1
         CALL GSROTM(IAGROT,D90,ROTAN,D90,ROTA2,0.,0.)
C
C-----Find the position of the centre of each face
C
         IDUM = VXYZFC(IFAC,XYZ)
C
C Compute displacements for the center of the box
C
         DX  = XVDBX * COS(PHI)
         DY  = XVDBX * SIN(PHI)
         XX  = XYZ(1)-DX
         YY  = XYZ(2)-DY
         ZZ  = XYZ(3)
         DX2 = XVDB2 * COS(PHI)
         DY2 = XVDB2 * SIN(PHI)
         XX2 = XX+DX2
         YY2 = YY+DY2
         ZZ2 = ZZ
C
C?----Put in place the faces
C
         CALL GSPOS('VDBX',IFAC,'VDET',XX,YY,ZZ,IAGROT,'ONLY')
         CALL GSPOS('VDB2',IFAC,'VDET',XX2,YY2,ZZ2,IAGROT,'ONLY')
      ENDDO
C
      RETURN
      END
