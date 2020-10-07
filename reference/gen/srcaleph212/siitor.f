      SUBROUTINE SIITOR(IMD,IST,IPH,IRD,ZZ,PHI,RAD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE / USER
C     B.BLOCH       May    1992
C! Transform subcomponents indices into Z,phi,rho coordinates
C   Input :
C          IMD   Module number ( 1-2)
C          IST   Z stack number ( 1-12)
C          IPH   Phi bin number ( 1-32)
C          IRD   Radial bin number ( 1-16)
C   Output:
C          ZZ,PHI,RAD    corresponding z, phi ( degrees), rho
C          RAD = -1.     means error
C   Called by USER program
C.---------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
      COMMON/SINALI/NASINT,NASIPO
      PARAMETER(JSINMN=1,JSINTP=3,JSINXR=5,LSINTA=20)
      PARAMETER ( RDEF = 16. ,YTDEF = 24.5 )
C!    set of intrinsic functions to handle BOS banks
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
      RAD =-1.
      IOR = MOD(IST-1,3)
C! transform to ARS system from local system indices
C                  x y z     <-----------   IMD IST IPH IRD
      IROW = ISINUM(IST,IMD)
      IXTAL = (IPH-1)/2 +1
      JSINT = IW(NASINT)
      IF ( JSINT.LE.0 ) GO TO 998
      ISIDE = 1
      IF ( IPH.GE.9 .AND. IPH.LE.24 ) ISIDE = -1
C GET RADIUS in local coordinates
      DR = RTABL(JSINT,IROW,JSINXR+IXTAL-1) - RDEF
      DXT= RTABL(JSINT,IROW,JSINTP)
      DYT= RTABL(JSINT,IROW,JSINTP+1) - YTDEF
      RAD = RMINSI(IMD)+ RADSTP*(IRD-1)+0.5*RADSTP + DR
C GET PHI in local coordinates
      ITP = IOR +1
      PHI = PHISTP*(IPH-1) + PHSHFT(ITP,IMD)+0.5*PHISTP
C GET Z   in local coordinates
      ZED = Z0SNSI(IMD)+ ZWIDSI*(IST-1)
C transform into ARS cartesian system
      X = RAD * COS(PHI) - DXT + DPOSSI(1,IMD) + ISIDE * GAPXSI(IMD)
      Y = RAD * SIN(PHI) - DYT + DPOSSI(2,IMD)
      Z = ZED  + DPOSSI(3,IMD)
      IF (IMD.EQ.2) Z = -Z
C transform to desired coordinates
      RAD = SQRT(X*X + Y* Y )
      PHI = ATG (Y,X)
      PHI = PHI*RADEG
      ZZ = Z
 998  RETURN
      END
