      SUBROUTINE SIXTOA(IAD,IOR,XX,YY,ZZ,IOK)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address encoding routine  from X,Y,Z coordinates
C   Input :
C          XX ,YY ,ZZ  coordinates of space point
C   Output:
C          IAD   ADDRESS encoded (16 bits) for SIDI
C          IOR   Bin number in triplet (0-2)
C          IOK non 0 means error    -1 wrong in R
C                                   -2 wrong in Z
C                                   -3 wrong in Phi
C                                   -4 wrong in address coding
C                                   -5 falls in gap between halves
C   Called by USER program
C.---------------------------------------------------------------------
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
      PARAMETER ( EPS  = 0.001 )
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
      IAD = -1
      IOK = -4
C GET module
      IMD = 1
      IF (ZZ.LT.0.) IMD = 2
      Z = ABS (ZZ)
      X = XX
      Y = YY
C! transform from ARS system to local system indices
C                  x y z      ----------->  IMD IST IPH IRD
      JSINT = IW(NASINT)
      IF ( IW(JSINT).LE.0 ) GO TO 999
C Get STACK   bin
      IOK = -2
      IST = INT((Z+EPS-Z0SNSI(IMD)-DPOSSI(3,IMD))/ZWIDSI)+1
      IF ( IST.LE.0 .OR. IST.GT.NZBNSI) GO TO 999
      IROW = ISINUM(IST,IMD)
      DXT= RTABL(JSINT,IROW,JSINTP)
      DYT= RTABL(JSINT,IROW,JSINTP+1) - YTDEF
      X = X+DXT-DPOSSI(1,IMD)
      Y = Y+DYT-DPOSSI(2,IMD)
C Check for gap between halves of Calo
      X0 = ABS(X)
      ISIDE = 1
      IF ( X.LT.0. ) ISIDE =-1
      X = X - ISIDE * GAPXSI(IMD)
      RAD = SQRT(X*X+Y*Y)
C Get PHI bin
      IOK = -3
      PHI = ATG(Y,X)
      ITP = MOD(IST-1,3)+1
      PHIT= PHI-PHSHFT(ITP,IMD)
      IF (PHIT.LT.0.) PHIT= PHIT+TWOPI
      IF (PHIT.GT.TWOPI) PHIT= PHIT-TWOPI
      IPH = INT(PHIT/PHISTP)+1
      IF ( IPH.LE.0 .OR. IPH.GT.NPBNSI) GO TO 999
C GET RADIUS  bin
      IOK = -1
      IXTAL = (IPH-1)/2 +1
      DR = RTABL(JSINT,IROW,JSINXR+IXTAL-1) - RDEF
      RADP =  RAD-RMINSI(IMD)-DR
      IF ( RADP.LT.0.) GO TO 999
      IRD = INT(RADP/RADSTP)+1
      IF ( IRD.LE.0 .OR. IRD.GT.NRBNSI) GO TO 999
C  Then  encode address from elements
      IOK = -4
      CALL SIENCD(IAD,IOR,IMD,IST,IPH,IRD)
      IF ( IAD.EQ.-1) GO TO 999
      IOK = -5
      IF (     X0.LT.GAPXSI(IMD)) GO TO 999
      IOK = 0
 999  RETURN
      END
