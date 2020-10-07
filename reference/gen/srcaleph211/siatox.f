      SUBROUTINE SIATOX(IAD,IOR,POS,IOK)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address Decoding routine  into X,Y,Z position
C   Input :
C          IAD   ADDRESS encoded (16 bits)
C          IOR   Bin number in triplet (0-2)
C   Output:
C          POS(3) x,y,z of corresponding pad barycenter
C          IOK = -1 means error
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
      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
      COMMON/SINALI/NASINT,NASIPO
      PARAMETER(JSINMN=1,JSINTP=3,JSINXR=5,LSINTA=20)
      DIMENSION POS(3)
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
C  First decode address into elements
      IOK = 0
      CALL SIDCOD(IAD,IOR,IMD,IST,IPH,IRD)
      IF ( IMD.EQ.-1) GO TO 998
C local coordinates
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
C GET X,Y ,Z
      POS(1) = X
      POS(2) = Y
      POS(3) = Z
      RETURN
 998  IOK = -1
      RETURN
      END
