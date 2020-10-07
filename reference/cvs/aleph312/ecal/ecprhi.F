      SUBROUTINE ECPRHI
C-------------------------------------------------------------------
C      O.CALLOT    28-JAN-86
C
C! Print history banks ETTD,EWTD
C
C-------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      PARAMETER (NPARL=20,NBLIG=6)
      DIMENSION IPVAL(NPARL,NBLIG),ISUMH(3)
      CHARACTER*30 FORM(NBLIG)
      CHARACTER*10 TITSD(3),TNAME
C
#include "bmacro.h"
      DATA TITSD / 'End Cap A ' , '   Barrel ' , 'End Cap B ' /
      DATA FORM /
     +'('' Theta  = '',20I6)',
     +'('' Phi    = '',20I6)',
     +'('' Sign 1 = '',20I6)',
     +'(''      2 = '',20I6)',
     +'(''      3 = '',20I6)',
     +'('' Sum all= '',20I6)' /
C ----------------------------------------------------------------
      LOUT = IW(6)
C
C  *** First, the tower history
C
      KETTD = NLINK('ETTD',1)
      IF( KETTD .EQ.0 ) GOTO 500
      NBTR = LROWS( KETTD )
      IF(NBTR.EQ.0) GO TO 500
      WRITE(LOUT,1000) NBTR
C
C *** if ETTD, nr=1 exist, then suppose nexts are nr=2,3,4
C
      KETT2 = IW ( KETTD - 1 )
      KETT3 = IW ( KETT2 - 1 )
      KETT4 = IW ( KETT3 - 1 )
      IND1  = KETTD + LMHLEN
      IND2  = KETT2 + LMHLEN
      IND3  = KETT3 + LMHLEN
      KET4  = KETT4 + LMHLEN
      LKET4 = LCOLS (KETT4)
      DO 100 IT = 1,NBTR
        NUMTR = IW(IND1+IT)
        LONTR = IW(IND2+IT)
        LADTR = IW(IND3+IT)
        ISUMH(1) = 0
        ISUMH(2) = 0
        ISUMH(3) = 0
        DO 101 J=1,LONTR
          IOFS = KET4 + LADTR + (J-1) * LKET4
          ISUMH(1) = ISUMH(1) +  IW(IOFS+1)
          ISUMH(2) = ISUMH(2) +  IW(IOFS+2)
          ISUMH(3) = ISUMH(3) +  IW(IOFS+3)
  101   CONTINUE
        DO 102 K=1,3
  102   ISUMH(K) = NINT( .001 * ISUMH(K) )
        ISUMT = ISUMH(1) + ISUMH(2) + ISUMH(3)
        WRITE(LOUT,1100) IT,NUMTR,LONTR,ISUMT,ISUMH
        IF(LONTR.EQ.0) GO TO 100
  105   MAXPR = 0
        DO 110 J=1,NPARL
          KAD = KET4 + LADTR
          IPVAL(J,1) = IBITS(IW(KAD),16,8)
          IPVAL(J,2) = IBITS(IW(KAD),2,9)
          IPVAL(J,3) = NINT( .001 * IW(KAD+1) )
          IPVAL(J,4) = NINT( .001 * IW(KAD+2) )
          IPVAL(J,5) = NINT( .001 * IW(KAD+3) )
          IPVAL(J,6) = NINT( .001 * (IW(KAD+1)+IW(KAD+2)+IW(KAD+3)) )
          LADTR = LADTR + LKET4
          MAXPR = MAXPR + 1
          LONTR = LONTR - 1
          IF(LONTR.LE.0) GO TO 120
  110   CONTINUE
  120   DO 130 J=1,NBLIG
          WRITE(LOUT,FORM(J)) (IPVAL(K,J),K=1,MAXPR)
  130   CONTINUE
        IF(LONTR.GT.0) GO TO 105
  100 CONTINUE
  500 CONTINUE
C
C  ***  Now, the wire plane history
C
      KEWTD = NLINK ('EWTD',1)
      IF( KEWTD .EQ. 0 ) GOTO 900
      IF(NBTR.EQ.0) GO TO 900
      KEWT2 = IW( KEWTD - 1 )
      KEWT3 = IW( KEWT2 - 1 )
      KEWT4 = IW( KEWT3 - 1 )
      IND1  = KEWTD + LMHLEN
      IND2  = KEWT2 + LMHLEN
      IND3  = KEWT3 + LMHLEN
      KEW4  = KEWT4 + LMHLEN
      LEW4  = LCOLS (KEWT4)
      WRITE(LOUT,2000) NBTR
      DO 600 IT=1,NBTR
        NUMT = IW(IND1 + IT)
        LONT = IW(IND2 + IT)
        IADT = IW(IND3 + IT)
        ISUMW = 0.
        DO 610 J=1,LONT
          IPT = KEW4 + IADT + (J-1) * LEW4
          DO 611 K=1,LEW4-1
  611     ISUMW = ISUMW + IW(IPT+K)
  610   CONTINUE
        ISUMW = NINT( .001 * ISUMW )
        WRITE(LOUT,2010) IT,NUMT,LONT,ISUMW
        IF(LONT.EQ.0) GO TO 600
  615   MAXPR = 0
        DO 620 JMOD=1,LONT
          IPMOD = KEW4 + IADT
          IMOD  = IW(IPMOD)
          ISCO  = (IMOD-1)/12 + 1
          TNAME = TITSD(ISCO)
          IMO   = IMOD - 12 * (ISCO-1)
          DO 630 J=1,15
            IPVAL(J,1) = NINT( .001 * IW(IPMOD   +J))
            IPVAL(J,2) = NINT( .001 * IW(IPMOD+15+J))
            IPVAL(J,3) = NINT( .001 * IW(IPMOD+30+J))
  630     CONTINUE
          WRITE(LOUT,2020) TNAME,IMO,((IPVAL(I,J),I=1,15),J=1,3)
          IADT = IADT + LEW4
  620   CONTINUE
  600 CONTINUE
  900 CONTINUE
      RETURN
 1000 FORMAT(/1X,'+++ECPRHI+++ ETTD banks print out ',I4,' tracks.'/)
 1100 FORMAT(/' -- Track',I5,' GEANT number',I5,' with ',I4,
     +  ' towers. Total signal =',I7,' , per stack =',3I7)
 2000 FORMAT (/1X,'+++ECPRHI+++ EWTD banks print out ',I4,' tracks.'/)
 2010 FORMAT(/' -- Track',I5,' GEANT number',I5,' with ',I4,
     + ' modules hits.  Total signal =',I7)
 2020 FORMAT (1X,A10,I2,'  1- 15 ',15I6/13X,' 16- 30 ',15I6,
     +        /13X,' 31- 45 ',15I6)
      END
#endif
