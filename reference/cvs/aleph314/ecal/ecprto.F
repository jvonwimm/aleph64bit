      SUBROUTINE ECPRTO
C------------------------------------------------------------------
C     O.CALLOT    27-JAN-86
C
C! Print banks ETHT and ETDI
C
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      PARAMETER (NLIPR=16,NPARL=20)
      DIMENSION PRVAL(NPARL,NLIPR),IPVAL(NPARL,NLIPR),IFLAG(NLIPR)
      EQUIVALENCE (PRVAL(1,1),IPVAL(1,1))
      DIMENSION ISUMH(3),ISUMD(3)
      CHARACTER*32 FORM(NLIPR)
C
#include "bmacro.h"
      DATA  FORM /
     +'(//'' Theta  = '',20I6)'  ,
     +'('' Phi    = '',20I6)'    ,
     +'('' Sign 1 = '',20I6)'    ,
     +'(''      2 = '',20I6)'    ,
     +'(''      3 = '',20I6)'    ,
     +'('' Sum all= '',20I6)'    ,
     +'('' Digi 1 = '',20I6)'    ,
     +'(''      2 = '',20I6)'    ,
     +'(''      3 = '',20I6)'    ,
     +'('' Sum all= '',20I6)'    ,
     +'('' Noise1 = '',20F6.3)'  ,
     +'(''      2 = '',20F6.1)'    ,
     +'(''      3 = '',20F6.1)'    ,
     +'('' Gain 1 = '',20F6.3)'  ,
     +'(''      2 = '',20F6.3)'    ,
     +'(''      3 = '',20F6.3)'    /
C ---------------------------------------------------------
      LOUT = IW(6)
      MODE = 0
      KETHT = NLINK ('ETHT',0)
      IF( KETHT .NE. 0 ) MODE = MODE + 1
      KETDI = NLINK ('ETDI',0)
      IF( KETDI .NE. 0 ) MODE = MODE + 2
      IF(MODE.EQ.0) GO TO 990

      DO 1 I=1,NLIPR
 1    IFLAG(I) = 0
      IF(MODE.NE.2) THEN
        CALL VFILL(IFLAG,6,1)
        IETHT = KETHT + LMHLEN
        NHT = LROWS( KETHT )
        LHT = LCOLS( KETHT )
        IF(NHT.EQ.0)     MODE = MODE - 1
        KHT = IETHT + 1
      ENDIF
      IF(MODE.NE.1) THEN
        CALL VFILL(IFLAG,2,1)
        IETDI = KETDI + LMHLEN
        NDI = LROWS( KETDI )
        LDI = LCOLS( KETDI )
        IF(NDI.EQ.0)     MODE = MODE - 2
        KDI = IETDI + 1
        CALL VFILL(IFLAG(7),LDI,1)
        JENDI = NLINK ('ENDI',0)
        KENDI = 0
        IF( JENDI .EQ. 0 ) GOTO 3
        NENDI = LROWS( JENDI )
        LENDI = LCOLS( JENDI )
        KENDI = JENDI + LMHLEN + 1
        CALL VFILL(IFLAG(11),6,2)
    3   CONTINUE
      ENDIF
C
C--- printing loop. prepare buffer to be printed...
C
      WRITE(LOUT,1000) NHT,NDI
      DO 4 I=1,3
         ISUMH(I) = 0
 4    ISUMD(I) = 0
      IF(NDI.EQ.0.AND.NHT.EQ.0) GO TO 900
100   DO 105 IJ = 1,NLIPR
      DO 105 JI = 1,NPARL
105   IPVAL(JI,IJ)=0
      MAXPR = 0
      DO 110 I=1,NPARL
        LMOD = MODE
        IF(MODE.EQ.3) THEN
          IET1 = IW(KHT)
          IET2 = IW(KDI)
          IF(IET1.GT.IET2) LMOD = 2
          IF(IET1.LT.IET2) LMOD = 1
          IETIQ = IET1
        ENDIF
        IF(LMOD.EQ.1) IETIQ = IW(KHT)
        IF(LMOD.EQ.2) IETIQ = IW(KDI)
        IPVAL(I,1) = IBITS(IETIQ,16,8)
        IPVAL(I,2) = IBITS(IETIQ,2,9)
        IF(LMOD.NE.2) THEN
          ISUM = 0.
          DO 120 K=1,3
            IPVAL(I,2+K) = NINT( .001 * IW(KHT+K) )
            ISUM = ISUM + IW(KHT+K)
            ISUMH(K) = ISUMH(K) + IW(KHT+K)
  120     CONTINUE
          IPVAL(I,6) = NINT( .001 * ISUM )
          KHT = KHT + LHT
          NHT = NHT - 1
          IF(NHT.EQ.0) MODE = MODE - 1
        ENDIF
        IF(LMOD.NE.1) THEN
          ISUM = 0.
          DO 130 K=1,3
            IPVAL(I,6+K) = NINT( .001 * IW(KDI+K) )
            ISUM = ISUM + IW(KDI+K)
            ISUMD(K) = ISUMD(K) + IW(KDI+K)
            IF(KENDI.NE.0) THEN
              PRVAL(I,10+K) = .001 * IW(KENDI+K)
              PRVAL(I,13+K) = .000001 * IW(KENDI+3+K)
            ENDIF
  130     CONTINUE
          IPVAL(I,10) = NINT( .001 * ISUM )
          KDI = KDI + LDI
          IF(KENDI.NE.0) KENDI = KENDI + LENDI
          NDI = NDI - 1
          IF(NDI.EQ.0) MODE = MODE - 2
        ENDIF
        MAXPR = MAXPR + 1
        IF(MODE.EQ.0) GO TO 200
  110 CONTINUE
C
C-- now, print the datas using dedicated format
C
  200 CONTINUE
      DO 210 I=1,NLIPR
        IF(IFLAG(I).EQ.2) WRITE(LOUT,FORM(I)) (PRVAL(J,I),J=1,MAXPR)
        IF(IFLAG(I).EQ.1) WRITE(LOUT,FORM(I)) (IPVAL(J,I),J=1,MAXPR)
  210 CONTINUE
      IF(MODE.NE.0) GO TO 100
  900 CONTINUE
      DO 910 K=1,3
        ISUMH(K) = NINT( .001 * ISUMH(K) )
        ISUMD(K) = NINT( .001 * ISUMD(K) )
  910 CONTINUE
      JSUMH = ISUMH(1) + ISUMH(2) + ISUMH(3)
      JSUMD = ISUMD(1) + ISUMD(2) + ISUMD(3)
      WRITE(LOUT,1100) JSUMH,ISUMH,JSUMD,ISUMD
  990 RETURN
 1000 FORMAT(/' +++ECPRTO+++ ETHT, ETDI, ENDI tower data in MEV :',
     + ' tower hits (',I4,' ) and digits (',I4,' )'/)
 1100 FORMAT(//' Grand total for hits   = ',I10,' ,per stack = ',
     +   3I10/ '                 digits = ',I10,'              ',
     +   3I10/)
      END
#endif
