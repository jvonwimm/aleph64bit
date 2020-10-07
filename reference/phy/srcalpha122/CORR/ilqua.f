      INTEGER FUNCTION ILQUA(ICOD)
C ...............................................................
C . ICOD   / I   Alpha hadron code                              .
C .                                                             .
C . ILQUA  / I   1   if the hadron contains a b (not a b-bar)   .
C .             -1   if the hadron contains a b-bar             .
C .              2   if the hadron contains a c (not a c-bar)   .
C .             -2   if the hadron contains a c-bar             .
C ...............................................................
      IMPLICIT NONE
      INTEGER ICOD,IPAR
      LOGICAL BBARY,BQMES,BBMES,CQUA,CBAR
C
C - b baryons or anti-baryons
      BBARY =        (ICOD.GE.211 .AND. ICOD.LE.238)
     &          .OR. (ICOD.GE.259 .AND. ICOD.LE.270)
     &          .OR. (ICOD.GE.283 .AND. ICOD.LE.300)
     &          .OR. (ICOD.GE.351 .AND. ICOD.LE.360)
C
C - b mesons
      BQMES = (ICOD.eq.115)
     &   .OR. (ICOD.eq.118)
     &   .OR. (ICOD.eq.120)
     &   .OR. (ICOD.eq.121)
     &   .OR. (ICOD.eq.123)
     &   .OR. (ICOD.eq.126)
     &   .OR. (ICOD.eq.128)
     &   .OR. (ICOD.eq.129)
C
C - b-bar mesons
      BBMES = (ICOD.eq.116)
     &   .OR. (ICOD.eq.117)
     &   .OR. (ICOD.eq.119)
     &   .OR. (ICOD.eq.122)
     &   .OR. (ICOD.eq.124)
     &   .OR. (ICOD.eq.125)
     &   .OR. (ICOD.eq.127)
     &   .OR. (ICOD.eq.130)
C
      IPAR = ICOD - 2*(icod/2)
C
C - c mesons and baryons
      CQUA=          (ICOD.EQ.35)
     &          .OR. (ICOD.EQ.37)
     &          .OR. (ICOD.EQ.39)
     &          .OR. (ICOD.EQ.41)
     &          .OR. (ICOD.EQ.82)
     &          .OR. (ICOD.EQ.84)
     &          .OR. (ICOD.EQ.86)
     &          .OR. (ICOD.GE.169 .AND. ICOD.LE.210.and.ipar.eq.1)
C
C - c-bar mesons and c anti-baryons
      CBAR=          (ICOD.EQ.36)
     &          .OR. (ICOD.EQ.38)
     &          .OR. (ICOD.EQ.40)
     &          .OR. (ICOD.EQ.53)
     &          .OR. (ICOD.EQ.83)
     &          .OR. (ICOD.EQ.85)
     &          .OR. (ICOD.EQ.87)
     &          .OR. (ICOD.GE.169 .AND. ICOD.LE.210.and.ipar.eq.0)
      ILQUA = 0
C
C - fill ILQUA
      IF ((Ipar.eq.1.and.BBARY).or.BQmes) ILQUA=1
      IF ((Ipar.eq.0.and.BBARY).or.BBmes) ILQUA=-1
      IF (CQUA) ILQUA=2
      IF (CBAR) ILQUA=-2
      RETURN
      END
