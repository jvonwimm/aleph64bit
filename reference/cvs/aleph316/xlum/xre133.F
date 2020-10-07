      LOGICAL FUNCTION XRE133(KRUN,KEVT)
C**********************************************************
C
C  Author : J. Boucrot, March 06 1997
C  Modified : B Bloch from QRE133(ALPHA)for alephlib,Nov 1997
C! Flags laser events taken during LEP 1.5 data
C     XRE133 = .True.  if NOT laser TPC event
C     XRE133 = .False. if unrecognisable TPC laser event
C
C***********************************************************
#ifndef DOC
      PARAMETER ( NBADS = 13)
      INTEGER LBADR(NBADS),LBADE(NBADS)
      DATA LBADR / 40307,40316,40334,40345,40395,40395,
     +             40396,40396,40418,40441,40471,40483,
     +             40512 /
      DATA LBADE /  7214, 3398, 1542, 5064, 7511,13615,
     +              1993, 8630, 3126, 6159, 2751, 1596,
     +              1391 /
C
C
      XRE133=.TRUE.
      IF (KRUN.LT.40000.OR.KRUN.GT.40550) GO TO 999
      DO 100 IK=1,NBADS
         IF (KRUN.NE.LBADR(IK).OR.KEVT.NE.LBADE(IK)) GO TO 100
         XRE133=.FALSE.
         GO TO 999
 100  CONTINUE
C
 999  RETURN
      END
#endif
