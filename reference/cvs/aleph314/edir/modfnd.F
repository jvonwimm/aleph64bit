      SUBROUTINE MODFND(THETA,PHI,IREG,MODULE,IRC)
C-----------------------------------------------------------------------
CKEY EDIR ECAL MODULES
C! find which module a given PECO is in.
C-
C   Input  : THETA  = Theta angle of Ecal cluster
C            Phi    = Phi angle of Ecal cluster
C   Output : IREG   = Ecal region flag
C                     1 if Barrel
C                     2 if Endcap
C            MODULE = Module number
C            IRC    = Return code
C                     0 : success
C                     1 : problem
C-
C   Called by   : PHEDIR
C   Calls :  None
C   Input banks : None
C-
C                               Author: Lee Thompson, 31st January 1990
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
      IRC = 0
      IREG = 0
      THETA = THETA*180./3.14159
      PHI   = PHI*180./3.14159
C --
C   Barrel or endcap
C --
      IF (THETA.GT.40..AND.THETA.LT.140.) THEN
         PHIOFF = 1.875
         MODOFF = 12
         IREG = 2
      ELSE IF (THETA.LE.40.) THEN
         PHIOFF = 15. + 1.875
         MODOFF = 0
         IREG = 1
      ELSE IF (THETA.GE.140.) THEN
         PHIOFF = 15. + 1.875
         MODOFF = 24
         IREG = 3
      ELSE
         IRC = 1
      ENDIF
C --
      IF (IRC.EQ.1) RETURN
C --
      PHICOR = PHI + PHIOFF
      IF (PHICOR.GE.360.) PHICOR = PHICOR - 360.
      MODULE = INT(PHICOR/30.) + MODOFF + 1
C --
      RETURN
      END
#endif
