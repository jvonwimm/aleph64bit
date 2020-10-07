      SUBROUTINE ADIREC (FDIREC)
C --------------------------------------------------------------
C! set the flag FDIREC to .FALSE. at 1st entry
CKEY ALREAD DIRECT ACCESS DAF EDIR / INTERNAL
C - F.Ranjard - 891020
C --------------------------------------------------------------
#ifndef DOC
      LOGICAL FDIREC
      DATA IFI /0/
C -------------------------------------------------------------
      IF (IFI.EQ.0) THEN
         IFI = 1
         FDIREC = .FALSE.
      ENDIF
      END
#endif
