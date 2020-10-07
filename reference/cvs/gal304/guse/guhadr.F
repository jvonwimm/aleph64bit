*DK guhadr
      SUBROUTINE GUHADR
C ----------------------------------------------------------------------
C. - F.Ranjard - 850430
C. - GEANT3 user routine called when a hadronic process has been
C.   selected in the current step.
C. - Called from    GTHADR                              from GEANT3 pam
C. - Calls          GHEISH                              from GEANT3 pam
C.
C --------------------------------------------------------
#ifndef DOC
C
      CALL GHEISH
C
      RETURN
       END
#endif
