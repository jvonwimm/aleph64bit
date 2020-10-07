      SUBROUTINE VMKR95
C----------------------------------------------------------------------
C!  - Initialise the package which handles bonding maps
C!    For Vdet version '95
C!
C
C   A. Bonissent, October 1995
C
C?
C!======================================================================
#ifndef DOC
C
      CALL ABRUEV(IRUN,IEVT)
      CALL VINIBM(IRUN,IFLAG)
      IF(IFLAG.NE.1)THEN
         CALL RERROR('VMKR95',-1,'VDET bonding maps init. failed')
      ENDIF
      RETURN
      END
#endif
