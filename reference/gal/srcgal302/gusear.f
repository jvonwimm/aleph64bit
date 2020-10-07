      SUBROUTINE GUSEAR
C -----------------------------------------------
C. - F.Ranjard - 850515
C. - dummy routines
C --------------------------------------------------
      CHARACTER*6 SUBR
C -------------------------------------------------
      SUBR='GUSEAR'
      GOTO 999
      ENTRY GUINME
      SUBR='GUINME'
      GOTO 999
      ENTRY GUNCON
      SUBR='GUNCON'
      GOTO 999
      ENTRY GUNSOU
      SUBR='GUNSOU'
      GOTO 999
      ENTRY GUNXTS
      SUBR='GUNXTS'
C
 999  CALL ALTELL ('GUSEAR: the routine does not exist ',0,'FATAL')
      RETURN
      END
