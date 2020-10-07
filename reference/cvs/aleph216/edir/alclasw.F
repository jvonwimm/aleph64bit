      SUBROUTINE ALCLASW (KCLASW)
C----------------------------------------------------------------------
CKEY ALEF EDIR CLASS / USER
C - F.Ranjard - 910801                    from M.Talby
C! get ALEPH class word
C-
C   Output :  KCLASW   / I   = 30 bit mask known as EDIR class word
C-
C   Called by   : USER
C   Calls  : SELEVT, ABCLAS,ABGTWCL
C----------------------------------------------------------------------
#ifndef DOC
      LOGICAL EVTFLG(30)
C --------------------------------------------------------------------
C - make the selection
      CALL SELEVT (EVTFLG)
C - set the write class word to 0
      CALL ABCLAS (0)
C - fill the write class word
      ICLAS = 30
      DO 1 IC=1,29
         IF (EVTFLG(IC)) THEN
            ICLAS = IC
            CALL ABCLAS (ICLAS)
         ENDIF
  1   CONTINUE
C - if no class has been selected set class 30
      IF (ICLAS.EQ.30) CALL ABCLAS (30)
C - get write class word from ALPHARD package
      CALL ABGTWCL (KCLASW)
      END
#endif
