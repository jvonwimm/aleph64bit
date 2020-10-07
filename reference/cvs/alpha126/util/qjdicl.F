      SUBROUTINE QJDICL(NJETS,CNAM,ICLASS,PTCUT,PTNEXT,
     &                  NJMAX,NJMIN,MR,MD)
CKEY JETS /USER
C----------------------------------------------------------------------
C   Author   : J. von Wimmersperg       28-JUL-2015
C
C   Description
C   ===========
C!   Set up and call QGDICL Jet finding routine. (DICLUS)
C  input:
C         PTCUT: if PTCUT=-N then force to N jets
C 
C output: NJETS= number of reconstructed jets
C              < 0 error

C         QCTBUF.INC contains jet momenta and track-jet association
C======================================================================
#ifndef DOC
      CHARACTER *(*) CNAM
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGDICL(NJETS,PTCUT,PTNEXT,NJMAX,NJMIN,MR,MD)
C
      IF (CNAM.NE.' ')  THEN
        IDR=0
        DO 1 IJET = NJETS, 1, -1
          CALL QJSAVE (CNAM,IJET,ICLASS,IDR,0,1)
          IDR=1
    1   CONTINUE
      ENDIF
C
      END
#endif
