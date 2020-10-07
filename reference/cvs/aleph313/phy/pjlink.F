      SUBROUTINE PJLINK( KOLDPJ, NP, KJJ, NJ, KNEWPJ, ERROR )
C----------------------------------------------------------------------
CKEY PHYSICS JADE JETS
C   Author   : C. Bowdery     10-JAN-1991    Update particle-jet links
C
C   Description
C   ===========
C!    Update links between particles and jets after further evolution
C!    of jet clustering from standard y-cut to user-specified y-cut.
C
C
C  input   : KOLDPJ    Old assoc. of original particles to old jets
C            NP        Number of original particles (dim of KOLDPJ)
C            KJJ       Assoc. of old jets to new jets
C            NJ        Number of old jets (dim of KJJ)
C
C  output  : KNEWPJ    New assoc. of original particles to new jets
C                      KNEWPJ can be the same as KOLDPJ if wanted.
C            ERROR     .TRUE. if KOLDPJ has an illegal jet number
C                      .FALSE. otherwise
C
C  The routine assumes the original particles are numbered 1..NP
C  and that KOLDPJ(i) contains info on particle i
C  and that the old jets are in the range 1..NJ
C  and that KJJ(j) contains info on old jet j.
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
      INTEGER  NP, NJ, I
C
      INTEGER  KOLDPJ(NP), KJJ(NJ), KNEWPJ(NP)
C
      LOGICAL  ERROR
C
C------------------------ C O D E --------------------------------------
C
C                            Examine the association of the original
C                            particle I to jet JET. If JET exists in KJJ
C                            list, copy assignment of old jet to new jet
C                            for particle I.
C
      ERROR = .FALSE.
C
      DO  1000  I = 1,NP
C
        JET = KOLDPJ(I)
        IF( JET .GT. 0  .AND.  JET .LE. NJ ) THEN
          KNEWPJ(I) = KJJ(JET)
        ELSE
          ERROR = .TRUE.
        ENDIF
C
 1000 CONTINUE
C
      RETURN
      END
#endif
