      SUBROUTINE X1ECWM(ISEGM,NMODS,KMODS)
C----------------------------------------------------------------------
C!  - Get mapping from modules to segments for ECAL wires
C.
C.   Author   :- Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for ALEPHLIB
C!   Modified :- Yves A. Maumary       28-MAR-1990 Bounds for ISEGM
C.
C.   Inputs:
C.        - ISEGM : INTEGER : trigger segment number
C.
C.   Outputs:
C.        - NMODS : INTEGER : number of modules mapped onto the segment
C.                            number ISEGM
C.        - KMODS : INTEGER : array of dimension NMODS containing the
C.                            module number (TRIGGER NUMBERING!!!)
C.
C?   Description
C?   ===========
C?   Returns the quantity of ECAL wire modules and their number IN THE
C?   TRIGGER NUMBERING SCHEME (!) which are mapped together onto
C?   trigger segment ISEGM given as input
C?
C.======================================================================
#ifndef DOC
      SAVE
      DIMENSION KMODS(*)
      DIMENSION ILOOK(72,3)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
C - At first entry set up the corr. matrix
        DO 10 KA = 1,72
C -- endcap A
          IF(KA.LE.12)THEN
            IOFFS = MOD(KA-1,6)
            NMODS = 2
            ILOOK(KA,1) = IOFFS*2 + 1
            ILOOK(KA,2) = IOFFS*2 + 2
            IF(ILOOK(KA,2).GT.12) ILOOK(KA,2) = 1
            ILOOK(KA,3) = 0
C -- overlap region A
          ELSEIF(KA.GT.12.AND.KA.LE.24)THEN
            IOFFS = MOD(KA-1,12)
            NMODS = 3
            ILOOK(KA,1) = IOFFS
            IF(ILOOK(KA,1).EQ.0) ILOOK(KA,1) = 12
            ILOOK(KA,2) = IOFFS + 1
            ILOOK(KA,3) = IOFFS + 13
C -- barrel
          ELSEIF(KA.GT.24.AND.KA.LE.48)THEN
            IOFFS = MOD(KA-1,12)
            NMODS = 1
            ILOOK(KA,1) = IOFFS + 13
            ILOOK(KA,2) = 0
            ILOOK(KA,3) = 0
C -- overlap region B
          ELSEIF(KA.GT.48.AND.KA.LE.60)THEN
            IOFFS = MOD(KA-1,12)
            NMODS = 3
            ILOOK(KA,1) = IOFFS + 13
            ILOOK(KA,2) = IOFFS + 24
            IF(ILOOK(KA,2).EQ.24) ILOOK(KA,2) = 36
            ILOOK(KA,3) = IOFFS + 25
C -- endcap B
          ELSEIF(KA.GT.60.AND.KA.LE.72)THEN
            IOFFS = MOD(KA-1,6)
            NMODS = 2
            ILOOK(KA,1) = IOFFS*2 + 25
            ILOOK(KA,2) = IOFFS*2 + 26
            IF(ILOOK(KA,2).GT.36) ILOOK(KA,2) = 25
            ILOOK(KA,3) = 0
          ENDIF
   10   CONTINUE
        FIRST = .FALSE.
      ENDIF
C - Normal entry : get pointers back
      IF(ISEGM.LT.1.OR.ISEGM.GT.72)THEN
        NMODS = 0
        RETURN
      ELSEIF(ISEGM.LE.12.OR.ISEGM.GT.60)THEN
        NMODS = 2
      ELSEIF(ISEGM.GT.12.AND.ISEGM.LE.24) THEN
        NMODS = 3
      ELSEIF(ISEGM.GT.24.AND.ISEGM.LE.48) THEN
        NMODS = 1
      ELSEIF(ISEGM.GT.48.AND.ISEGM.LE.60) THEN
        NMODS = 3
      ELSE
        NMODS = 0
        RETURN
      ENDIF
      KMODS(1) = ILOOK(ISEGM,1)
      KMODS(2) = ILOOK(ISEGM,2)
      KMODS(3) = ILOOK(ISEGM,3)
      RETURN
      END
#endif
