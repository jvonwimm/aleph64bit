      SUBROUTINE GFAKE (FAKTAB,PFAKE,WARN,ERAW,DISTAN,ANG12,ECLFL)
C-----------------------------------------------------------------
C  M.Verderi                                                2-10-94
C! Preparation of the elec. fake photon likelihood computation
C
C  Called by GEMFAK
C  Calls  gfakew
C------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER NVARF
      PARAMETER (NVARF=16)
      REAL FAKTAB(NVARF,2), PFAKE, WARN, ERAW
      REAL DISTANG,ANGLE12,DISTAN,ANG12,ECLFL
      INTEGER KECLFLAG

      REAL E1,F4_1,ERAW1,THEBARY1,PHIBARY1,STAGERM1,EGERS1_1,EGERS2_1,
     &     PRFAK1_1,PRFAK2_1,ECLVS1_1,ECLVS2_1,ECLVS3_1,FENDCAP1,
     &     EFRF4I1,EC4CRA1
      REAL E2,F4_2,ERAW2,THEBARY2,PHIBARY2,STAGERM2,EGERS1_2,EGERS2_2,
     &     PRFAK1_2,PRFAK2_2,ECLVS1_2,ECLVS2_2,ECLVS3_2,FENDCAP2,
     &     EFRF4I2,EC4CRA2
      COMMON / EMFAKE /
     &     E1,F4_1,ERAW1,THEBARY1,PHIBARY1,STAGERM1,EGERS1_1,EGERS2_1,
     &     PRFAK1_1,PRFAK2_1,ECLVS1_1,ECLVS2_1,ECLVS3_1,FENDCAP1,
     &     EFRF4I1,EC4CRA1,
     &     E2,F4_2,ERAW2,THEBARY2,PHIBARY2,STAGERM2,EGERS1_2,EGERS2_2,
     &     PRFAK1_2,PRFAK2_2,ECLVS1_2,ECLVS2_2,ECLVS3_2,FENDCAP2,
     &     EFRF4I2,EC4CRA2,
     &     DISTANG,ANGLE12,KECLFLAG

      REAL XCOS
      REAL     GFAKEW
      EXTERNAL GFAKEW

C Quantites relatives au premier gamma:
      E1       = FAKTAB( 1,1)
      F4_1     = FAKTAB( 2,1)
      ERAW1    = FAKTAB( 3,1)
      THEBARY1 = FAKTAB( 4,1)
      PHIBARY1 = FAKTAB( 5,1)
      STAGERM1 = FAKTAB( 6,1)
      EGERS1_1 = FAKTAB( 7,1)
      EGERS2_1 = FAKTAB( 8,1)
      PRFAK1_1 = FAKTAB( 9,1)
      PRFAK2_1 = FAKTAB(10,1)
      ECLVS1_1 = FAKTAB(11,1)
      ECLVS2_1 = FAKTAB(12,1)
      ECLVS3_1 = FAKTAB(13,1)
      FENDCAP1 = FAKTAB(14,1)
      EFRF4I1  = FAKTAB(15,1)
      EC4CRA1  = FAKTAB(16,1)
C Quantites relatives au second  gamma:
      E2       = FAKTAB( 1,2)
      F4_2     = FAKTAB( 2,2)
      ERAW2    = FAKTAB( 3,2)
      THEBARY2 = FAKTAB( 4,2)
      PHIBARY2 = FAKTAB( 5,2)
      STAGERM2 = FAKTAB( 6,2)
      EGERS1_2 = FAKTAB( 7,2)
      EGERS2_2 = FAKTAB( 8,2)
      PRFAK1_2 = FAKTAB( 9,2)
      PRFAK2_2 = FAKTAB(10,2)
      ECLVS1_2 = FAKTAB(11,2)
      ECLVS2_2 = FAKTAB(12,2)
      ECLVS3_2 = FAKTAB(13,2)
      FENDCAP2 = FAKTAB(14,2)
      EFRF4I2  = FAKTAB(15,2)
      EC4CRA2  = FAKTAB(16,2)


      PFAKE =  0.0
      ERAW  =  0.0
      WARN  =  0.0
      DISTAN=  0.0
      ANG12 =  0.0
      ECLFL =  0.0
      XCOS = ABS(COS(THEBARY2))

C Definition of the flag warn:
C ----------------------------

C Outside acceptance: nothing done
      IF (XCOS.GT.0.95) THEN
         WARN  =  100.
         PFAKE = -1.0
         GOTO 999
      ELSE
C In overlap region:
         IF (XCOS.GT.0.72.AND.XCOS.LT.0.80) THEN
            WARN = 10.
         ELSE
C Photon on crack:
            IF (EC4CRA2/(F4_2*ERAW2).GT.0.65) THEN
               IF (XCOS.LT.0.76) THEN
                  WARN = 1.0
               ELSE
                  WARN = 2.0
               ENDIF
            ENDIF
         ENDIF
      ENDIF

C Preliminary condition for computation:
C --------------------------------------

      IF (ERAW1.NE.ECLVS1_2.AND.ERAW1.NE.ECLVS2_2.AND.
     &    ERAW1.NE.ECLVS3_2) GOTO 999

C Computation with hypothesis: eraw1 is the parent of eraw2:
C ----------------------------------------------------------

      ERAW = ERAW1

      IF (XCOS.LT.0.76) THEN
         PFAKE = GFAKEW (550,650)
      ELSE
         PFAKE = GFAKEW (750,850)
      ENDIF
C
      DISTAN = DISTANG
      ANG12  = ANGLE12
      ECLFL  = FLOAT(KECLFLAG)

999   RETURN
      END
#endif
