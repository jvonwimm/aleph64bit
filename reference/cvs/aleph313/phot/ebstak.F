        SUBROUTINE EBSTAK( NVTM , MODU , ECLS , STAC )
C ----------------------------------------------------
C   AUTHOR   : J.Badier      28/09/89
C! Stacks contents when several modules.
CKEY PHOTONS STACKS / INTERNAL
C
C   Input     : NVTM    Overlap region code.
C               MODU(M,1)  Module number of the module M.
C               MODU(M,2)  Subcomponent number of the module M.
C               ECLS(K,M)  Content of the stack K in the module M.
C                       Up to 3 modules ordered as decreasing energies.
C
C   Output    : STAC(3)  Stacks contents.
C                        If NVTM = 2 or 4 : STAC(1) = Barrel ,
C                        STAC(2) = Endcap st1 , STAC(3) = Endcap st2,3
C                        If NVTM = 3 or 5 : STAC(1) = Barrel st1 ,
C                        STAC(2) = Barrel st2,3 , STAC(3) = Endcap.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION MODU(3,*) , ECLS(3,*) , STAC(*) , ECL(3,2)
      IF ( MODU(2,1) .EQ. 0 ) THEN
C   One module.
        STAC(1) = ECLS(1,1)
        STAC(2) = ECLS(2,1)
        STAC(3) = ECLS(3,1)
      ELSE
C ----- Stacks of a same detector are gathered in ECL
        DO 1 I = 1,2
        DO 1 J = 1,3
    1   ECL(J,I) = 0.
        DO 2 IMD = 1,3
          IF( MODU(IMD ,1) .EQ. 0 ) GO TO 4
C ----- JDET = 1 for barrel, JDET =2 for endcap.
          JDET = 2
          IF( MODU(IMD,2) .EQ. 2 ) JDET = 1
          DO 3 J = 1,3
            ECL(J,JDET) = ECL(J,JDET) + ECLS(J,IMD)
    3     CONTINUE
    2   CONTINUE
    4   CONTINUE
C ---- 2 or 3 modules.
        IF( ( NVTM .EQ. 0 .AND. MODU(1,2) .EQ. 2 ) .OR.
     +        NVTM .EQ. 6 ) THEN
C   Barrel.
          STAC(1) = ECL(1,1)
          STAC(2) = ECL(2,1)
          STAC(3) = ECL(3,1) + ECL(1,2) + ECL(2,2) + ECL(3,2)
        ELSE
C   Endcap.
          IF( ( NVTM .EQ. 0 .AND. MODU(1,2) .NE. 2 ) .OR.
     +          NVTM .EQ. 1 )                        THEN
            STAC(1) = ECL(1,2) + ECL(1,1) + ECL(2,1) + ECL(3,1)
            STAC(2) = ECL(2,2)
            STAC(3) = ECL(3,2)
          ELSE
C   Overlap near endcap.
            IF ( NVTM .EQ. 2 .OR. NVTM .EQ. 4 ) THEN
              STAC(1) = ECL(1,1) + ECL(2,1) + ECL(3,1)
              STAC(2) = ECL(1,2)
              STAC(3) = ECL(2,2) + ECL(3,2)
            ELSE
              IF ( NVTM .EQ. 3 .OR. NVTM .EQ. 5 ) THEN
C   Overlap near barrel.
                STAC(1) = ECL(1,1)
                STAC(2) = ECL(2,1) + ECL(3,1)
                STAC(3) = ECL(1,2) + ECL(2,2) + ECL(3,2)
              ELSE
C   Poubelle.
                STAC(1) = ECLS(1,1)
                STAC(2) = ECLS(2,1)
                STAC(3) = ECLS(3,1)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
#endif
