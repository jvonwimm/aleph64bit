      SUBROUTINE WCLUS(NCLUS)
C-----------------------------------------------------------------------
CKEY EDIR CLUSTERS
C! This routine counts the number of clusters using the wires.
C-
C   Input  : None
C   Output : NCLUS  = Number of clusters
C-
C   Called by   : SELCAL
C   Calls  : None
C   Input banks : None
C-
C                                     Author: M.N.Minard - 910400
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
      COMMON / EWIR / EWIRE ( 36 )
      DIMENSION   NDEB(2) , NFIN(2)
      EXTERNAL NAMIND
C --
      NCLUS = 0
      NHIGH = 0
C --
C   Loop on subcomponents
C --
      DO 10 IL = 1, 3
      INIT =0
C --
C   Loop on modules
C --
      DO 9 IPL = 1, 12
      IMOD = (IL-1)*12+IPL
      IF ( EWIRE(IMOD).GT.0.200) THEN
C --
C   Energy above threshold
C --
         IF ( INIT.EQ.0 ) THEN
C --
C   Mark beginning of cluster
C --
           INIT = 1
           ECLU = EWIRE(IMOD)
           IF ( NCLUS.LT.2) THEN
             NDEB(NCLUS+1) = IMOD
           ENDIF
         ELSE
           ECLU = ECLU + EWIRE(IMOD)
         ENDIF
      ELSE
C --
C   Energy below threshold
C --
        IF ( INIT.NE.0) THEN
C --
C   Mark end of cluster
C --
            INIT = 0
            IF (ECLU.GT.3.) THEN
              NCLUS = NCLUS + 1
              IF ( NCLUS.LT.3) THEN
                NFIN (NCLUS) = IMOD - 1
              ENDIF
           ENDIF
        ENDIF
      ENDIF
 9    CONTINUE
      IF ( INIT.NE.0) THEN
         INIT = 0
         IF ( ECLU.GT.65.) NHIGH = NHIGH + 1
         IF (ECLU.GT.3.) THEN
            NCLUS = NCLUS + 1
            IF ( NCLUS.LT.3) THEN
                IF ( NCLUS.EQ.2) THEN
                   IF ( NDEB(1).EQ.(IL-1)*12+1  ) THEN
                      NDEB(1) =(IL-1)*12-(IL*12-NDEB(2))
                      NCLUS = NCLUS-1
                      GO TO 10
                   ENDIF
                ENDIF
              NFIN (NCLUS) = IL*12
            ENDIF
         ENDIF
      ENDIF
 10   CONTINUE
      IGOO = 0
      IF ( NCLUS.GT.1.AND.NHIGH.GT.0 ) NCLUS = 0
      IF ( NCLUS.EQ.1) THEN
          IF (NFIN(1).GT.12.AND.NFIN(1).LT.25) THEN
C --
C   Barrel
C --
             IF (NFIN(1)-NDEB(1).GT.3) NCLUS = 22
          ENDIF
      ENDIF
      RETURN
      END
#endif
