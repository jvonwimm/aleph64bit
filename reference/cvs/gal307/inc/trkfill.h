*CD trkfill      FILL MCTRACKELEMENT /TRKCOM/
C - If it is the 1st point ( IGNWVO=1 ) fill ITRKEL and part of TRKELE
C
         IF (FIRST) THEN
            ITRKEL(1) = IGTRA
            ITRKEL(2) = IGSTAK
            ITRKEL(3) = IGVERT
            ITRKEL(4) = IGPART
            ITRKEL(5) = NGAMES(NGLEVE)
            ITRKEL(6) = NGUMBR(NGLEVE)
            ITRKEL(7) = IGSVOL
            ITRKEL(8) = IGNWVO
            ITRKEL(9) = IGSTOP
            ISEN = IUCOMP(ITRKEL(5),IAGSEN(1,1),LSENV)
            IF (ISEN .EQ. 0) THEN
               ITRKEL(10) = 0
            ELSE
               ITRKEL(10)= NGUMBR(IAGSEN(ISEN,2))
            ENDIF
            ITRKEL(11)= IGTRTY
            CALL UCOPY (GVECT,TRKELE,7)
            TRKELE(8) = GETOT
            TRKELE(9) = GSLENG
            TRKELE(10)= GTOFG
            TRKELE(11)= GSTEP
            TRKELE(12)= GDESTP
            TRKELE(13)= GMASS
            TRKELE(14)= GCHARG
            TRKELE(15)= GTRAN(1,NGLEVE)
            TRKELE(16)= GTRAN(2,NGLEVE)
            TRKELE(17)= GTRAN(3,NGLEVE)
            CALL UCOPY (GRMAT(1,NGLEVE),TRKELE(18),10)
         ENDIF
C
C - If the point is inside the medium ( IGNWVO=0,2 ) complete TRKELE
C   and fill TRKNXT
C
         IF (FINTO) THEN
            IF (ITRKEL(8).EQ.0 .AND. .NOT.FIRST) THEN
               CALL UCOPY (TRKNXT(1),TRKELE(1),10)
            ENDIF
            CALL UCOPY (GVECT(1),TRKNXT(1),7)
            TRKNXT(8) = GETOT
            TRKNXT(9) = GSLENG
            TRKNXT(10)= GTOFG
C
            TRKELE(11) = GSTEP
            TRKELE(12) = GDESTP
            ITRKEL(8)  = IGNWVO
            ITRKEL(9)  = IGSTOP
         ENDIF
C
