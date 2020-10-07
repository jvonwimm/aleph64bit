      FUNCTION HCLTST(IT1,JF1,NR1,IT2,JF2,NR2)
C----------------------------------------------------------------
C!
C! test storey connection
C!
C!      G.Capon   date : 861001  , revised 20-jun-1990
C!      function to test the connection between two storeys
C!      if the two storeys are connected  HCLTST is GE.0.
C!      criterion : at least a contact between storey corners
C!      input : IT1,JF1,NP1 = indices Itheta, Iphi, Nregion for storey 1
C!              IT2,JF2,NP2 = indices Itheta, Iphi, Nregion for storey 2
C!        called by HCFCLU
C!
C?
C!======================================================================
#ifndef DOC
      DIMENSION JFMAX(3)
      DATA JFMAX/24,48,96/
C
      HCLTST=-1.
      IF(IABS(IT1-IT2).GT.1) RETURN
      IF(JF1.EQ.1 .AND. JF2.EQ.JFMAX(NR2)) THEN
         HCLTST=1.
         RETURN
      ENDIF
      IF(JF2.EQ.1 .AND. JF1.EQ.JFMAX(NR1)) THEN
         HCLTST=1.
         RETURN
      ENDIF
      IF(NR1.EQ.NR2)THEN
         HCLTST=(JF2-JF1+1)*(JF1-JF2+1)
      ELSEIF(NR1.GT.NR2)THEN
         HCLTST=(JF1-2*JF2+2)*(2*JF2-JF1+1)
      ELSEIF(NR2.GT.NR1)THEN
         HCLTST=(JF2-2*JF1+2)*(2*JF1-JF2+1)
      ENDIF
      RETURN
      END
#endif
