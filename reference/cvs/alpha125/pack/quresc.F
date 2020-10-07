      SUBROUTINE QURESC
CKEY FILL /USER       
C-----------------------------------------------------------------------
C!  User-interface for calling rescaling routines before the
C!  Energy-Flow
C!  called from QFILL
C                                 F.Ligabue      07-97
C-----------------------------------------------------------------------
#ifndef DOC
C
C    HCALENSC : HCAL ONLINE BUG CORRECTION      
C
C link the following files:
C
C /al/reference/phy/hcalensc.F   /al/reference/phy/qcorfic.F  (on UNIX) 
C phy:hcalensc.F      phy:qcorfic.F         (on ALWS)
C    
C then      
C
C      CALL HCALENSC
C-----------------------------------------------
C
C   correct for even-odd HCAL intermodule data/MC discrepancy 
C
C 1 -  link the following file:
C       /al/reference/phy/calresc_impd.F   (on UNIX)
C       phy:calresc_impd.F                 (on ALWS)
C 2 -  Put the data card HCIM in your CARDS file
C 3 -  then
C
C      CALL CALRESC_IMPD 
C-----------------------------------------------------
C   
C   rescale ECAL and HCAL for systematic studies.
C
C 1 -  link the following file:
C       /al/reference/phy/calresc_syst.F   (on UNIX)
C       phy:calresc_syst.F                 (on ALWS)
C 2 - Put the data card CALS in your CARDS file
C 3 - then
C
C      CALL CALRESC_SYST    
C      
      END
#endif
