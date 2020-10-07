      SUBROUTINE MINBLD(NWANT,WLIST,KEEP,CLASS)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Build Mini banks for data.
C
C     Author: Stephen Haywood      06-Jan-93
C     Modify: Agnieszka Jacholkowska   30-Sep-98
C     Modified J. Boucrot  12-October 2000 : E14 and Eflow banks
C
C     Input  : NWANT  = number of banks listed on MINI card
C              WLIST  = Wish-list of banks to be written on Mini
C              KEEP   = flag indicating whether event should be kept
C              CLASS  = logical array set according to class word
C-----------------------------------------------------------------------
#ifndef DOC
C
      CHARACTER*800 WLIST
      CHARACTER*4 NAME
      LOGICAL WANT,CLASS
      DIMENSION CLASS(32)
      INTEGER MVR
C-----------------------------------------------------------------------
C++   Determine whether one writes new or old-fashioned MINIs:
C
      MVR=MINVSN(DUMMY)
      IF (MVR.LT.204) THEN
         CALL MINBLO(NWANT,WLIST,KEEP,CLASS)
      ELSE
         CALL MINBLN(NWANT,WLIST,KEEP,CLASS)
      ENDIF       
C
      RETURN
      END
#endif
