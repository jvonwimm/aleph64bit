      INTEGER FUNCTION ECXMOD ( IWORD )
C----------------------------------------------------------------------
CKEY E_SET EHPA ECAL MODULE
C!  - Decode the EHPA data word and convert into a module number
C!
C!     Lee F. Thompson. 29th April 1994.
C?
C =====================================================================
#ifndef DOC
      INTEGER CABLE
      INTEGER IWORD
C*---------------------------------------------------------------------
      ECXMOD = 0
      CABLE = IBITS ( IWORD , 19 , 12 ) / 8
      IF ( CABLE .LT. 48 ) THEN
          ECXMOD = CABLE / 4  + 1
      ELSEIF ( CABLE .LT. 240 ) THEN
          ECXMOD = ( CABLE - 48 ) / 16 + 13
      ELSE
          ECXMOD = ( CABLE - 240 ) / 4 + 25
      ENDIF
      RETURN
      END
#endif
