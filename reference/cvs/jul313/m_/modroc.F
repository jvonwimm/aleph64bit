      INTEGER FUNCTION MODROC ( IROC , IADC )
C--------------------------------------------------
C!    Calculate ECAL module number from ROC and ADC
C!
C!     Lee F. Thompson. 23rd April 1990.
C?
C!======================================================================
#ifndef DOC

       MODROC = 0

       IF ( IROC .LT. 0 .OR. IROC .GT. 8 ) GOTO 999
       IF ( IROC .EQ. 1 .OR. IROC .EQ. 8 ) THEN
           IF ( IADC .LT. 0 .OR. IADC .GT. 15 ) GOTO 999
           MODROC = 1 + IADC / 2
           IF ( IROC .EQ. 8 ) MODROC = MODROC + 24
       ELSE
           MODROC = 13 + (IROC-2)*2 + IADC/8
       END IF

  999 RETURN
      END
#endif
