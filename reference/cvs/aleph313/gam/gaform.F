      SUBROUTINE GAFORM(NSTAL,LISTAL,ESTAL,LSTK,ESTK,NSTK,KSTAG)
C.----------------------------------------------------------------------
CKEY GAMPACK STORE / INTERNAL
C   J.C.Brient      Creation  1/10/91
C! Store in right format the storeys info. of photon for stack KSTAG
C   Input :
C           NSTAL   # of storeys of the photon   INTEGER
C           LISTAL  all the storeys of photon    INTEGER
C           ESTAL   storeys energy               REAL
C           KSTAG   stack number to store        INTEGER
C   Output:
C           NSTK    # of storeys in stack        INTEGER
C           LSTK    list of storeys              INTEGER
C           ESTK    storeys energy               REAL
C   Calls: None
C   Called by GAMPEX
C.----------------------------------------------------------------------
#ifndef DOC
      DIMENSION LISTAL(3,*),ESTAL(*),LSTK(3,*),ESTK(*)
      NSTK = 0
      DO I = 1 , NSTAL
        IF(LISTAL(3,I) .EQ. KSTAG ) THEN
          NSTK = NSTK + 1
          LSTK(1,NSTK)= LISTAL(1,I)
          LSTK(2,NSTK)= LISTAL(2,I)
          LSTK(3,NSTK)= LISTAL(3,I)
          ESTK( NSTK )= ESTAL(I)
        ENDIF
      ENDDO

      RETURN
      END
#endif
