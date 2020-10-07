      SUBROUTINE IRDDAF(IRUN,IRET)
C-----------------------------------------------------------------------
C! Read DAF banks for ITC - for 'prepare data' only.
C!
CKEY ITCDES ITC
C!   Author     :-  J. Sedgbeer  89/03/07
C!   Modified   :-  J. Sedgbeer  92/01/30 Remove call IFEOLD (obsolete)
C!                                        Add calls to ALTELL.
C!   Input:
C!    IRUN    /I : Current run number
C!
C!   Output:
C!    IRET    /I : Error flag: (as for AGETDB)
C!                   IRET > 0  Existing values still valid
C!                   IRET = 0  Error. One or more banks missing for this
C!                             run - leave values unchanged or, if first
C!                             time use default values.
C!                   IRET < 0  1 or more banks reloaded
C!
C!   calls     : IGEOMW  (ALEPHLIB)  Get ITC Geometry consts.
C!               IFECON  (ALEPHLIB)  Get Front-end constants
C!               IINALI  (ALEPHLIB)  Get alignment constants
C!               IINRES  (ALEPHLIB)  Get Drift-time and resolution consts.
C!
C!   libraries:  none
C!
C!   Description:
C! Set up ITC constants from DAF. Gets constants for geometry, alignment,
C! front-end and resolution only, i.e. all constants necessary for
C! ITC prepare data.
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C-----------------------------------------------------------------------
      CALL IGEOMW(IRUN,IRET1)
      CALL IINALI(IRUN,IRET2)
      CALL IFECON(IRUN,IRET3)
      CALL IINRES(IRUN,IRET4)
C
      IF(IRET1.EQ.0) CALL ALTELL(
     +  'IRDDAF: Missing ITC Geometry banks. ** SERIOUS ERROR **.',
     +  0,'RETURN')
      IF(IRET2.EQ.0) CALL ALTELL(
     +  'IRDDAF: Missing ITC Alignment banks. ** SERIOUS ERROR **.',
     +  0,'RETURN')
      IF(IRET3.EQ.0) CALL ALTELL(
     +  'IRDDAF: Missing ITC Front-end banks. ** SERIOUS ERROR **.',
     +  0,'RETURN')
      IF(IRET4.EQ.0) CALL ALTELL(
     +  'IRDDAF: Missing ITC Resolution banks. ** MAYBE SERIOUS **.',
     +  0,'RETURN')
C
      IRET = 1
      IF(IRET1.LT.0.OR.IRET2.LT.0)  IRET = -1
      IF(IRET3.LT.0.OR.IRET4.LT.0)  IRET = -1
      IF(IRET1.EQ.0.OR.IRET2.EQ.0)  IRET = 0
      IF(IRET3.EQ.0.OR.IRET4.EQ.0)  IRET = 0
C
      END
#endif
