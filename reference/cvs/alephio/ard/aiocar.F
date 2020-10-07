      SUBROUTINE AIOCAR( CARINP,NINP, CAROUT,NOUT )
C-----------------------------------------------------------------------
CKEY ALREAD RUNCARDS
C   D. Schlatter   4.89
C!  returns list of valid RUN-cards for input and output
C   Input :  NINP      /I    number of different input cards
C            NOUT      /I    number of different output cards
C
C   Output:  CARINP    /C    valid card-names for input
C            CAROUT    /C    valid card-names for output
C-----------------------------------------------------------------------
#ifndef DOC
        CHARACTER*4 CARINP(*),CAROUT(*)
C ---------------------------------------------------------------------
        IF (NINP.GE.1) CARINP(1)='FILI'
        IF (NOUT.GE.1) CAROUT(1)='FILO'
        IF (NOUT.GE.2) CAROUT(2)='POT '
        IF (NOUT.GE.3) CAROUT(3)='DST '
        IF (NOUT.GE.4) CAROUT(4)='MDST'
        END
#endif
