      SUBROUTINE ALINST(CHSTR,ITAB,NWDS)
C-------------------------------------------------------------------------------
C! Transform a character string CHSTR of arbitrary length
C  into its integer representation
CKEY  ALEF CHARACTER INTEGER / USER
C Author     J. Boucrot     25-Nov-1988
C Input argument :  CHSTR = character string
C Output arguments :
C   ITAB = array of integer representations
C   NWDS = number of words filled in ITAB
C-------------------------------------------------------------------------------
#ifndef DOC
      PARAMETER (NCHW = 4)
      CHARACTER*(*) CHSTR
      INTEGER    ITAB(*)
C
      LENV=LNBLNK(CHSTR)
      NWDS=(LENV+NCHW-1)/NCHW
C Input is blank word :
      IF (NWDS.EQ.0) THEN
         NWDS=1
         ITAB(1)=INTCHA('    ')
         GO TO 999
      ENDIF
C
C Input is not blank :
C
      LA=1
C
      DO 10 I=1,NWDS
         LB=MIN(LA+NCHW-1,LENV)
         ITAB(I)=INTCHA(CHSTR(LA:LB))
         LA=LB+1
 10   CONTINUE
C
 999  RETURN
      END
#endif
