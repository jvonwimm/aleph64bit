      SUBROUTINE ALSTIN(ITAB,NWDS,CHSTR)
C-----------------------------------------------------------------------
C! Transform an array of char. integer representations
C! into the corresponding character string
CKEY  ALEF CHARACTER INTEGER / USER
C Author     J. Boucrot     25-Nov-1988
C Input arguments :
C   ITAB = array of integer representations
C   NWDS = number of words filled in ITAB
C Output argument :  CHSTR = character string
C CHSTR is blank if NWDS = 0
C-----------------------------------------------------------------------
      PARAMETER ( NCHW = 4 )
      CHARACTER*4 CHAINT
      CHARACTER*(*) CHSTR
      INTEGER    ITAB(*)
C
      K1=1
      K2=NCHW
      CHSTR='    '
      DO 10 I=1,NWDS
         CHSTR(K1:K2)=CHAINT(ITAB(I))
         K1=K1+NCHW
         K2=K2+NCHW
 10   CONTINUE
C
      KOU=MAX0(K1-1,NCHW)
      CHSTR=CHSTR(1:KOU)
      RETURN
      END
