      SUBROUTINE ALSTHO(ITAB,NWDS,CHSTR)
C-------------------------------------------------------------------------------
C! Transform an array of hollerith  into the corresponding character string
CKEY  ALEF CHARACTER HOLLERITH / USER
C Author     F.Ranjard - 901220
C Input arguments :
C   ITAB = array of holleriths
C   NWDS = number of words filled in ITAB
C Output argument :  CHSTR = character string
C CHSTR is blank if NWDS = 0
C-------------------------------------------------------------------------------
#ifndef DOC
      PARAMETER ( NCHW = 4 )
      CHARACTER*4 CHAHOL
      CHARACTER*(*) CHSTR
      INTEGER    ITAB(*)
C
      K1=1
      K2=NCHW
      CHSTR='    '
      DO 10 I=1,NWDS
         CHSTR(K1:K2)=CHAHOL(ITAB(I))
         K1=K1+NCHW
         K2=K2+NCHW
 10   CONTINUE
C
      KOU=MAX0(K1-1,NCHW)
      CHSTR=CHSTR(1:KOU)
      RETURN
      END
#endif
