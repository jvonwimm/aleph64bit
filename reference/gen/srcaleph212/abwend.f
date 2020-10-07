      SUBROUTINE ABWEND
C ----------------------------------------------------------------
C - F.Ranjard - 900927          from H.Albrecht
C! close output file
CKEY ALPHARD CLOSE OUTPUT FILE
C ----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /ABRCOM/ BATCH,INIT,CLOSE1,CLOSE2,FWFILM
     &               ,IUNDAT(2),IUTDAT(5),IUNSEL,IUNSE2,IUTSEL
     &               ,MASKA,MASKW
     &               ,WLIST,TLIST
      LOGICAL BATCH, INIT, CLOSE1, CLOSE2, FWFILM
      CHARACTER*1   TLIST, WLIST
C
C ---------------------------------------------------------------
      IF (WLIST .NE. ' ')  CALL ABWEVE
      IF (CLOSE1)  CALL BCLAST (IUTSEL)
      CLOSE1 = .FALSE.
      IF (CLOSE2)  CALL BWRITE (IW, IUTDAT(1), '0')
      CLOSE2 = .FALSE.
      INIT = .TRUE.
C
      END
