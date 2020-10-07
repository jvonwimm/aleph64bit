      SUBROUTINE ABWEVE
C --------------------------------------------------------------
C - F.Ranjard - 900927      from H.Albrecht
C! write output file with/without EDIR
CKEY ALPHARD WRITE
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
      INTEGER IBITC(30)
      DATA    IBITC/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     +        8192,16384,32768,65536,131072,262144,524288,1048576,
     +        2097152,4194304,8388608,16777216,33554432,67108864,
     +        134217728,268435456,536870912/
C --------------------------------------------------------------
  600 IF (IUTSEL .NE. 0)  THEN
        IF (MASKW .EQ. 0)  THEN
          CALL BCLASW (IUTSEL, 0)
        ELSE
          DO 610 I=1,30
  610     IF (IAND (MASKW, IBITC(I)) .NE. 0)  CALL BCLASW (IUTSEL, I)
        ENDIF
      ENDIF
C       write FILM card if 1st event of EDIR on input file
      IF (FWFILM) CALL CRFILM
      FWFILM = .FALSE.
C
      IF (IUTDAT(1) .NE. 0)  THEN
        CLOSE2 = .TRUE.
        IF(WLIST.EQ.'E') CALL CMPLIS (WLIST, I)
        CALL BWRITE (IW, IUTDAT(1), WLIST)
      ENDIF
      WLIST = ' '
C
      END
