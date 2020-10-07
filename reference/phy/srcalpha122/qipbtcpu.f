      SUBROUTINE QIPBTCPU(CHSUB,IFLAG)
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C!    Determine CPU time usage for QIPBTAG subroutines
C  Called from QIPBTAG
C     Author  S.Schael      09-May-1995
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
      CHARACTER*(*) CHSUB
      INTEGER       IFLAG
      INTEGER       NSUBS, MSUBS
      PARAMETER     (MSUBS = 50)
      CHARACTER*20  CHSUBS(50)
      REAL          TIMING(50)
      COMMON / QIPBCPUT / NSUBS, TIMING, CHSUBS
      LOGICAL       LOT0, FIRST
      DATA          LOT0/.FALSE./, FIRST/.TRUE./
C-----------------------------------------------------------------------
      IF (FIRST) THEN
         NSUBS     = 1
         CHSUBS(1) = 'BETWEEN SUBS'
         FIRST     = .FALSE.
         CALL VZERO(TIMING,MSUBS)
         IQITI=IW(NAMIND('QITI'))
      ENDIF
C
C The routine is executed only if the user provided a 'QITI' data card :
      IF (IQITI.EQ.0) GO TO 9999
      IF (IFLAG.EQ.0 .OR. IFLAG.EQ.1) THEN
         IL = LENOCC(CHSUB)
         DO I = 2,NSUBS
            IF (CHSUBS(I)(1:IL).EQ.CHSUB(1:IL)) GOTO 200
         ENDDO
         NSUBS         = NSUBS + 1
         IF (NSUBS .GT. MSUBS) THEN
            WRITE (IW(6),*) 'QIPBTCPU: TOO MANY SUBROUTINES !'
            GOTO 9999
         ENDIF
         CHSUBS(NSUBS) = CHSUB
         I             = NSUBS
C
  200    CONTINUE
         IF (IFLAG .EQ. 0) THEN
            CALL TIMED(T0)
            TIMING(1) = TIMING(1) + T0
            LOT0      = .TRUE.
            GOTO 9999
         ENDIF
         IF (IFLAG .EQ. 1) THEN
            IF (.NOT. LOT0) THEN
               WRITE (IW(6),*) 'QIPBTCPU: WRONG ORDER OF CPU-TIME CALLS'
               GOTO 9999
            ENDIF
            CALL TIMED(DT)
            TIMING(I) = TIMING(I) + DT
            LOT0      = .FALSE.
         ENDIF
C
      ELSEIF (IFLAG .EQ. 2) THEN
C
C------- PRINT CPU-TIMEING
         TTOT = VSUM(TIMING,NSUBS)
         WRITE (IW(6),*)
         WRITE (IW(6),10600) 'Q I P B T A G :     C P U   -  T I M E'//
     .                  '  U S A G E'
         WRITE (IW(6),10700) 'SUBROUTINE','CPU TIME(%)','CPU TIME (S)'
         WRITE (IW(6),10000)
         DO I = 1,NSUBS
            WRITE (IW(6),'(1X,A20,2(F9.4,3X))') CHSUBS(I),
     .                                     TIMING(I)/TTOT*100,
     .                                     TIMING(I)
         ENDDO
         WRITE (IW(6),10000)
         WRITE (IW(6),*) 'TOTAL CPU TIME (S): ',TTOT
         WRITE (IW(6),*)
         GOTO 9999
      ELSE
         WRITE (IW(6),*) 'QIPBTCPU: INVALID FLAG=',IFLAG
      ENDIF
C
 9999 RETURN
C
10000 FORMAT (1X,80('-'))
10600 FORMAT (1X,10('*'),2X,A,2X,10('*'))
10700 FORMAT (1X,A,2(10X,A))
      END
