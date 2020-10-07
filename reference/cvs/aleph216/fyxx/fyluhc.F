      INTEGER FUNCTION FYLUHC(IFKIN,MXDAU,LUDAU)
C -----------------------------------------------------------
CKEY FYXX MCARLO KINE HISTORY / USER
C - F.Ranjard - 880920
C! Decode LUND history code.
C  Return the # of daughters and the list of daughter#s of a track
C  known by its FKIN row #.
C  There are FYLUHC such daughters.
C  if FYLUHC .eq. 0 then NO daughter, or NOT a LUND history code
C  if FYLUHC .gt. MXDAU then the buffer LUDAU is too small
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: FYLUHC
C              External References: NAMIND(BOS77)
C              Comdecks referenced: BCS, BMACRO
C
C - usage  : NDAU  = FYLUHC (IHCOD,MXDAU,LUDAU)
C - input  : IFKIN = track# known by its row# in FKIN bank
C            MXDAU = length of LUDAU array
C - output : LUDAU = array which contains the list of daughters
C            FYLUHC= # of daughters ( the #s of the 1st MXDAU are
C                    stored in LUDAU)
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fxjjpar.h"
      INTEGER LUDAU(*)
      DATA NFKIN /0/
#include "bmacro.h"
C - history code
      IHCOD(IFLR) = ITABL(JFKIN,IFLR,JFKIHC)
C - mother track#
      MOTHER(IFLR)= MOD (IHCOD(IFLR),10000)
C --------------------------------------------------------------
      IF (NFKIN.EQ.0) THEN
         NFKIN = NAMIND('FKIN')
      ENDIF
C
      NDAU = 0
C
C - IF not FKIN bank RETURN with FYLUHC=0
      JFKIN = IW(NFKIN)
      IF (JFKIN.EQ.0) GOTO 900
C
C - IF not a valid track# THEN RETURN with FYLUHC=0
      IF (IFKIN.EQ.0 .OR. IFKIN.GT.LROWS(JFKIN)) GOTO 900
C
C - IF not a valid LUND history code THEN RETURN with FYLUHC=0
      IF (IHCOD(IFKIN) .EQ. 0) GOTO 900
C
C - Loop over all history codes which are .ne. 0 to find the
C   daughters of track# IFKIN
      DO 10 I=1,LROWS(JFKIN)
         IF (IHCOD(I) .EQ. 0) GOTO 10
         IF (MOTHER(I).EQ.IFKIN) THEN
            NDAU = NDAU + 1
            IF (NDAU.LE.MXDAU) LUDAU(NDAU) = I
         ENDIF
 10   CONTINUE
C
 900  FYLUHC = NDAU
      END
#endif
