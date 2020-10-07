      SUBROUTINE QSBANK (CBANK,N)
CKEY BANKS /INTERNAL
C----------------------------------------------------------------------
C! Create ALPHA banks or increase their size
C called from anywhere
C                                                   H.Albrecht 03.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER *(*) CBANK
      COMMON / FINDPA / NAQPSP,NAQAFP,NQAFP
C----------------------------------------------------------------------
C
      IF (CBANK .EQ. 'QLIN')  THEN
        KOQLIN = NBANK ('QLIN', 0, N + LMHLEN) + LMHLEN
        IF (KOQLIN .EQ. LMHLEN)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QLIN')
        IW(KOQLIN-1) = 1
        IW(KOQLIN) = N
C
      ELSE IF (CBANK .EQ. 'QVEC')  THEN
        NOLD = 1
        IF (IW(NAQVEC) .NE. 0)  NOLD = IW(IW(NAQVEC)+2) + 1
        KQVEC = NBANK ('QVEC', 0, N * KCQVEC + LMHLEN)
        IF (KQVEC .EQ. 0)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QVEC')
        IW(KQVEC+1) = KCQVEC
        IW(KQVEC+2) = N
        KOQVEC = KQVEC + LMHLEN - KCQVEC
        KLFRT = N
C         KSAME pointer
        DO 10 I=NOLD,N
   10   IW(KOQVEC+I*KCQVEC+JQVESP) = I
C
      ELSE IF (CBANK .EQ. 'QDET')  THEN
        JOLD = KOQDET
        KOQDET = NBANK ('QDET', 0, N * KCQDET + LMHLEN) + LMHLEN
        IF (KOQDET .EQ. LMHLEN)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QDET')
        IW(KOQDET-1) = KCQDET
        IW(KOQDET) = N
        DO 20 I=KLUST+1,KFFRT-1
   20   IW(KOQVEC+I*KCQVEC+JQVEQD) =
     &    IW(KOQVEC+I*KCQVEC+JQVEQD) + KOQDET - JOLD
C
      ELSE IF (CBANK .EQ. 'QVRT')  THEN
        KQVRT = NBANK ('QVRT', 0, N * KCQVRT + LMHLEN)
        IF (KQVRT .EQ. 0)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QVRT')
        IW(KQVRT+1) = KCQVRT
        IW(KQVRT+2) = N
        KLFRV = N
        KOQVRT = KQVRT + LMHLEN - KCQVRT
C
      ELSE IF (CBANK .EQ. 'QPAR')  THEN
        KQPAR = NBANK ('QPAR', 0, N * KCQPAR + LMHLEN)
        IF (KQPAR .EQ. 0)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QPAR')
        IW(KQPAR+1) = KCQPAR
        IW(KQPAR+2) = N
        KOQPAR = KQPAR + LMHLEN - KCQPAR
        KOQPLI = NBANK ('QPLI', 0, N + LMHLEN) + LMHLEN
        IF (KOQPLI .EQ. LMHLEN)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QPLI')
        IW(KOQPLI-1) = 1
        IW(KOQPLI) = N
        KOQPBT = NBANK ('QPBT', 0, N + LMHLEN) + LMHLEN
        IF (KOQPBT .EQ. LMHLEN)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QPBT')
        IW(KOQPBT-1) = 1
        IW(KOQPBT) = N
C
      ELSE IF (CBANK .EQ. 'QPSP')  THEN
        LENPS = LMHLEN + N * ( JQPANA+1)
        KQPSP = NBANK ('QPSP', 0, LENPS )
        IF (KQPSP .EQ. 0)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QPSP')
        IW(KQPSP+1) = JQPANA + 1
        IW(KQPSP+2) = N
        CALL BKFMT ( 'QPSP' , '(I)' )
        NAQPSP=NAMIND('QPSP')
C
      ELSE IF (CBANK .EQ. 'QAFP')  THEN
        LENPS = LMHLEN + N * ( JQPANA+1)
        KQAFP = NBANK ('QAFP', 0, LENPS )
        IF (KQAFP .EQ. 0)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QAFP')
        IW(KQAFP+1) = JQPANA + 1
        IW(KQAFP+2) = NQAFP
        CALL BKFMT ( 'QAFP' , '(I)' )
        NAQAFP = NAMIND('QAFP')
C
      ELSE IF (CBANK .EQ. 'QTRA')  THEN
        KOQTRA = NBANK ('QTRA', 0, N + LMHLEN) + LMHLEN
        IF (KOQTRA .EQ. LMHLEN)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QTRA')
        IW(KOQTRA-1) = 1
        IW(KOQTRA) = N
C
      ELSE IF (CBANK .EQ. 'QFPA')  THEN
        KOQFPA = NBANK ('QFPA', 0, N * KCQFPA + LMHLEN)
        IF (KOQFPA .EQ. 0)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QFPA')
        IW(KOQFPA+1) = KCQFPA
        IW(KOQFPA+2) = N
        KOQFPA = KOQFPA + LMHLEN - KCQFPA
C
      ELSE IF (CBANK .EQ. 'QMTX')  THEN
        KOQMTL = NBANK ('QMTL', 0, N + LMHLEN) + LMHLEN
        KOQMTS = NBANK ('QMTS', 0, N + LMHLEN) + LMHLEN
        IF (KOQMTS .EQ. LMHLEN)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QMTS')
        IW(KOQMTL-1) = 1
        IW(KOQMTL) = N
        IW(KOQMTS-1) = 1
        IW(KOQMTS) = N
C
      ELSE IF (CBANK .EQ. 'QWRK')  THEN
C                       QWRK has NO mini header !
        KQWRK = NBANK ('QWRK', 0, N)
        IF (KQWRK .EQ. 0)
     +    CALL QMTERM ('0_QSBANK_ Insufficient space for bank QWRK')
      ENDIF
C
      END
#endif
