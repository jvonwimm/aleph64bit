      SUBROUTINE MINNOF(MODE)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Ensure no formats are written for POT banks.
C
C     Author: Stephen Haywood      28-Feb-91
C
C     Input  : MODE   = -1 to delete links to formats
C                     = +1 to restore links to formats
C
C     Called by MINOUT.
C
C     The banks whose formats will be dropped are listed on 'T' list,
C     which is prepared by MINOUT.
C
C     This requires changes to BOS system common.
C     The formats are disabled by zeroing their address. However, these
C     are restablished by garbage collections in the work space, and so
C     must be done just before the banks are written.
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (MAXBNK=200,MAXCHA=4*MAXBNK)
      CHARACTER*800 MLISTE,MLISTR
      LOGICAL MINIMC,NOFORM
      COMMON / MINCOM / MLISTE,MLISTR,MINIMC,NOFORM
C
      COMMON / SYSBOS / NSYST,NAMES,NPRIM,IDNAM,IDPTR,IDFMT
      CHARACTER*4 NAME,NLIST
      DIMENSION KSAVE(MAXBNK)
      DATA KREF,KSHFT,KSAVE / 2*0,MAXBNK*0 /
      SAVE KREF,KSHFT,KSAVE
C
      IF (MODE.EQ.-1) CALL VZERO(KSAVE,MAXBNK)
C
C++   Look at location of format for a reference bank to detect
C++   shifts of formats resulting from garbage collection.
C++   RUNH is chosen since it does not share its root with other banks.
C
      IOFF = IW(IDNAM-2) + MOD(IABS(INTCHA('RUNH')),NPRIM) - 3
      IOFF = IW(IDPTR+IOFF)
      IF (MODE.EQ.-1) THEN
         KREF = IW(IDFMT+IOFF)
      ELSE
         KSHFT = IW(IDFMT+IOFF) - KREF
      ENDIF
C
C++   Loop over banks to be written out.
C
      DO 100 I=1,MAXBNK
         NAME = NLIST(IW,I,'T')
         IF (NAME.EQ.' ') GOTO 110
         IF (NAME(1:1).EQ.'D') GOTO 100
         IF (NAME(1:1).EQ.'d') GOTO 100
         INAME = INTCHA(NAME)
C
C++      Try to pick up pointer to bank format.
C++      The pointers are associated with the name and several may
C++      share the same root, therefore allow a small number of loops.
C++      If correct, IOFF should correspond to 'pos' in BOSIO table.
C
         IOFF = IW(IDNAM-2) + MOD(IABS(INAME),NPRIM) - 3
         LOOP = 0
   90    LOOP = LOOP + 1
            IF (LOOP.GT.6) GOTO 100
            IOFF = IW(IDPTR+IOFF)
            IF (IOFF.LE.0) GOTO 100
            IF (IW(IDNAM+IOFF).NE.INAME) GOTO 90
C
C++      Pick up pointer.
C
         IF (MODE.EQ.-1) THEN
            KFMT = IW(IDFMT+IOFF)
         ELSE
            IF (KSAVE(I).LE.0) GOTO 100
            KFMT = KSAVE(I) + KSHFT
         ENDIF
C
C++      Check we really have found correct format bank.
C
         IF (IW(KFMT-3).NE.INTCHA('+FMT')) GOTO 100
         IF (IW(KFMT+1).NE.INAME) GOTO 100
C
C++      Modify pointer: either zero it or restore it.
C
         IF (MODE.EQ.-1) THEN
            KSAVE(I) = KFMT
            IW(IDFMT+IOFF) = 0
         ELSE
            IW(IDFMT+IOFF) = KFMT
         ENDIF
  100 CONTINUE
  110 CONTINUE
C
      RETURN
      END
