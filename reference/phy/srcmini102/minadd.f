      SUBROUTINE MINADD
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Add other requested banks to Mini-DST list.
C
C     Author: Stephen Haywood      15-Mar-90
C     Modify: Stephen Haywood      08-Jun-90   v 3.0
C
C     Input  : MINA data card
C     Output : Extra names in MLISTE in / MINCOM /
C
C     Called by MINDST
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
      PARAMETER (MAX=100)
      CHARACTER*800 CLIST
      CHARACTER*4 ALIST,BANK,NLIST,CHAINT
      LOGICAL FIRST,NONE
      DIMENSION ALIST(MAX)
      SAVE FIRST,NONE,NADDE,ALIST
      DATA FIRST,NONE,NADDE / .TRUE.,.FALSE.,0 /
C
      IF(NONE) RETURN
C
C++   Initialisation.
C
      IF(FIRST) THEN
         FIRST = .FALSE.
C
C++      See if there are banks to be added on MINA card.
C
         KMINA = IW(NAMIND('MINA'))
         IF(KMINA.LE.0 .OR. IW(KMINA).LE.0) THEN
            NONE = .TRUE.
            RETURN
         ENDIF
         NADD = IW(KMINA)
         IF(NADD.GT.MAX) THEN
            WRITE(IW(6),'(//'' MINADD: Cannot add more than'',I3,
     &        '' banks with MINA card''//)') MAX
            NADD = MAX
         ENDIF
C
C++      Identify run banks on 'C' list and set up.
C
         CLIST = ' '
         DO 10 I=1,200
            BANK = NLIST(IW,I,'C')
            IF(BANK.EQ.' ') GOTO 15
            CLIST(4*I-3:4*I) = BANK
   10    CONTINUE
   15    CONTINUE
C
C++      Loop over banks to be added, and store as lists.
C
         DO 20 I=1,NADD
            BANK = CHAINT(IW(KMINA+I))
            IF(INDEX(CLIST,BANK).GT.0) THEN
               LAST = LENOCC(MLISTR)
               MLISTR(LAST+1:LAST+4) = BANK
            ELSE
               NADDE = NADDE + 1
               ALIST(NADDE) = CHAINT(IW(KMINA+I))
            ENDIF
   20    CONTINUE
      ENDIF
C
C++   Add banks to Mini-DST list.
C
      DO 100 I=1,NADDE
  100 IF(IW(NAMIND(ALIST(I))).GT.0) CALL MINLIS(ALIST(I))
C
      RETURN
      END
