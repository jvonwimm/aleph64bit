      PARAMETER (MAXBNK=200,MAXCHA=4*MAXBNK)
      CHARACTER*800 MLISTE,MLISTR
      LOGICAL MINIMC,NOFORM
      COMMON / MINCOM / MLISTE,MLISTR,MINIMC,NOFORM
#if defined(DOC)
C!    Mini-DST bank list.
C
C     MAXBNK = maximum number of banks in string MLISTE
C     MAXCHA = maximum number of characters in string MLISTE
C     MLISTE = list of Mini-DST banks created for an event
C     MLISTR = list of run banks to be written for Mini-DST
C     MINIMC = (logical) MC banks will be written on Mini-DST
C     NOFORM = (logical) Formats will not be written on Mini-DST
#endif
