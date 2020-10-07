      SUBROUTINE MINRUN
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Define run banks to be written for Mini-DST.
C
C     Author: Stephen Haywood      15-May-90
C     Modify: Stephen Haywood      08-Jun-90   v 3.0
C     Modify: Agnieszka Jacholkowska  1-Nov-1994
C
C     Called by MINDST
C     More banks may be added from MINA card, interpreted by MINADD.
C     Also the nature of the data (MC or real) is determined.
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
      DIMENSION IPV(10),IAV(10)
      CHARACTER*4 BANK,NLIST
C
C++   Determine whether this is a Monte-Carlo file.
C++   Whether MC banks are written can be overridden by the NOMC card.
C
      CALL ALVSN(ITYP,IPV,IAV,IYR)
      IF (IPV(1).GT.0) THEN
         MINIMC = .TRUE.
      ELSE
         MINIMC = .FALSE.
      ENDIF
C
      IF(NLINK('NOMC',0).GT.0) MINIMC = .FALSE.
      IF(NLINK('NNMC',0).GT.0) MINIMC = .FALSE.
C
C++   Define banks on run record.
C++   For MC, copy C list.
C
      IF(MINIMC) THEN
         MLISTR = ' '
         DO 10 I=1,50
            BANK = NLIST(IW,I,'C')
            IF(BANK.EQ.' ') GOTO 15
   10    MLISTR(4*I-3:4*I) = BANK
   15    CONTINUE
      ELSE
         MLISTR = 'RUNR'//'RUNH'//'RALE'//'RLEP'//'RHAH'//'JSUM'//'SLUM'
     &          //'LEHI'//'LUMI'//'LBAK'//'LVHI'//'XTOP'//'XTYP'//'XTBN'
     &           //'ALPB'//'PLSC'//'TSSR'//'BLQP'
      ENDIF
      IF (INDEX(MLISTR,'DQIV').EQ.0)
     +    MLISTR=MLISTR(1:LNBLNK(MLISTR))//'DQIV'
C
      RETURN
      END
