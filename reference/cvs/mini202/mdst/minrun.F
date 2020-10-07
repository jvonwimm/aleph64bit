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
#ifndef DOC
C
#include "bcs.h"
#include "mincom.h"
C
      DIMENSION IPV(10),IAV(10)
      CHARACTER*4 BANK,NLIST
      INTEGER IDTYP
C
C++   Determine whether this is a LEP1 or LEP2 data
C 
      IDTYP = 0      
      CALL MINDTYP(IDTYP)
      IF (IDTYP.GT.0) THEN
         WRITE(6,'(//40X,''**** Mini-DST LEP'',I2,'' ****'')')
     &   IDTYP
      ELSE
         WRITE(6,'(//40X,''**** Mini-DST Unkown Type'',I2,'' ****'')')
     &   IDTYP
      ENDIF
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
      ELSEIF (IDTYP.EQ.2) THEN
         MLISTR = 'RUNR'//'RUNH'//'RALE'//'RLEP'//'RHAH'//'JSUM'//'SLUM'
     &          //'LEHI'//'LUMI'//'LBAK'//'LVHI'//'XTOP'//'XTYP'//'XTBN'
     &          //'ALPB'//'PLSC'//'TSSR'//'BLQP'//'X1TT'//'X3PS'//'KXME'
     &          //'PHHY'//'PCHY'//'PCPA'//'HT0C'//'HCLB'//'BCGN'
      ELSEIF (IDTYP.LE.1) THEN
         MLISTR = 'RUNR'//'RUNH'//'RALE'//'RLEP'//'RHAH'//'JSUM'//'SLUM'
     &          //'LEHI'//'LUMI'//'LBAK'//'LVHI'//'XTOP'//'XTYP'//'XTBN'
     &          //'ALPB'//'PLSC'//'TSSR'//'BLQP'//'X1TT'//'X3PS'//'KXME'
      ENDIF
      IF (INDEX(MLISTR,'DQIV').EQ.0)
     +    MLISTR=MLISTR(1:LNBLNK(MLISTR))//'DQIV'
C
      RETURN
      END
#endif
