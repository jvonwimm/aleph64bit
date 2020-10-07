      LOGICAL FUNCTION OPENDB(LDBAS)
C.---------------------------------------------------------------------
CKEY IO OPEN DAF
C  F.Ranjard - 880304     modified by M.Rumpf for APOLLO sequence
C! Open DAF
C  DAF length is 1024
C  Input : LDBAS logical unit number for DAF
C  Output: OPENDB is true if the file is found
C  Called by USER program before any use of the D.B.
C  Calls BDADF from Boslib
C  For APOLLO ,it is assumed that a LINK ADBSCONS.DAF
C  exists on the DAF.
C.---------------------------------------------------------------------
#ifndef DOC
#include "albitw.h"
#include "bcs.h"
      PARAMETER (ILNDB=1024)
      LOGICAL LEX
C --------------------------------------------------------------------
      LEX = .FALSE.
      IF (LDBAS .GT. 0) THEN
         LEX=.TRUE.
         INQUIRE (UNIT=LDBAS,EXIST=LEX,RECL=LEN)
      ENDIF
C
      IF (LEX) THEN
#if defined(ALEPH_DEC) || defined(ALEPH_SGI)
         OPEN (LDBAS,STATUS='OLD',ACCESS='DIRECT',RECL=ILNDB,READONLY)
#else
#if defined(APOLLO) || defined(ALEPH_HP)
         OPEN (LDBAS,STATUS='OLD',ACCESS='DIRECT',RECL=ILNDB,
     &         FILE='~/adbscons.daf')
#else
         OPEN (LDBAS,STATUS='OLD',ACCESS='DIRECT',RECL=ILNDB*LCHAR)
#endif
#endif
         IW(1)=2
         CALL BDABF(LDBAS,ILNDB,'    ','    ')
      ENDIF
      OPENDB = LEX
C
      END
#endif
