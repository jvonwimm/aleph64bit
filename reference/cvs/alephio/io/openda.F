      LOGICAL FUNCTION OPENDA(LDBAS,IVERS,IDATE)
C.---------------------------------------------------------------------
CKEY IO OPEN DA
C  F.Ranjard - 880304     modified by M.Rumpf for APOLLO sequence
C! Open DAF
C  DAF length is 1024
C  Input : LDBAS logical unit number for DAF
C  Output: OPENDA is true if the file is found
C          IVERS   = DAF version version
C          IDATE   = DAF date of last change
C  Called by USER program before any use of the D.B.
C  Calls BDADF from Boslib
C  For APOLLO ,it is assumed that a LINK ADBSCONS.DAF
C  exists on the DAF.
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "adbsjj.h"
      PARAMETER (ILNDB=1024)
      LOGICAL LEX
#include "bmacro.h"
C --------------------------------------------------------------------
      LEX = .FALSE.
      IF (LDBAS .GT. 0) THEN
         LEX=.TRUE.
#ifndef ETA
         INQUIRE (UNIT=LDBAS,EXIST=LEX,RECL=LEN)
#endif
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
         OPEN (LDBAS,STATUS='OLD',ACCESS='DIRECT',RECL=ILNDB*4)
#endif
#endif
         IW(1)=2
         CALL BDABF(LDBAS,ILNDB,'    ','    ')
C
C -      get version # and date of last change of the DAF
         IND = MDARD (IW,LDBAS,'ADBS',0)
         IF (IND .EQ. 0) THEN
            IDATE = 0
            IVERS = 0
         ELSE
            IDATE = ITABL(IND,1,JADBDC)
            IVERS = ITABL(IND,1,JADBVN)
         ENDIF
         LOUT = IW(6)
         IF (LOUT .GT. 0) THEN
            WRITE (LOUT,*)' +++OPENDA+++ ADBSCONS DAF version ',IVERS,
     &      '   date of last change ',IDATE
         ENDIF
      ENDIF
C
      OPENDA = LEX
C
      END
#endif
