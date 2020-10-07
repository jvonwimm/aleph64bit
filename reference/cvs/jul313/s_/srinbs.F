      SUBROUTINE SRINBS
C----------------------------------------------------------------------
C! SATR bank name indices
C!
C!    Author:     H. Meinhard       17-Aug-1987
C!    Modified:   H. Meinhard       02-May-1991  (7)
C!
C!    Description
C!    ===========
C!    Set name indices and bank formats for SATR named banks, reset
C!    work bank indices
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "psnamc.h"
#include "sanamc.h"
#include "swrkbk.h"
#include "bosext.h"
C----------------------------------------------------------------------
C? set name indices
      NASRAD = NAMIND('SRAD')
      NASADI = NAMIND('SADI')
      NASRTD = NAMIND('SRTD')
      NASCOO = NAMIND('SCOO')
      NASCRP = NAMIND('SCRP')
      NASSCP = NAMIND('SSCP')
      NASCLU = NAMIND('SCLU')
      NASCCP = NAMIND('SCCP')
      NASSBP = NAMIND('SSBP')
      NASPAT = NAMIND('SPAT')
      NASPCP = NAMIND('SPCP')
      NASSPP = NAMIND('SSPP')
      NASWPA = NAMIND('SWPA')
      NASKAN = NAMIND('SKAN')
      NASKCP = NAMIND('SKCP')
      NASSKP = NAMIND('SSKP')
      NASTRK = NAMIND('STRK')
      NASTCP = NAMIND('STCP')
      NASSTP = NAMIND('SSTP')
      NASUPA = NAMIND('SUPA')
      NASUCP = NAMIND('SUCP')
      NASSUP = NAMIND('SSUP')
      NASACO = NAMIND('SACO')
      NASFTR = NAMIND('SFTR')
      NASWTU = NAMIND('SWTU')
      NASHOT = NAMIND('SHOT')
      NAPSCO = NAMIND('PSCO')
      NAPSPO = NAMIND('PSPO')
      NAPSTR = NAMIND('PSTR')
C
C? reset work bank indices
      KWK1SW = 0
      KWK2SW = 0
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
