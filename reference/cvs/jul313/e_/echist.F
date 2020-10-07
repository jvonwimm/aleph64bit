      SUBROUTINE ECHIST
C----------------------------------------------------------------------
C!  - FILLS ECAL HISTOGRAMS
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "ecnamc.h"
#include "etp1jj.h"
#include "etc2jj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "esdajj.h"
#include "eclujj.h"
#include "eneutw.h"
#include "ecobjj.h"
#include "ehypjj.h"
#include "bmacro.h"
C
        NHECA = JULEC * 1000
        ESTO = 0.
        ECLT = 0.
        ETRA = 0.
        ECLA = 0.
C? STOREYS NUMBER
        KESDA = IW(NAESDA)
        IF (KESDA .EQ. 0) GOTO 999
        NMNT = IW (KESDA+LMHROW)
      CALL HFILL(NHECA+1,FLOAT(NMNT),0.,1.)
C? STOREYS ENERGY
        DO 10 I=1,NMNT
        ESTO = ESTO + RW(KESDA+LMHLEN+LESDAA*(I-1)+JESDME)
 10     CONTINUE
      CALL HFILL(NHECA+2,ESTO,0.,1.)
C? CLUSTERS NUMBER
        KECLU = IW(NAECLU)
       IF (KECLU .EQ. 0) GOTO 999
        NECL  = IW(KECLU+LMHROW)
      CALL HFILL(NHECA+3,FLOAT(NECL),0.,1.)
C? CLUSTERS ENERGY
        DO 11 I=1,NECL
        ECLT = ECLT + RW(KECLU+LMHLEN+LECLUA*(I-1)+JECLE4)
 11     CONTINUE
      CALL HFILL(NHECA+4,ECLT,0.,1.)
C? GET A CLUSTER AND FIND IF  TRACKS ARE ASSOCIATED
        KETP1 = IW(NAETP1)
       IF (KETP1 .EQ.0) GOTO 999
        NTRAK = IW(KETP1+LMHROW)
        KECT1 = IW(NAECT1)
        KERL3 = IW(NAERL3)
        ECLA = 0.
        DO 12 I=1,NECL
        NR   = IW(KECT1+LMHLEN+LECT1A*(I-1)+JECTER)
        IF (NR .EQ. 0) GOTO 12
        ICLU = IW(KERL3+LMHLEN+LERL3A*(NR-1)+JERLEC)
        ECLA = ECLA + RW(KECLU+LMHLEN+LECLUA*(ICLU-1)+JECLE4)
 12     CONTINUE
        CALL HFILL(NHECA+5,ECLA,0.,1.)
C
 999    CONTINUE
        CALL ECHIBA
        RETURN
        END
#endif
