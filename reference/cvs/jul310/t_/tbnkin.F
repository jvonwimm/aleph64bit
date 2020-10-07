      SUBROUTINE TBNKIN
C
C-----------------------------------------------------------------------
C! Initialize TPC name indices
C!
C!    Author:   R. Johnson   16-12-86
C!    Modified: R. Johnson   13-10-87
C!
C! Called by TINIJO
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "tpnamc.h"
C
      NATRCL=NAMIND('TRCL')
      NATMCL=NAMIND('TMCL')
      NATPHE=NAMIND('TPHE')
      NATPHT=NAMIND('TPHT')
      NATPTE=NAMIND('TPTE')
      NATTHE=NAMIND('TTHE')
      NATTHT=NAMIND('TTHT')
      NATGMA=NAMIND('TGMA')
      NATMTL=NAMIND('TMTL')
      NATBCO=NAMIND('TBCO')
      NATGTL=NAMIND('TGTL')
      NATGCL=NAMIND('TGCL')
      NATTRC=NAMIND('TTRC')
      NATCAL=NAMIND('TCAL')
      NATTCC=NAMIND('TTCC')
      NATPCH=NAMIND('TPCH')
      NATCLU=NAMIND('TCLU')
      NATLRL=NAMIND('TLRL')
      NATSRL=NAMIND('TSRL')
      NATCRL=NAMIND('TCRL')
      NATPAD=NAMIND('TPAD')
      NATPCO=NAMIND('TPCO')
      NATPDI=NAMIND('TPDI')
      NATPRL=NAMIND('TPRL')
      NATWDI=NAMIND('TWDI')
      NATPUL=NAMIND('TPUL')
      NATRFS=NAMIND('TRFS')
      NATWIR=NAMIND('TWIR')
      NATWPU=NAMIND('TWPU')
      NATCHA=NAMIND('TCHA')
      NATCTC=NAMIND('TCTC')
      NATLNK=NAMIND('TLNK')
      NATISL=NAMIND('TISL')
      NATALN=NAMIND('TALN')
      NATCON=NAMIND('TCON')
      NATCRC=NAMIND('TCRC')
      NATLCT=NAMIND('TLCT')
      NATCCN=NAMIND('TCCN')
      NATGFT=NAMIND('TGFT')
      NATPRD=NAMIND('TPRD')
      NATSCL=NAMIND('TSCL')
      NATSPU=NAMIND('TSPU')
      NATWTA=NAMIND('TWTA')
      NATELS=NAMIND('TELS')
      NATEXS=NAMIND('TEXS')
C
      RETURN
      END
#endif
