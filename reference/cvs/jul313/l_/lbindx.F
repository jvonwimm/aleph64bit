      SUBROUTINE LBINDX
C----------------------------------------------------------------------
C!   Set Lcal name indices and printing formats
C!
C!   Author   : P. H. Hansen  870401
C!   Modified : P. H. Hansen  890102
C!   Modified : H. Meinhard   16-Mar-1990
C!   Modified : H. Meinhard   24-Apr-1991
C!   Modified : P. H. Hansen  16-Jun-1997  Add BHZ0 bank index
C!
C!   Description :
C!   ===========
C?
C!=====================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
      EXTERNAL NAMIND
C----------------------------------------------------------------------
C Input banks (S list)
      NALTDI = NAMIND('LTDI')
      NALWDI = NAMIND('LWDI')
C JULIA banks (T list)
      NALSDA = NAMIND('LSDA')
      NALPDA = NAMIND('LPDA')
      NALCLU = NAMIND('LCLU')
      NALOBJ = NAMIND('LOBJ')
      NALIFL = NAMIND('LIFL')
      NALOCL = NAMIND('LOCL')
C Temporary banks
      NALTRK = NAMIND('LTRK')
      NALMAP = NAMIND('LMAP')
      NALSIN = NAMIND('LSIN')
C POT banks (E list)
      NALIDT = NAMIND('LIDT')
      NAPLSD = NAMIND('PLSD')
      NAPLPD = NAMIND('PLPD')
C Run end record  (permanent, but reset each run)
      NALUMI = NAMIND('LUMI')
      NALBAK = NAMIND('LBAK')
      NALONL = NAMIND('LONL')
      NALEHI = NAMIND('LEHI')
      NALVHI = NAMIND('LVHI')
C Job end record  (permanent)
      NALSTA = NAMIND('LSTA')
C Reconstruction constants (permanent)
      NALCRE = NAMIND('LCRE')
      NABHAB = NAMIND('BHAB')
      NABHZ0 = NAMIND('BHZ0')
      NALACC = NAMIND('LACC')
      NALECA = NAMIND('LECA')
      NALDST = NAMIND('LDST')
      NALDWP = NAMIND('LDWP')
C Geometry constants (permanent)
      NALALI = NAMIND('LALI')
      NALCAL = NAMIND('LCAL')
      NALLAY = NAMIND('LLAY')
      NALMTY = NAMIND('LMTY')
      NALPMO = NAMIND('LPMO')
      NALRWG = NAMIND('LRWG')
      NALSCO = NAMIND('LSCO')
      NALSLO = NAMIND('LSLO')
      NALWRG = NAMIND('LWRG')
      NALCPG = NAMIND('LCPG')
      END
#endif
