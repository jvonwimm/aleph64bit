      SUBROUTINE ECRNDX
C
C***********************************************************************
C                                                                      *
C!CREATE EC NAME INDICES      FOR BANKS                                *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  861001                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C!                                                                     *
C!**********************************************************************
#ifndef DOC
C
      EXTERNAL NAMIND
#include "bcs.h"
#include "ecnamc.h"
C
C  CREATE NAME INDICES
C
      NAESDA=NAMIND('ESDA')
      NAECLU=NAMIND('ECLU')
      NAETP1=NAMIND('ETP1')
      NAEFET=NAMIND('EFET')
      NAECT1=NAMIND('ECT1')
      NAETC2=NAMIND('ETC2')
      NAERL3=NAMIND('ERL3')
      NAETDI=NAMIND('ETDI')
      NAESTO=NAMIND('ESTO')
      NAETCK=NAMIND('ETCK')
      NAECOB=NAMIND('ECOB')
      NAEBOS=NAMIND('EBOS')
      NAEHYP=NAMIND('EHYP')
      NAEPAR=NAMIND('EPAR')
      NAEIDT=NAMIND('EIDT')
      NAEIGA=NAMIND('EIGA')
      NAEMIP=NAMIND('EMIP')
      NAEIBR=NAMIND('EIBR')
      NAEGMA=NAMIND('EGMA')
      NAEDST=NAMIND('EDST')
C
C
      RETURN
      END
#endif
