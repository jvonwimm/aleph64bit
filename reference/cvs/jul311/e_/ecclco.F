      SUBROUTINE ECCLCO
C***********************************************************************
C!Close EcObject banks                                                 *
C!                                                                     *
C!  Adjust the size of each bank to the data which it contains         *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870819                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ECOB,EBOS,EHYP,EPAR                                    *
C!    OUTPUT  : ECOB,EBOS,EHYP,EPAR                                    *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
C   Number of useful data words in bank at index id
      NDATA(ID)=LMHLEN+IW(ID+LMHCOL)*IW(ID+LMHLEN)
C
      KECOB=IW(NAECOB)
      ND=NDATA(KECOB)
      CALL AUBOS('ECOB',0,ND,KECOB,IGARB)
C
      KEBOS=IW(NAEBOS)
      ND=NDATA(KEBOS)
      CALL AUBOS('EBOS',0,ND,KEBOS,IGARB)
C
      KEHYP=IW(NAEHYP)
      ND=NDATA(KEHYP)
      CALL AUBOS('EHYP',0,ND,KEHYP,IGARB)
C
      KEPAR=IW(NAEPAR)
      ND=NDATA(KEPAR)
      CALL AUBOS('EPAR',0,ND,KEPAR,IGARB)
      RETURN
      END
#endif
