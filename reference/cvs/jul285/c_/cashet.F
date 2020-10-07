      SUBROUTINE CASHET(IER)
C***********************************************************************
C!associate hc, ec and tpc                                             *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870505                                    *
C!  MODIFIED : A. BONISSENT  901206                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1,EFET                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
#include "etp1jj.h"
#include "efetjj.h"
#include "ecnamc.h"
#include "bcs.h"
#include "eidtjj.h"
      COMMON/KLOCAL/KIFELC
      DATA IFRST/1/
      DATA PMIP/0.500/
#include "bmacro.h"
      CALL CINIRL(IER)
      IF (IER.NE.0)GO TO 998
      KETP1=IW(NAETP1)
      IF(KETP1.EQ.0)GO TO 999
      KEFET=IW(NAEFET)
      NTRK=LROWS(KEFET)
      IF(IFRST.EQ.1)THEN
         KIFELC=0
         IFRST=0
      ENDIF
      KEHEX=IW(NAMIND('EHEX'))
      IF(KEHEX.NE.0) THEN
C
C?      Build table of electrons
C
      CALL WBANK(IW,KIFELC,NTRK,*997)
      KEIDT=IW(NAEIDT)
C
C!  If bank EIDT exists, look at tracks; else, no electrons
C
      IF(KEIDT.NE.0)THEN
         NEIDT=LROWS(KEIDT)
         DO 20 IEIDT=1,NEIDT
           ITK=ITABL(KEIDT,IEIDT,JEIDFR)
           IW(KIFELC+ITK)=ITABL(KEIDT,IEIDT,JEIDIP)
   20    CONTINUE
      ENDIF
      ENDIF
      DO 100 ITR=1,NTRK
C      WRITE(6,*)' TRACE :',ITR
C
C?  GET TRACK MOMENTUM
C
      PTR=RTABL(KETP1,ITR,JETPPT)
C
C?   Select tracks with momentum larger then pmip
C
      IF(PTR.LT.PMIP)GO TO 100
C
C    Get extrap. flag
C
      IEFL=ITABL(KEFET,ITR,JEFEEF)
      IF(IEFL.NE.1)GO TO 100
C
C
C?  See if particle is an electron
C?  do not extrapolate electrons
C
      IF(KEHEX.NE.0)THEN
          IF(IW(KIFELC+ITR).EQ.1)GO TO 100
      ENDIF
C?  See if track is mip
C
      CALL CTRMIP(ITR,IFMIP)
C
C? Say that we did not find any hclu associated
C
      IFND=0
      IF(IFMIP.EQ.0)THEN
         CALL CASNMP(ITR,IFND)
      ENDIF
      CALL CASMP(ITR,IFND)
  100 CONTINUE
C
C?  now, associate neutral ecobs to hclu
C
      CALL CASNEU
      CALL CCLORL('CRL3')
      CALL WDROP(IW,KIFELC)
  999 RETURN
  998 CONTINUE
      CALL RERROR('CASHET',1,' Error from CINIRL')
      RETURN
  997 CONTINUE
      CALL RERROR('CASHET',2,' Too little space for work bank')
      IER=1
      RETURN
      END
#endif
