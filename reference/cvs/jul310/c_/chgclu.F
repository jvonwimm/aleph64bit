      SUBROUTINE CHGCLU(ITR,ICLUH,NCLUH,MXCLU,IER)
C***********************************************************************
C! Go from Trk to  Hclu (TRUE)                                         *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HTTD,HTHT                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Itr     : track number                                            *
C!   ICLUH   : Table of HCLUS                                          *
C!   NCLUH   : Number of HCLUS                                         *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
      DIMENSION ICLUH(*)
#include "bmacro.h"
      KHTT1=NLINK('HTTD',1)
      KHTT2=NLINK('HTTD',2)
      KHTT3=NLINK('HTTD',3)
      KHTT4=NLINK('HTTD',4)
      KHTHT=NLINK('HTHT',0)
C
C?  get track position in banks etht
C
      NTRD=LROWS(KHTT1)
      DO 10 I=1,NTRD
      IF(ITABL(KHTT1,I,1).EQ.ITR)THEN
         ITK=I
         GO TO 11
      ENDIF
   10 CONTINUE
C
C?  If no track number matches, exit
C
      GO TO 999
   11 CONTINUE
C
C?  get  Number of digitizings
C
      NDIG=ITABL(KHTT2,ITK,1)
C
C?  get number of first digitz
C
      IFDIG=ITABL(KHTT3,ITK,1)
C
C?  loop on towers digitz
C
      NCLUS=0
      DO 110 IDIG=1,NDIG
      ICDIG=IFDIG+IDIG
      IDG=ITABL(KHTT4,ICDIG,1)
      ITWA=ITABL(KHTHT,IDG,1)
      CALL CHSCLU(ITWA,ICLU)
      IF(ICLU.NE.0)THEN
         DO 112 IB=1,NCLUH
         IF(ICLU.EQ.ICLUH(IB))GO TO 110
  112    CONTINUE
         IF(NCLUH.GE.MXCLU)GO TO 998
         NCLUH=NCLUH+1
         ICLUH(NCLUH)=ICLU
      ENDIF
  110 CONTINUE
  999 CONTINUE
      IER=0
      RETURN
  998 CALL RERROR('CHGCLU',1,' MORE THAN  MXCLU CLUSTERS')
      END
#endif
