      LOGICAL FUNCTION HPHNOI(IROW,FRAC)
C***********************************************************
C! Test if an element of PHCO contains a fraction          *
C! of noisy towers greater than FRAC                       *
C!                                                         *
C! Author: R.Tenchini           901130                     *
C!                                                         *
C! IROW  = row in HCLU bank                                *
C! FRAC  = cut on fraction of noisy towers.                *
C!                                                         *
C!                                                         *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hntojj.h"
#include "hsdajj.h"
#include "bmacro.h"
      HPHNOI=.FALSE.
      KHSDA = NLINK('HSDA',0)
      IF (KHSDA .EQ. 0) RETURN
      JHNTO = NAMIND ('HNTO')
      KHNTO = IW(JHNTO)
      IF (KHNTO .EQ. 0) RETURN
      NTOW=0
      NTNO=0
      DO 10 I=1,LROWS(KHSDA)
         IPHROW=ITABL(KHSDA,I,JHSDCN)
         IPHI  =ITABL(KHSDA,I,JHSDPI)
         ITHE  =ITABL(KHSDA,I,JHSDTI)
         IF(IPHROW.EQ.IROW) THEN
            NTOW=NTOW+1
            DO 20 J=1,LROWS(KHNTO)
               IADDHS=ITABL(KHNTO,J,JHNTAD)
               IPHIHS=IBITS(IADDHS,0,16)
               ITETHS=IBITS(IADDHS,16,8)
               IF(IPHI.EQ.IPHIHS.AND.ITHE.EQ.ITETHS) THEN
                  NTNO=NTNO+1
               ENDIF
 20         CONTINUE
         ENDIF
 10   CONTINUE
      IF(NTOW.EQ.0) RETURN
      RAT=FLOAT(NTNO)/FLOAT(NTOW)
      IF(RAT.GT.FRAC) HPHNOI=.TRUE.
      RETURN
      END
#endif
