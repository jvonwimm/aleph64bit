      SUBROUTINE ECCHKB
C*********************************************************************
C! CHECK IF ANY BAD  CHANNEL WAS CONSIDERED AMONG ESDA BANK
C!
C!  AUTHOR  :  C. BENCHOUK 890529
C!  MODIFIED:
C!
C!  BANKS:
C!    INPUT  : ESDA,ESDB
C!    OUTPUT : NONE
C!    CREATED: NONE
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "eclcom.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "edstjj.h"
#include "eclujj.h"
C
C
C --- NUMBER OF STOREYS PER STACK AND NUMBER PER BANANA
      NPST=73728
      NPBAN=6144
C
      KESDA=NLINK('ESDA',0)
      KEDST=NLINK('EDST',0)
C
      IF(KEDST.EQ.0) GOTO 901
C
C --- LOOP ON BAD CHANNELS . IN THIS METHOD PUT THE VALUE OF THE
C --- ENERGY TO 0.001 MEV
C
      NDBAD=IW(KEDST+LMHROW)
      INDB= KEDST+LMHLEN
C
      DO 10 IRB=1,NDBAD
      INDSB=INDB+(IRB-1)*LEDSTA
      IW(INDSB+JEDSES)=0
      IADD=IW(INDSB+JEDSIO)
C
C --- GET STOREY NUMBER. CHECK IF IT IS IN IARRAY
C
      K=(IADD-1)/NPST
      IKST=IADD-K*NPST
      IBAN=(IKST-1)/NPBAN
      IKBA=IKST-IBAN*NPBAN
      KIB=IARRAY(IKBA)
C
      IF(KIB.EQ.0) GOTO 10
      ITOW=IW(KEWLOC+KIB+IBAN)
C
C
      IF(ITOW.EQ.0) GOTO 10
C
C --- MODIFY THE ENRGY AND FLAG THE STOREY IN THE TWO BANKS
C
      ISTO=ITOW+K
      INDENR=KESDA+LMHLEN+(ISTO-1)*LESDAA
      RW(INDENR+JESDME)=ENDEAD
      IW(INDENR+JESDED)=IRB
      IW(INDSB+JEDSES)=ISTO
C
C
C
 10   CONTINUE
      RETURN
C
 901  RETURN
      END
#endif
