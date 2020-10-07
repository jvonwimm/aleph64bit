      SUBROUTINE MINIJO
C--------------------------------------------------------------------
C! Initialise the MUON job
C
C! G. Capon    13-aug-87
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "munamc.h"
#include "mujosu.h"
#include "muocut.h"
#include "rlunit.h"
#include "mdeach.h"
      DIMENSION KSUBN(5),NSLO1(5),NSLO2(5),IOFFS(3)
      DATA IWARN/0/
      DATA KSUBN/ 1, 2, 2, 3, 3/
      DATA NSLO1/ 1, 1, 9, 1,20/
      DATA NSLO2/34, 8,16,19,38/
      DATA IOFFS/0,34,50/
#include "bmacro.h"
C
C                reset variables for job summary statistics
C
      NNMUDI=0
      NCLTOT=0
      NCLMAX=-1
      NTRTST=0
      NTRASS=0
C
C               fill flags for dead/missing elements
C
C               1 - missing subcomponents
C
      DO 21 KSUB=1,3
      DO 21 KSLOT=1,38
 21   MUCHMB(KSUB,KSLOT)=1
C           2 - missing slots
C                  3 - missing astros, iastr=100/170
      DO 14 N=1,36
 14   MASTRO(N)=1
C                  4 - missing buses (SGS boards)
      DO 30 N=1,36
 30   MUOBUS(N)=0
C
C               set cuts for muon identification routines
C
C changed from 3 to 2.2 on 5/10/92 by G.Taylor
C to help reduce the amount of shadowing
      PCUTMA=2.2
      PCUTHA=1.5
      ACCLAY(1)=4.
      ACCLAY(2)=4.
      ACCANG=10.
      ROADWD=3.
      TETASS=10.
      NEXNFO=10
      NLAS10=5
      NMINFP=3
      WCLUMX=4.
      NMCUT=NAMIND('MCUT')
      JMCUT=IW(NMCUT)
      IF (JMCUT.GT.0) THEN
         PCUTMA=RW(JMCUT+1)
         PCUTHA=RW(JMCUT+2)
         ACCLAY(1)=RW(JMCUT+3)
         ACCLAY(2)=RW(JMCUT+4)
         ACCANG=RW(JMCUT+5)
         ROADWD=RW(JMCUT+6)
         TETASS=RW(JMCUT+7)
         NEXNFO=IW(JMCUT+8)
         NLAS10=IW(JMCUT+9)
         NMINFP=IW(JMCUT+10)
         WCLUMX=RW(JMCUT+11)
      ENDIF
      IF(IWARN.LT.3) THEN
       IWARN=IWARN+1
      WRITE(LDEBRL,79)PCUTMA,PCUTHA,ACCLAY,ACCANG,ROADWD,TETASS,
     + NEXNFO,NLAS10,NMINFP,WCLUMX
      ENDIF
 79   FORMAT(' MINIJO - cuts for muon identification in Common MUOCUT',
     +/,' pcutma,...,wclumx =',7F6.2,3I5,F6.2)
C
C                   initialise MUON banks
C           MUDI = raw data bank - MHIT = reconstructed hits bank
C           MCAD = Muon Chambers Association Data bank
C
        NAMUDI=NAMIND('MUDI')
        NAMHIT=NAMIND('MHIT')
        NAMCAD=NAMIND('MCAD')
  999 CONTINUE
      RETURN
      END
#endif
