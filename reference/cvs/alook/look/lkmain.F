      PROGRAM LKMAIN
C ----------------------------------------------------------
C! program to look at banks
C     Author : A.McSporran         VAX version
C     Author : G. Bagliesi 890307  IBM version
C
C     Modified by D.Harvatis          11-AUG-89
C                 G.Bagliesi          21-SET-89
C
C     Maintained by F.Ranjard
C     This version can use Xwindows on ALWS
C ----------------------------------------------------------
#ifndef DOC
      PARAMETER (LBNAM=3000, LBCS=500000)
      COMMON /BCS/ IW(LBCS)
#include "lkevt.h"
#include "lkfil.h"
#include "lklast.h"
#include "version.h"
C ---------------------------------------------------------
C     initialize time on vax, do-nothing routine in batch and on
C     other machines
      CALL TIMEST (1000000.)
C     terminal output unit
      LOUT = 6
      CALL LKSTOUT (6)
      IW(6) = 0
      CALL ALVERS (ALVER)
      WRITE(LOUT,8000)IVERS ,IDATE, ALVER
 8000 FORMAT (/5X,'LOOK program version ',I2, ' released on ',I6
     &        ,' linked with ALEPHLIB ',F6.2)
C
      CALL BNAMES(LBNAM)
      CALL BOS(IW,LBCS)
C
C     BOS output unit
      IW(6) = LOUT
C     number of messages to be printed
      IW(7)=10000
C     number of banks to be printed
      IW(8)=10000
C     terminal logical unit
      LTERM = 5
C     data card logical unit
      LCARD = 7
      IW(5) = LCARD
C     output data card file (CARD 72 col.)
      LFILE(1) = 11
C     print file unit
      LFILE(2) = 12
C     output file (EPIO or NATIVE)
      LFILE(3) = 13
C     BOS format unit
      LUFMT = 8
C     data base unit
      LDBAS = JUNIDB(0)
#if defined(IBM)
C     IBM terminal unit
      LTERM = 50
      CALL VMCMS('FILEDEF 50 TERM (BLOCK 80',IRC)
#endif
C
      CALL LKINIT
      WRITE(LOUT,*) '[Type "HELP" for command list]'
      LBNKNM=' '
      IVOULU=1
      CALL LKLOOK
      END
#endif
