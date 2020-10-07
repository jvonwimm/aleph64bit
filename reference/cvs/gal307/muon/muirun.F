*DK muirun
      SUBROUTINE MUIRUN
C ----------------------------------------------------
C - F.Ranjard - 860624
C             modified for DAF and new bank layout
C             by F.Bossi,D.Kuhn,R.Xu 87-09-05
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C - Initialize MUON BOS banks and MUON common blocks
C - Called from   ASIPAC                      from this .HLB
C - Calls         NAMIND, BKFMT               from BOS77 lib
C -----------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "munamc.h"
#include "mugncn.h"
#include "mug1pr.h"
#include "mustat.h"
#include "mjjpar.h"
        INTEGER  ALGTDB
        EXTERNAL ALGTDB
      CHARACTER*52 LIST
      DATA LIST /'MUOGMBAGMBBGMBSGMBTGMECGMEBGMETGMESGMMAGMMBGMMTGMMSG'/
#include "bmacro.h"
C ------------------------------------------------------
      IF (IDETJO(8).GT.0) THEN
      NAMUDG = NAMIND ('MUDG')
      NAMUHT = NAMIND ('MUHT')
      NAMUDT = NAMIND ('MUDT')
      NAMUTD = NAMIND ('MUTD')
C
C - set bank format
C
      CALL BKFMT ('MUTD','I')
      CALL BKFMT ('MUHT','I')
      CALL BKFMT ('MUDG','I')
      CALL BKFMT ('MUDT','I')
      ENDIF
      JDMUST = 0
      JDMTHT = 0
C - set flag for NEW geometry
      ICMUJO(5) = 1
C
C       For the genaral muon detector information, fill the common block
C       only once.
C
        IRET = ALGTDB (LRDBIO,LIST,IRUNJO)
        IF (IRET.EQ.0) CALL ALTELL ('MUON data base banks missing',0,
     &    'STOP')
C
        NAMUOG = NAMIND('MUOG')
        NAMBAG = NAMIND('MBAG')
        NAMBBG = NAMIND('MBBG')
        NAMBSG = NAMIND('MBSG')
        NAMBTG = NAMIND('MBTG')
c
        NAMECG = NAMIND('MECG')
        NAMEBG = NAMIND('MEBG')
        NAMETG = NAMIND('METG')
        NAMESG = NAMIND('MESG')
C
        NAMMAG = NAMIND('MMAG')
        NAMMBG = NAMIND('MMBG')
        NAMMTG = NAMIND('MMTG')
        NAMMSG = NAMIND('MMSG')
C
#include "muiwna.h"
        WDEIMU = RTABL(JMUOG,1,JMUOWI)
        WDTBMU = RTABL(JMUOG,1,JMUOTU)
        OFTBMU = 0.5*RTABL(JMUOG,1,JMUOSP)
        WDATMU = RTABL(JMUOG,1,JMUOAC)
        HTATMU = RTABL(JMUOG,1,JMUOAC)
        HFHTMU = 0.5 * HTATMU
        SGLSMU = RTABL(JMUOG,1,JMUOSE)
        PSCRMU = RTABL(JMUOG,1,JMUOET)
        DSSPMU = RTABL(JMUOG,1,JMUODI)
        PTSPMU = RTABL(JMUOG,1,JMUODE) + DSSPMU
        PTXSMU = RTABL(JMUOG,1,JMUOXS)
        SNSXMU(1) = RTABL(JMUOG,1,JMUOX1)
        SNSXMU(2) = RTABL(JMUOG,1,JMUOX2)
        SNSXMU(3) = RTABL(JMUOG,1,JMUOX3)
        SNSXMU(4) = RTABL(JMUOG,1,JMUOX4)
        PTYSMU = RTABL(JMUOG,1,JMUOYS)
        SNSYMU(1) = RTABL(JMUOG,1,JMUOY1)
        SNSYMU(2) = RTABL(JMUOG,1,JMUOY2)
        SNSYMU(3) = RTABL(JMUOG,1,JMUOY3)
        SNSYMU(4) = RTABL(JMUOG,1,JMUOY4)
C
C - set the matrix to find address of a module in
C   geometric BOS bank by means of the slot#, the volume#
C   and the 3rd level volume name
C
 888  CONTINUE
      CALL VZERO(MMADPR(1,1),48)
        MMMNSL = ITABL(JMMAG,1,JMMANS)
        DO 10   IADD=1,MMMNSL
        ISLT = ITABL(JMMSG,IADD,JMMSVO)
      IF( IADD .LE. NMMIN )THEN
         ISET = 1
      ELSE IF( IADD .LE. NMMA )THEN
         ISET = 2
      ELSE IF( IADD .LE. NMMBI )THEN
         ISET = 3
      ELSE
         ISET = 4
      ENDIF
      MMADPR(ISLT,ISET) = IADD
   10 CONTINUE
C
C       Initialize statistics for MUON
C
      NEVMUH = 0
      NTMCLU = 0
      NMXCLU = 0
C
      END
#endif
