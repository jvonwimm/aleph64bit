      SUBROUTINE GETLUM(IRUN,IFOUN,IRQFL,NZ,NB,RLUMI,BK,BT)
C----------------------------------------------------------------------
C! Gets the luminosity and # of Z0s and Bhabhas for a run
C  Author  J.Boucrot  15-June-1990
C  Modified 28-Sep-1992 for runs with SICAL
C  Modified 19-May-1993 to get informations from Sical bank 'SLUM'
C  Modified 26-Aug-1997 to get lumi from 'LUMI' if it's 0. in 'RLUM'
CKEY ALEF LFIL LUMINOSITY
C
C Input   : IRUN    / INTE = run number
C
C Output  : IFOUN   / INTE = return code
C                            0   no information for run IRUN
C                            1   information from 'RLUM' bank
C                            2   information from the RUN Header banks
C                                JSUM and LUMI  ( LCAL)
C                            3   information from the RUN Header banks
C                                JSUM and SLUM  ( Sical )
C            The following arguments are defined only if IFOUN.GT.0  :
C           NZ      / INTE = number of Z0 --> Hadrons
C           NB      / INTE = number of Bhabhas
C           RLUMI   / REAL = Best estimate of Luminosity , in nb**-1
C             The following arguments are defined only if IFOUN=1
C           IRQFL   / INTE = Run_Quality flag for this run
C                     Bit 0 set = 'PERF'    1 = 'MAYB'     2 = 'DUCK'
C           BK      / REAL = Number of background Bhabha events
C           BT      / REAL = Bhabha trigger efficiency
C
C Description:
C  get the RLUM data base bank which contains the run IRUN
C  if the bank does not exist get the information from the SOR banks
C  JSUM and LUMI.
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "jsumjj.h"
#include "lumijj.h"
#include "rlumjj.h"
#include "slumjj.h"
      INTEGER ALGTRO
      DATA EPSIL / 0.001 /
C - runs with SiCAL > 16600
      DATA NRSIC / 16600 /
C - runs with LUMI from Sical only ( 1993 , 1994 and 1995 LEP I )
      DATA NR93  / 20000 /
C - High energy runs with LUMI from LCAL only :
      DATA NLP2  / 40000 /
#include "bmacro.h"
C----------------------------------------------------------------------
      IFOUN=0
      NZ=0
      NB=0
      IRQFL=0
      RLUMI=0.
      JRLUM = ALGTRO ('RLUM',IRUN,JRLURN,JROWR)
      IF (JRLUM.NE.0 .AND. JROWR.GT.0) THEN
C     RLUM bank found : get the output arguments
         JRLUM = IABS (JRLUM)
         IFOUN=1
         KRLUM=KROW(JRLUM,JROWR)
         IRQFL=IW(KRLUM+JRLURQ)
         NZ   = IW(KRLUM+JRLUNZ)
         NB   = IW(KRLUM+JRLUNB)
         RLUMI= RW(KRLUM+JRLULU)
         BK   = RW(KRLUM+JRLUBK)
         BT   = RW(KRLUM+JRLUBT)
      ENDIF
C  No 'RLUM' bank, or lumi=0. in RLUM
C  Try to find run header banks 'JSUM' , 'LUMI' , 'SLUM ' :
      IF (RLUMI.LT.EPSIL) THEN
         JJSUM = NLINK('JSUM',IRUN)
         IF (JJSUM.GT.0) THEN
            NZ = ITABL (JJSUM,1,JJSUVZ)
         ENDIF
C For runs before NR93 , or after NLP2 : take bank 'LUMI'
         IF (IRUN.LT.NR93.OR.IRUN.GT.NLP2) THEN
            JLUMI=NLINK('LUMI',IRUN)
            IF (JLUMI.EQ.0) GO TO 999
C Take method # 5  for LEP I runs without SICAL
C      method # 9  for LEP I runs with SICAL :
C      method # 10 for LEP II runs
            IMETH = 5
            IF ( IRUN.GT.NRSIC) IMETH = 9
            IF ( IRUN.GT.NLP2 ) IMETH = 10
            NB = RTABL (JLUMI,IMETH,JLUMB2)
            RLUMI = RTABL (JLUMI,IMETH,JLUMLU)
            IFOUN=2
C For runs after NR93 : take bank 'SLUM' , method 2 ,
C if not available try  'LUMI' , method 10 :
         ELSE
            JSLUM=NLINK('SLUM',IRUN)
            IF (JSLUM.GT.0) THEN
               IMETH=2
               NB = RTABL (JSLUM,IMETH,JSLUTI)
               RLUMI = RTABL (JSLUM,IMETH,JSLULU)
               IFOUN=3
            ELSE
               JLUMI=NLINK('LUMI',IRUN)
               IF (JLUMI.EQ.0) GO TO 999
               IMETH=10
               NB = RTABL (JLUMI,IMETH,JLUMB2)
               RLUMI = RTABL (JLUMI,IMETH,JLUMLU)
               IFOUN=2
            ENDIF
         ENDIF
      ENDIF
C
 999  RETURN
      END
#endif
