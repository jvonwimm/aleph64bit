      SUBROUTINE ALDTYP(ILEV)
C ----------------------------------------------------------------------
C! Finds the category of data : KIN,RAW,POT,DST,MDST
C Author     J. Boucrot     26-AUG-1988
CKEY ALEF DATA TYPE / USER
C Calls  ALGTYP                                      from  ALEPHLIB
C Input bank : RHAH (or RHOH)  or  AJOB  or  KRUN  or RUNH
C Output argument :
C    ILEV = 1 : KINGAL data
C         = 2 :  RAW           = 3     POT      = 4     DST
C         = 5 :  MINI          = 6    MICRO     = 7     NANO
C  No other value is allowed for Aleph data  ( ILEV =0 )
C  For JULIA outputs , this routine works properly only for files
C  created with versions 2.19 onwards .
C----------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rhahjj.h"
       INTEGER ALGTYP
#include "bmacro.h"
C ----------------------------------------------------------------------
         ILEV=0
C Is it a KINGAL output file ? It must have a KJOB bank and no GALEPH tracking :
         ITST=ALGTYP(IGEAN,ISIMD,IFAST,ITPCS)
         IKJO=IW(NAMIND('KJOB'))
         IF (ITST.EQ.0.AND.IKJO.GT.0) THEN
            NADAT=1
            GO TO 300
         ENDIF
C Is it a NANODST ?  there must be a 'NDST' bank :
         INANO=IW(NAMIND('NDST'))
         IF (INANO.EQ.0) GO TO 30
         NADAT=7
         GO TO 300
C Is it a Julia output file ? : the RHAH bank should exist
 30      IADR=IW(NAMIND('RHAH'))
         IF (IADR.EQ.0) GO TO 50
         NATOU=ITABL(IADR,LROWS(IADR),JRHANO)
         NADAT=MOD(NATOU,10)
         NATIN=ITABL(IADR,LROWS(IADR),JRHANI)
         NADIN=MOD(NATIN,10)
         IF (NADIN.GT.NADAT) NADAT=NADIN
         IF (NADAT.LT.1.OR.NADAT.GT.5) NADAT=0
         IF (NADAT.GT.0) GO TO 300
C This file may be a JULIA output without RUNH :
 50      NADAT=0
         IDHEA=IW(NAMIND('DHEA'))
         IF (IDHEA.GT.0) NADAT=3
         IF (NADAT.NE.0) GO TO 300
C Is it a Galeph output file ?
 100     IF (ITST.EQ.0) GO TO 250
         NADAT=2
         IF (ISIMD.NE.0) NADAT = 4
         GO TO 300
C
C Last chance : is it a RAW data tape/file ?
C
 250     IF (NLINK('RUNR',0).EQ.0) GO TO 300
         NADAT=2
C
 300     ILEV=NADAT
C
 999     RETURN
         END
#endif
