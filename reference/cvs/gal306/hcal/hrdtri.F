*DK hrdtri
      SUBROUTINE HRDTRI (LDBAS,IRUN,IFLAG)
C -------------------------------------------------------------------
C - G.Catanesi - 871007 -                    F.Ranjard - 880203
C! load /HCTRIG/ with DB trigger banks
C- input arguments:
C        LDBAS    : data base logical unit#
C        IRUN     : run number
C - output argument:
C        IFLAG     : return flag ( = ALGTDB(LDBAS,list,IRUN) )
C
C -----------------------------------------------------
#ifndef DOC
#include "hctrig.h"
#include "hjjpar.h"
#include "bcs.h"
      INTEGER ALGTDB
      EXTERNAL ALGTDB
      CHARACTER*12 LISTT
      DATA LISTT /'HCTGHSBAHSEC'/
#include "bmacro.h"
C -----------------------------------------------------------------
C
C  If the trigger constants are required loads the banks
C    from the D.A file ADBSCOMB.DAF (unit 4) to the
C            HCTRIG common
C
      IFLAG=ALGTDB(LDBAS,LISTT,IRUN)
      IF(IFLAG.EQ.0)RETURN
C
C      Get index for Trigger banks
C
      JHCTG = IW(NAMIND( 'HCTG'))
      JHSBA = IW(NAMIND( 'HSBA'))
      JHSEC = IW(NAMIND( 'HSEC'))
C
      IF(JHCTG.NE.0)THEN
C
C  Store the content of the HCTG bank
C
         NHCEPR = ITABL(JHCTG,1,JHCTEP)
         NHCBPR = ITABL(JHCTG,1,JHCTBP)
         NHCETR = ITABL(JHCTG,1,JHCTET)
         NHCBTR = ITABL(JHCTG,1,JHCTBT)
C
         KHCTG = JHCTG + LMHLEN + JHCTTM - 1
         DO 10 J=1,LHCRT
            IHCTRG(J) = IW(KHCTG+1)
            NHCBTS(J) = IW(KHCTG+2)
            NHCETS(J) = IW(KHCTG+3)
            KHCTG = KHCTG + 3
   10    CONTINUE
C
C
C  Trigger quantities used in Galeph
C
         MHCETR = (NHCEPR-1) * NHCETR
         MHCBTR = (NHCBPR-1) * NHCBTR
C
      ENDIF
C
C Store the content of a part of HSBA bank
C
      IF(JHSBA.NE.0)THEN
C
         LHSBA = LCOLS(JHSBA)
         NHSBA = LROWS(JHSBA)
         DO 20 J=1,NHSBA
            IYHCFI(J) = ITABL(JHSBA,J,JHSBEN)
   20    CONTINUE
C
      ENDIF
C
C Store the content of a part of HSEC bank
C
      IF(JHSEC.NE.0)THEN
C
         LHSEC = LCOLS(JHSEC)
         NHSEC = LROWS(JHSEC)
         DO 30 J=1,NHSEC
            IXHCFI(J) = ITABL(JHSEC,J,JHSEFN)
            IXHCSE(J) = ITABL(JHSEC,J,JHSESN)
   30    CONTINUE
C
      ENDIF
      END
#endif
