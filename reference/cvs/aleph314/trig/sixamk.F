      SUBROUTINE SIXAMK
C*********************************************************************
C                                                                    *
C   Author   :- Joe Boudreau          30-OCT-1991                    *
C                                                                    *
C! Build the SIXA bank for Sical trigger                             *
C                                                                    *
C   Inputs        :  SIFO bank                                       *
C   Outputs       :  SIXA bank                                       *
C                                                                    *
C*********************************************************************
#ifndef DOC
C
      SAVE
#include "bcs.h"
C
#include "x1namc.h"
#include "sitrco.h"
#include "sixajj.h"
#include "sifojj.h"
C
      PARAMETER ( IFUL = 4095)
      INTEGER KSIXA,KSIFO,ITRIP,KSIXB
      INTEGER JPNT,IADDR,I,IAND,MIN,ISHFT,IMD,IST,IPH,IRD,IRT
      INTEGER ISEG,IESEG,IPTY,IFE,KPNT
#include "bmacro.h"
      KSIFO = IW(NASIFO)
      IF (KSIFO.GT.0) THEN
C
C     Create the SIXA bank with fixed length , auxilliary bank SIXB
C
         LEN = LMHLEN + 16*LSIXAA
         CALL AUBOS('SIXA',0,LEN,KSIXA,IGARB)
         IF (KSIXA.LE.0) RETURN
         IW(KSIXA + LMHCOL) = LSIXAA
         IW(KSIXA + LMHROW) = 16
         LEN = LMHLEN + 32*LSIXAA
         CALL AUBOS('SIXB',0,LEN,KSIXB,IGARB)
         IF (KSIXB.LE.0) RETURN
         IW(KSIXB + LMHCOL) = LSIXAA*2
         IW(KSIXB + LMHROW) = 16
C
C   Loop over each row in the SIFO bank and each plane in the triplet
C   For each Amplex, find the two trigger segments which contain that
C   Amplex.Increment the energy in the SIXA bank by the amount of each
C   fast-or signal.
         DO 100 ITRIP = 1,LROWS(KSIFO)
            IADDR = ITABL(KSIFO,ITRIP,JSIFAD)
            DO 200 I = 1,3
                CALL SIDCOD(IADDR,I-1,IMD,IST,IPH,IRD)
                IF (IMD.LT.0.OR.IST.LT.0.OR.IPH.LT.0.OR.IRD.LT.0) RETURN
                IPTY = MOD(IST+1,2)+1
                DO 300 IRT = 1,2
                   ISEG=MOD((IPH-1)+2*(IRT-1),32)/4+8*(IMD-1)+1
                   JPNT = KROW(KSIXB,ISEG)+IRT+2*(IPTY-1)
                   IW(JPNT) = IW(JPNT)+ ITABL(KSIFO,ITRIP,JSIFA1+I-1)
 300            CONTINUE
 200        CONTINUE
 100     CONTINUE
C
C Now apply the conversion factor to the total signal  in both roads of
C each segment and fill SIXA in a compact format
         DO 400  ISEG = 1,16
         DO 400  IPTY = 1,2
         DO 400  IRT  = 1,2
            JPNT = KROW(KSIXB,ISEG)+IRT+2*(IPTY-1)
            IW(JPNT) = SIFOXA*IW(JPNT)
            IW(JPNT) = MIN(IW(JPNT),IFUL)
            KPNT = KROW(KSIXA,ISEG)+IPTY
            IESEG  = IW(JPNT)
            CALL MVBITS(IESEG,0,16,IW(KPNT),16*(IRT-1))
 400     CONTINUE
      ENDIF
C
      CALL BDROP(IW,'SIXB')
      RETURN
      END
#endif
