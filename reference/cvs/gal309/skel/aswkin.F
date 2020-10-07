*DK aswkin
      SUBROUTINE ASWKIN
C ---------------------------------------------------------
C - F.RANJARD - 860308
C! Interface Geant3/BOS kine banks
C - Translate GEANT banks : 'VERT' and 'KINE' into similar
C   BOS banks.
C - Called by    ASEVST                       from this .HLB
C - Calls        NBANK, BLIST, NAMIND, BKFMT  from BOS77.hlb
C                ALBOS                        from this .HLB
C ----------------------------------------------------------
#ifndef DOC
#include "jqcom.h"
#include "kincom.h"
#include "gcbank.h"
#include "gcnum.h"
      SAVE AVER, IPROG
C - def. of 'VERT' parameters : header length, # of parameters
      PARAMETER (LHVE=3, LPVE=5, LEVE=LHVE+LPVE)
C - def. of 'KINE' parameters : header length, # of parameters
      PARAMETER (LHKI=3, LPKI=5, LEKI=LHKI+LPKI)
      DATA IFI/0/
#include "bmacro.h"
#include "kmacro.h"
      NUMGP(JD) = INT (GB(JD+5))
C ----------------------------------------------------------
C
C - get the alephlib version # used in KINGAL or GALEPH
      IF (IFI .EQ. 0) THEN
         IFI = 1
         CALL KIAVER (AVER,IPROG)
      ENDIF
C
C - get the PART bank
      JPART = IW(NAPART)
C
C - Translate GEANT banks 'VERT' and 'KINE' into BOS banks
C
C                                    'VERT' banks
      DO 1 I=1,NGVERT
         JV = LGB(JGVERT-I)
         IF (JV.EQ.0) GOTO 1
         NOUT = GB(JV+7)
         CALL ALBOS ('VERT',I,LEVE+NOUT,KVE,IGARB)
         IW(KVE+1) = LHVE
         IW(KVE+2) = LPVE
         IW(KVE+3) = NOUT
         KV1 = KVE + LHVE
         RW(KV1+1) = GB(JV+1)
         RW(KV1+2) = GB(JV+2)
         RW(KV1+3) = GB(JV+3)
         RW(KV1+4) = GB(JV+4)
         IW(KV1+LPVE) = GB(JV+5)
         DO 11 N=1,NOUT
            IW(KV1+LPVE+N) = GB(JV+7+N)
 11      CONTINUE
 1    CONTINUE
C                                      'KINE'
      DO 2 I=1,NGTRAC
         JK = LGB(JGKINE-I)
C        IF the Geant# .EQ. the NOtracking marker word THEN skip it
         NGP = NUMGP(JK)
         IF (NGP .EQ. NOTRKI) GOTO 2
C
         IF (JK.LE.0) GOTO 2
         NVX = GB(JK+7)+1
         CALL ALBOS ('KINE',I,LEKI+NVX,KKI,IGARB)
         IW(KKI+1) = LHKI
         IW(KKI+2) = LPKI
         IW(KKI+3) = NVX
         KK1 = KKI+LHKI
         RW(KK1+1) = GB(JK+1)
         RW(KK1+2) = GB(JK+2)
         RW(KK1+3) = GB(JK+3)
         IW(KK1+5) = NGP
C        If the Geant# > 52 then Geant# is not = Aleph#
         IF (NGP.GT.52) THEN
           DO M=1,LROWS(JPART)
            IF (NGP.EQ.ITABL(JPART,M,1)) IW(KK1+5) = M          
           ENDDO
         ENDIF
C        In the ALEPHLIB 9.0 the KINE and FKIN banks have been modified:
C        word(4) has been changed to contain mass instead of energy.
C        to be backward compatible the following test is necessary.
C        when files created with an ALEPHLIB earlier than 9.0 will have
C        disappeared only the statment IF(I.GT.NITRKI.. will be necessary.
         IF (AVER .LT. 9.0) THEN
            RW(KK1+4) = GB(JK+4)
         ELSE
            IF(I .GT. NITRKI) THEN
               RW(KK1+4) = PARMAS(IW(KK1+5))
            ENDIF
         ENDIF
         IW(KK1+6) = GB(JK+6)
         DO 21 N=2,NVX
            IW(KK1+LPKI+N) = GB(JK+6+N)
 21      CONTINUE
 2    CONTINUE
C
 999  CONTINUE
      RETURN
      END
#endif
