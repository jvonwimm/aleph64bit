      SUBROUTINE MINHEA
C-----------------------------------------------------------------------
C! Fill header bank DHEA for Mini-DST.
C
CKEY MDST /INTERNAL
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PYER,FRFT,YV0V,PEWI banks
C     Output : DHEA bank
C
C     The energy flow information is filled by calling FIDHEA later.
C     If this routine is not called, the information will be zero.
C     FIDHEA is written by Minard and Pepe and lives in Julia.
C     Also the ALEPHLIB routine ALFIEL is called to obtain the B-field.
C     UFITQL is called to identify good tracks.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "frftjj.h"
#include "lolejj.h"
#include "pewijj.h"
#include "dheajj.h"
C
      INTEGER UFITQL
      DATA EWMIN / 0.500 /
C
#include "bmacro.h"
C
C++   Create the DHEA bank.
C
      LEN = LMHLEN + LDHEAA
      CALL AUBOS('DHEA',0,LEN, KDHEA,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINHEA: Cannot create DHEA bank'')')
         RETURN
      ENDIF
      IW(KDHEA+LMHCOL) = LDHEAA
      IW(KDHEA+LMHROW) = 1
C
C++   Physics flag.
C
      IW(KROW(KDHEA,1)+JDHEFP) = 0
C
C++   Find the number of vertices and the number of V0's.
C
      KPYER = NLINK('PYER',0)
      IF(KPYER.GT.0) THEN
         NPYER = LROWS(KPYER)
      ELSE
         NPYER = 0
      ENDIF
C
      KYV0V = NLINK('YV0V',0)
      IF(KYV0V.GT.0) THEN
         NYV0V = LROWS(KYV0V)
      ELSE
         NYV0V = 0
      ENDIF
C
      IW(KROW(KDHEA,1)+JDHENX) = NPYER + NYV0V
      IW(KROW(KDHEA,1)+JDHENV) = NYV0V
C
C++   Count the number of positive/negative charged tracks.
C++   A check is made on track quality: only tracks from origin or V's
C++   with momentum less than Ebeam are counted.
C
      KFRFT = NLINK('FRFT',0)
      IF(KFRFT.GT.0) THEN
         NFRFT = LROWS(KFRFT)
      ELSE
         NFRFT = 0
      ENDIF
C
      NPOS = 0
      NNEG = 0
      DO 100 I=1,NFRFT
         IQUAL = UFITQL(I)
         IF(IQUAL.NE.1 .AND. IQUAL.NE.3) GOTO 100
         IF(RTABL(KFRFT,I,JFRFIR).GT.0.) THEN
            NPOS = NPOS + 1
         ELSE
            NNEG = NNEG + 1
         ENDIF
  100 CONTINUE
C
C++   The charge determination relies on knowing the B-field direction.
C
      BSIGN = - ALFIEL(DUMMY)
      IF(BSIGN.GT.0) THEN
         IW(KROW(KDHEA,1)+JDHENP) = NPOS
         IW(KROW(KDHEA,1)+JDHENM) = NNEG
      ELSE
         IW(KROW(KDHEA,1)+JDHENP) = NNEG
         IW(KROW(KDHEA,1)+JDHENM) = NPOS
      ENDIF
C
C++   Flag for reconstruction status.
C++   As a temporary measure, fill the LOLE error flag.
C
      KLOLE = NLINK('LOLE',0)
      IF(KLOLE.GT.0) THEN
         IFLAG = ITABL(KLOLE,1,JLOLER)
      ELSE
         IFLAG = 0
      ENDIF
      IW(KROW(KDHEA,1)+JDHERS) = IFLAG
C
C++   Ecal wire t0 (energy weighted over modules).
C
      KPEWI = NLINK('PEWI',0)
      IF ( KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF(KPEWI.GT.0) THEN
         NPEWI = LROWS(KPEWI)
         SUME = 0.
         SUMT = 0.
         DO 200 I=1,NPEWI
            KEV = 0
            DO 210 IPLAN=1,45
  210       KEV = KEV + ITABL(KPEWI,I,JPEWPD-1+IPLAN)
            EGEV = FLOAT(KEV) / 1000000.
            IF(EGEV.LT.EWMIN) GOTO 200
            TIME = FLOAT(ITABL(KPEWI,I,JPEWTI))
            TIME = ITABL(KPEWI,I,LCOLS(KPEWI))
            SUME = SUME + EGEV
            SUMT = SUMT + EGEV * TIME
  200    CONTINUE
         IF(SUME.GT.0.) THEN
            TIME = SUMT / SUME
         ELSE
            TIME = 0.
         ENDIF
         IW(KROW(KDHEA,1)+JDHEWT) = NINT(TIME)
      ENDIF
C
      RETURN
      END
#endif
