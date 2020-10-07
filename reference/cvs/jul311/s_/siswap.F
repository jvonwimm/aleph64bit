      INTEGER FUNCTION SISWAP(IMI,ISI,IPI,IRI,IMO,ISO,IPO,IRO)
C-------------------------------------------------------------
C!  - Unswap some cables for runs before 14780
C!  - Unswap some cables for runs in range [ 17537,17618]
C!
C!    Author B.Bloch-Devaux JUNE 92 ,November 92
C!    Input:  ADDRESS components as module , layer,phi,radius
C!                                  IMI       ISI  IPI  IRI
C!    Output  ADDRESS components as module , layer,phi,radius
C!                                  IMO       ISO  IPO  IRO
C!    Return value : SISWAP = 1 if swap was done
C!                            0 if no swap done
C!======================================================================
#ifndef DOC
                  SAVE
#include "rcurnt.h"
      IOK = 0
                  IMO = IMI
                  ISO = ISI
                  IPO = IPI
                  IRO = IRI
                  IF ( IRUNRC.LT. 2001) THEN
                  ELSE IF (IMI.EQ.2 .AND. IRUNRC.LT.14780) THEN
C   Phi 13-14 are swapped with phi 15-16  in Sical B
                    IF ( IPI.EQ.13 ) THEN
                      IPO= 15
                    ELSE IF ( IPI.EQ.14 ) THEN
                      IPO= 16
                    ELSE IF ( IPI.EQ.15 ) THEN
                      IPO= 13
                    ELSE IF ( IPI.EQ.16 ) THEN
                      IPO= 14
                    ENDIF
                  ELSEIF (IMI.EQ.1 .AND. IRUNRC.LE.17618 .AND. IRUNRC.
     &              GE.17537) THEN
C   Phi 17-18 are swapped with phi 21-22   in Sical A
                    IF ( IPI.EQ.17 ) THEN
                      IPO= 21
                    ELSE IF ( IPI.EQ.18 ) THEN
                      IPO= 22
                    ELSE IF ( IPI.EQ.21 ) THEN
                      IPO= 17
                    ELSE IF ( IPI.EQ.22 ) THEN
                      IPO= 18
                    ENDIF
                  ENDIF
                  IF (IPO.NE.IPI) IOK=1
                  SISWAP = IOK
   99             RETURN
      END
#endif
