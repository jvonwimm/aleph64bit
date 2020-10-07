      SUBROUTINE GAMREAD(ICLUS,EMIGA,NNGA,GAMVE,IRTFG)
C---------------------------------------------------------------------
C! Interface for GAMPEX (use of the bank PGAC)
C
C  Patrick Janot -- 30 sept 1991, Modified for PGPC -- 15 April 1993
C                                 Modified for PGAC -- 30  oct  1994
C---------------------------------------------------------------------
#ifndef DOC
#include "parabank.h"
      DIMENSION gamve(20,20)
      DIMENSION gamce(20,20)
#include "qcde.h"
#include "qmacro.h"
C
      CALL vzero(gamve(1,1),200)
      CALL vzero(gamce(1,1),200)
      nnga = 0
      irtfg = 0
      ipgac = 0
      ipgac = NLINK('PGAC',0)
      IF ( ipgac .LE. 0 ) GOTO 999
C
C  Now (free) re-fills GAMVE
C
      ncol = IW(ipgac+lmhcol)
      nrow = IW(ipgac+lmhlen)
      DO 10 jpgac = 1 , nrow
        IF ( IW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaPE) .NE. iclus )
     .  GOTO 10
        nnga = nnga + 1
        IF ( nnga .GT. 20 ) THEN
          WRITE (IW(6),*) 'Warning!! More than 20 photons in cluster ',
     .               iclus
          nnga = 20
          GOTO 999
        ENDIF
        gamve( 1,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaEC)
        gamve( 2,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaTC)
        gamve( 3,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaPC)
        gamve( 4,nnga) = 1.
        gamve( 6,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaF4)
        gamve( 7,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaQ1)
        gamve( 8,nnga) = IW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaST)
        gamve( 9,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaR1)
        gamve(10,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaR2)
        gamve(11,nnga) = IW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaQU)/100
        gamve(12,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaER)
     .                 * RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaGC)
        gamve(13,nnga) = IW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaQU)
     .                 - IW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaQU)/2*2
        gamve(16,nnga) = RW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaDM)
        iovfl          = IW(ipgac+lmhlen+ncol*(jpgac-1)+jpgaQU)
     .                 - gamve(11,nnga)*100
     .                 - gamve(13,nnga)
        icrak          = gamve(13,nnga)
C
C Use the F4 technique only in for 'true' photons, and outside
C cracks and Endcap/Barrel overlaps.
C
        IF  ( gamve(6,nnga) .GT. 0.8 .AND.
     .        gamve(7,nnga) .LT. 1.5 ) THEN
C    .        gamve(7,nnga) .LT. 1.5 .AND.
C    .        icrak         .EQ. 0   .AND.
C    .        iovfl         .EQ. 0         ) THEN
            gamve(1,nnga) = gamve(1,nnga)*gamve(4,nnga)
C
        ELSE
C
C  Else use the raw energy corrected for leakage and saturation.
C
          gamve(1,nnga) = gamve(12,nnga)*gamve(4,nnga)
        ENDIF
   10 CONTINUE
C
 999  RETURN
      END
#endif
