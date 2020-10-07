      SUBROUTINE X2PRIN (TFLAG)
C ---------------------------------------------------------
C - F.Ranjard - 890530
C! Print various quantities from level 2 trigger
CKEY TRIGGER PRINT LEVEL2 / USER
C
C - Input   : TFLAG / CHAR*4 = 'INIT'  initialization
C                              'EVEN'  event printout
C
#ifndef DOC
      SAVE
      CHARACTER*(*) TFLAG
#include "bcs.h"
#include "x2cons.h"
C --------------------------------------------------------
      IF (IW(6) .EQ. 0) RETURN
C
C - print initial values
C
C Now try and print out the constants that were just read in.
C
      IF (TFLAG .EQ. 'INIT') THEN
        WRITE(IW(6),7040)ZACPMM,IX2PRL,ADVELO,TDVELO,IGNPAD,
     +                    NTBINS,CLOCKR,IRWDTH,
     +                    ((IPADPR(IO,IPAIR),IO=1,2),IPAIR=1,ICHX),
     +                    ITHRSH
C
      ELSEIF (TFLAG .EQ. 'EVEN') THEN
C
C - print event results
C
C       pretty print mask.
C
        WRITE(IW(6),9311)IX2MSK(40),IX2MSK(39),IX2MSK(28),IX2MSK(27),
     +    IX2MSK(16),IX2MSK(15),IX2MSK(56),IX2MSK(50),IX2MSK(41),
     +    IX2MSK(38),IX2MSK(29),IX2MSK(26),IX2MSK(17),
     +    IX2MSK(14),IX2MSK(8),IX2MSK(2),IX2MSK(57),IX2MSK(55),
     +    IX2MSK(51),IX2MSK(49),IX2MSK(42),IX2MSK(37),IX2MSK(30),
     +    IX2MSK(25),IX2MSK(18),IX2MSK(13),IX2MSK(9),IX2MSK(7),
     +    IX2MSK(3),IX2MSK(1),IX2MSK(58),IX2MSK(60),IX2MSK(52),
     +    IX2MSK(54),IX2MSK(43),IX2MSK(48),IX2MSK(31),IX2MSK(36),
     +    IX2MSK(19),IX2MSK(24),IX2MSK(10),IX2MSK(12),IX2MSK(4),
     +    IX2MSK(6),IX2MSK(59),IX2MSK(53),IX2MSK(44),IX2MSK(47),
     +    IX2MSK(32),IX2MSK(35),IX2MSK(20),IX2MSK(23),IX2MSK(11),
     +    IX2MSK(5),IX2MSK(45),IX2MSK(46),IX2MSK(33),IX2MSK(34),
     +    IX2MSK(21),IX2MSK(22)
C
C       Print level 3 bos banks.
C
        CALL X2PRHI
C
      ENDIF
C
 7040 FORMAT(/1X,'+++X2PRIN+++   Second level trigger conditions*****'/
     +   4X, 'Z-acceptance     ',F10.4,' Print level ',I7,/,
     +   4X, 'Drift velocities ',2(F10.4,2X),/,
     +   4X, 'Ignored Pads     ',3I3,'  Theta bins ',2I3,/,
     +   4X, 'Clockrate        ',F10.4,' Roads data ',2I3,/,
     +   4X, 'Padpairs         ',8(I2,1X),/,
     +   4X, 'Chosen Threshs   ',8(I2,1X),/,
     +   4X, '*****************************************'/)
C
 9311 FORMAT(/,' +++X2PRIN+++  Level 2 track mask.',/,/,
     + 23X,2(2I1,8X),2I1,/,
     + 5X,I1,8X,I1,7X,I1,2(2X,I1,6X,I1),2X,I1,7X,I1,8X,I1,/,
     + 2(3X,I1),4X,I1,3X,I1,7(4X,I1),3X,I1,4X,I1,3X,I1,/,
     + 2(3X,I1),4X,I1,3X,I1,7(4X,I1),3X,I1,4X,I1,3X,I1,/,
     + 5X,I1,8X,I1,7X,I1,2(2X,I1,6X,I1),2X,I1,7X,I1,8X,I1,/,
     + 23X,2(2I1,8X),2I1,/,
     + 2X,'154-165  165-141   141-120   120-60     60-39'
     +      ,4X,'39-26    26-15  <-Theta',/,
     + 6X,'Endcap -Z',12X,'B  A  R  R  E  L',11X
     +      ,'Endcap +Z',/,/)
C
      END
#endif
