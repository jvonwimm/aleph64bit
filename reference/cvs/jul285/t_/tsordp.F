      SUBROUTINE TSORDP(LUNIT)
C
C---------------------------------------------------------------------
C! Print TPC start-of-run conditions
C
C      Author:   R. Johnson   27-06-88
C
C      Input:    LUNIT     /I     Fortran logical unit for output
C
C---------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      CHARACTER RNTYP*8,CHAINT*4
C
      KTSOR=IW(NAMIND('TSOR'))
      IF (KTSOR.EQ.0) THEN
C
C++     Sometimes the online folks forget the T in 'TSOR'
C
        KTSOR=IW(NAMIND(' SOR'))
      ENDIF
      IF (KTSOR.NE.0) THEN
        KRUNH=IW(NAMIND('RUNH'))
        IF (KRUNH.NE.0) THEN
          RNTYP(1:4)=CHAINT(IW(KRUNH+7))
          RNTYP(5:8)=CHAINT(IW(KRUNH+8))
        ELSE
          RNTYP(1:8)='        '
        ENDIF
        BF= FLOAT(IW(KTSOR+1))/10000.
        IFG=IW(KTSOR+2)
        SW= FLOAT(IW(KTSOR+3))/10.
        WG= FLOAT(IW(KTSOR+4))/10.
        EL= FLOAT(IW(KTSOR+5))/10.
        CC= FLOAT(IW(KTSOR+6))/100.
        WRITE(LUNIT,1) RNTYP,BF,IFG,SW,WG,EL,CC,IW(KTSOR+7),
     &                  IW(KTSOR+9),IW(KTSOR+10)
    1   FORMAT(//' TPC start of run conditions (bank TSOR):'/
     &           ' Trigger type=',T42,A/
     &           ' Magnetic field=',T40,F10.5,' kgauss'/
     &           ' Pulsed (1) or DC (0)=',T40,I10/
     &           ' Sense wire potential=',T40,F10.1,' V'/
     &           ' Gating grid potential=',T40,F10.1,' V'/
     &           ' Last electrode potential=',T40,F10.1,' V'/
     &           ' Resistor chain current=',T40,F10.2,' muA'/
     &           ' Potential wire potential=',T40,I10,' V'/
     &           ' Clock frequency=',T40,I10,' kHz'/
     &           ' Trigger repetition rate=',T40,I10,' Hz')
        AP= FLOAT(IW(KTSOR+17))/10.
        AT= FLOAT(IW(KTSOR+18))/10.
        GP= FLOAT(IW(KTSOR+19))/10.
        GT= FLOAT(IW(KTSOR+20))/10.
        WRITE(LUNIT,2) (IW(KTSOR+I),I=11,16),AP,AT,GP,GT
    2   FORMAT(' Level 1 scale factor=',T40,I10/
     &         ' Level 2 scale factor=',T40,I10/
     &         ' Level 1 delay=',T40,I10,' nsec'/
     &         ' Level 2 delay=',T40,I10,' nsec'/
     &         ' Ebx delay=',T40,I10,' nsec'/
     &         ' Laser delay=',T40,I10,' nsec'/
     &         ' Atmospheric pressure=',T40,F10.1,' hPa'/
     &         ' Atmospheric temperature=',T40,F10.1,' degrees'/
     &         ' Gas pressure=',T40,F10.1,' hPa'/
     &         ' Gas temperature=',T40,F10.1,' degrees'//)
      ENDIF
C
      RETURN
      END
#endif
