      SUBROUTINE VDB2DD(D2)
C ---------------------------------------------------------------
C! Give position of VDB2 box, relative to VDBX
CKEY VDET GEOM
C! Author : A. Bonissent  15/08/94
C! Temporary routine, should be replaced by something more appropriate
C ---------------------------------------------------------------
      INTEGER VNRWAF
      IF(VNRWAF().EQ.2)D2 = -0.311785
C For new Vdet, it is 150 mu of silicon (1/2 wafer)
C                plus 100 mu of carbon fibre (1/2 omega)
C                = 0.025 cm
      IF(VNRWAF().EQ.3)D2 = -0.025
C For test purpose, we now put it to the same value
      D2 = -0.311785
      RETURN
      END
