*DK vfndmc
      SUBROUTINE VFNDMC(IMOD,MAXC,NROS,MUXL,IPICH)
C----------------------------------------------------------------------
C! Get max number of R/O cahnnels, r/o pich and mux length
CKEY VDET DBASE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C! Input : IMOD module number
C! Output : MAXC  Number of r/o channels
C!          NROS  Number of r/o strips
C!          MUXL  Multiplexing length
C!          IPICH Readout pitch
C-----------------------------------------------------------------------
#ifndef DOC
      DIMENSION MAXC(*),MUXL(*),IPICH(*),NROS(2)
      INTEGER VNRWAF,VROSTM,VNSTCM
C
C Get  max number of readout channels in one module,
C and readout frequency ipich
C
      IBID = VROSTM(1,NROS1,RPICH,IPICH(1))
      IBID = VROSTM(2,NROS2,RPICH,IPICH(2))
      NROS(1)=NROS1
      NROS(2)=NROS2
      DO IV=1,2
         MAXC(IV)=VNSTCM(IV)
         MUXL(IV)=MAXC(IV)
      ENDDO
C      IF(VNRWAF().EQ.2)THEN
CC VDET 93
C         MAXC(1) = 2*NROS1
C         MUXL(1) = 10000
C      ENDIF
C      IF(VNRWAF().EQ.3)THEN
CC VDET 95
C         MAXC(1) = 3*NROS1/2
C         MUXL(1) = 1000
C      ENDIF
C      MUXL(2) = 10000
      RETURN
      END
#endif
