      SUBROUTINE VDFSUP
C ----------------------------------------------------------------------
C
C!  Suppress the hits in dummy faces
C
C -  Author         Alain Bonissent, 22-October-1995
C    Modified       Manoj Thulasidas 15-May-1996
C             to change the subroutine name
CKEY V_DECK VDET
C
C  Input :  none
C
C  Output :  suppressed VDXY, VDZT
C
C ----------------------------------------------------------------------
#ifndef DOC
      INTEGER VNTFAC, VNRWAF, VNRMOD, VIFACI, VFACSN
      EXTERNAL VNTFAC, VNRWAF, VNRMOD, VIFACI, VFACSN
C
C       LOGICAL VSLOTF
      NFAC = VNTFAC()
      NWFF = VNRWAF()*VNRMOD()
      DO JFAC = 1,NFAC
        IF(VFACSN(JFAC).EQ.0)THEN
          IRET = VIFACI(JFAC,ILAY,IFAC)
          DO IWFF = 1,NWFF
            CALL VAENWA(IBNK,ILAY,IWFF,IFAC,1)
            CALL NDROP('VDXY',IBNK)
            CALL NDROP('VDZT',IBNK)
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END
#endif
