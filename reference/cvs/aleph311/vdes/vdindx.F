      SUBROUTINE VDINDX(IINDEX,IINDXY,NWAFER,ILAYND,IPHIND,IWAFND)
C
C!  Builds the 'standard' wafer indexes
C   No inputs
C   Outputs;
C      IINDEX = Unique consecutive integer for each physical wafer
C      NWAFER = Bank number (vaenwa output) given IINDEX
C      IINDXY = Bank number for r-phi side, compresses wafers 1+2,3+4
C      ILAYND = Layer of this IINDEX
C      IPHIND = phi   of this IINDEX
C      IWAFND = wafer of this IINDEX
C
#ifndef DOC
      SAVE
C     2  = number of layers
C     4  = number of wafers in a face
C     12 = maximum number of faces in layer#1
C     15 = maximum number of faces in layer#2
C
      INTEGER MAXIND
      PARAMETER (MAXIND = 12*4+15*4)
      INTEGER IINDEX(2,4,15),IINDXY(MAXIND),NWAFER(MAXIND)
      INTEGER ILAYND(MAXIND),IPHIND(MAXIND),IWAFND(MAXIND)
C
C  Local variables
C
      INTEGER IND,ILAY,IWAF,IPHI
      INTEGER NPHI(2)
      DATA NPHI/12,15/
C
C  Loop over all wafers, and calculate the indicies
C
      IND = 1
      DO ILAY=1,2
        DO IPHI=1,NPHI(ILAY)
          DO IWAF=1,4
            IINDEX(ILAY,IWAF,IPHI)=IND
            CALL VAENWA(NWAFER(IND),ILAY,IWAF,IPHI,1)
            IF(IWAF .EQ. 2)THEN
              IINDXY(IND) = IND-1
            ELSE IF(IWAF .EQ. 3)THEN
              IINDXY(IND) = IND+1
            ELSE
              IINDXY(IND) = IND
            END IF
            ILAYND(IND) = ILAY
            IWAFND(IND) = IWAF
            IPHIND(IND) = IPHI
            IND = IND + 1
          END DO
        END DO
      END DO
      RETURN
      END
#endif
