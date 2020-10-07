      SUBROUTINE YSTVTX(IXHX,PAR1,PAR2,IFLAG,ISTO,VTX,IBVD,LDMP)
C----------------------------------------------------------*
C!    Find correct circle crossing to start V0 vertex fit
CKEY YTOP
C!    Author :     W. Manner 25/01/93
C!
C!    Description
C!    ===========
C!     For tracks IXHX(1) IXHX(2) track parameters PAR1(5) PAR2(5)
C!     look if tracks have hits in front of vertex. Check that vertex
C      is behind beam crossing
C      IFLAG = 0   OK
C      IFLAG = 1   no vertex found
C      ISTO   no of vertices found and stored in VTX(3,I),I=1,ISTO
C----------------------------------------------------------*
#ifndef DOC
      SAVE
      INTEGER IXHX(*)
      LOGICAL LDMP
      REAL PAR1(*),PAR2(*),CORA(3,2),VTX(3,2),ZCO2(2)
      INTEGER IBMVD(8,2),IBMIT(29,2),IHI(2),IBVD(2)
C --------------------------------------------------------
      KBIT=-1
      ISTO=0
      CALL YBIPAT(IXHX(1),IBVD(1),IBITPC,
     +    IBMVD(1,1),IBMIT(1,1),IBMIT(9,1),IFLAG,LDMP)
      CALL YBIPAT(IXHX(2),IBVD(2),IBITPC,
     +    IBMVD(1,2),IBMIT(1,2),IBMIT(9,2),IFLAG,LDMP)
C Look at both circle crossings
  200 CONTINUE
      CALL YCIRCR(PAR1,PAR2,
     &  NCRO,NPHYS,CORA,ZCO2)
      IF(LDMP)
     +  PRINT *,'NPHYS,CORA',NPHYS,CORA,' ZCO2',ZCO2
      IFLAG=1
      DO 400 IPHYS=1,2
        IF(IAND(NPHYS,IPHYS).EQ.0) GO TO 400
        IF(ABS(CORA(3,IPHYS)-ZCO2(IPHYS)).GT.5.) GO TO 400
        CALL YPRVER(CORA(1,IPHYS),KBIT,LDMP)
C Do nothing with VDET hits
        IF(KBIT.LE.3) GO TO 300
Count ITC/TPC bits in front of vertex
        DO IT=1,2
          IHI(IT)=0
          DO IH = 1,KBIT-3
            IF(IBMIT(IH,IT).EQ.1) IHI(IT)=IHI(IT)+1
          ENDDO
        ENDDO
        IFLAK=0
        IF((IHI(1).GT.2.OR.IHI(2).GT.2) .AND.
     +     IHI(1)+IHI(2).GT.3) IFLAK=1
        IF(LDMP)
     +    PRINT *,'VTX ACC FLAG',IFLAK
        IF(IFLAK.EQ.1) GO TO 400
  300   CONTINUE
        IFLAG=0
        ISTO=ISTO+1
        VTX(1,ISTO)=CORA(1,IPHYS)
        VTX(2,ISTO)=CORA(2,IPHYS)
        VTX(3,ISTO)=CORA(3,IPHYS)
  400 CONTINUE
      END
#endif
