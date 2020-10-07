      FUNCTION VDMSRL(U,W,IWAFID)
C!----------------------------------------------------------------
C!  Get the radiation lenght for a given point on a vdet face
CKEY VDET TRACK
C!
C!  Author         G.Taylor    29/9/92
C!
C!  Input : U       VDET local coordinaate in FACE reference frame
C!          W       VDET local coordinaate in FACE reference frame
C!          IWAFID  wafer identifier of the VDET module ( as VDCO)
C!
C!  Output : VDMSRL fraction of radiation lenght at that point
C!
C!----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JVDRUL=1,JVDRUH=2,JVDRWL=3,JVDRWH=4,JVDRRL=5,JVDRIG=6,
     +          LVDRTA=6)
      INTEGER GTSTUP, ALGTDB
      DATA NVDRL,IROLD,IVDOLD/0,0,-9/
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C get the vdet material description from the database
C

      IF (NVDRL .EQ.0) NVDRL = NAMIND('VDRL')
      CALL ABRUEV (IRUN,IEVT)
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IVDSTP = GTSTUP ('VD',IRUN)
        IF (IVDSTP.NE.IVDOLD) THEN
           IVDOLD = IVDSTP
           IRET= ALGTDB(JUNIDB(0),'VDRL',-IVDSTP)
        ENDIF
      ENDIF
C
C - next entry
C
      JVDRL = IW(NVDRL)
      IF(JVDRL.LE.0) THEN
         CALL ALTELL('VDMSRL :  Needs a VDRL bank ',0,'RETURN')
         VDMSRL = 0.
         RETURN
      ENDIF
      CALL VADEWA(IWAFID,LAYER,IZ,IPHI,IVX)
      UFACE=U
      WFACE=W
      IF(LAYER.EQ.1) UFACE=-UFACE
      IF(WFACE.GT.0.) THEN
        WMODUL=WFACE
        UMODUL=UFACE
      ELSE
        WMODUL=-WFACE
        UMODUL=-UFACE
      ENDIF
      X=0.
      DO 10 I=1,LROWS(JVDRL)
        IF( ITABL(JVDRL,I,JVDRIG).EQ.1.OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.3.AND.LAYER.EQ.1).OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.4.AND.LAYER.EQ.2)) THEN
C a region of a face
          IF(UFACE.GE.RTABL(JVDRL,I,JVDRUL).AND.
     &      UFACE.LE.RTABL(JVDRL,I,JVDRUH).AND.
     &      WFACE.GE.RTABL(JVDRL,I,JVDRWL).AND.
     &      WFACE.LE.RTABL(JVDRL,I,JVDRWH)) X=X+RTABL(JVDRL,I,JVDRRL)
        ELSE IF( ITABL(JVDRL,I,JVDRIG).EQ.2.OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.5.AND.LAYER.EQ.1).OR.
     &     (ITABL(JVDRL,I,JVDRIG).EQ.6.AND.LAYER.EQ.2)) THEN
C a region of a module
          IF(UMODUL.GE.RTABL(JVDRL,I,JVDRUL).AND.
     &      UMODUL.LE.RTABL(JVDRL,I,JVDRUH).AND.
     &      WMODUL.GE.RTABL(JVDRL,I,JVDRWL).AND.
     &      WMODUL.LE.RTABL(JVDRL,I,JVDRWH)) X=X+RTABL(JVDRL,I,JVDRRL)
        ENDIF
   10 CONTINUE
      VDMSRL=X
      RETURN
      END
