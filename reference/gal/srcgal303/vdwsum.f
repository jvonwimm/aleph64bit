      SUBROUTINE VDWSUM
C-----------------------------------------------------------------------
C! Print run summary for VDET
CKEY VDET
C!
C!
C!  Author         G.Triggiani     7/06/88
C!  Modified       A. Bonissent    10/05/94
C!     Replace use of common VDSTAT by call to VDFILL
C!     with argument 'SUMMARY'
C!
C!  Description
C!  ===========
C!  Print out statistics accumulated during the run for Mini-Vertex
C!  detector
C!
C!  Called by :    ASCRUN                        from this .HLB
C!  Calls     :
C!
C-----------------------------------------------------------------------
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      DIMENSION IARG(8)
C
      WRITE(LOUTIO,980)
      CALL  VDFILL('SUMMARY',IARG)
      NVDHE=IARG(1)
      IVDHE=IARG(2)
      NVDHW=IARG(3)
      IF (NVDHE.GT.0) THEN
        WRITE(LOUTIO,981) NVDHE
        WRITE(LOUTIO,982) IVDHE
        WRITE(LOUTIO,983) NVDHW
        WRITE(LOUTIO,984) FLOAT(IVDHE)/FLOAT(NVDHE)
        IVDHW=IARG(4)
        WRITE(LOUTIO,985) FLOAT(IVDHW)/FLOAT(NVDHW)
        NVDSZ=IARG(5)
        IVDSZ=IARG(6)
        NVDSP=IARG(7)
        IVDSP=IARG(8)
        IF (NVDSZ.GT.0.AND.NVDSP.GT.0) THEN
          WRITE(LOUTIO,986) FLOAT(IVDSZ)/FLOAT(NVDSZ)
          WRITE(LOUTIO,987) FLOAT(IVDSP)/FLOAT(NVDSP)
        ELSE
          WRITE(LOUTIO,988)
        ENDIF
      ELSE
        WRITE(LOUTIO,989)
      ENDIF
C
  980 FORMAT(1H1,'================ VDET Run summary =================')
  981 FORMAT(1H0,'Number of events with hits in VDET.........',I8)
  982 FORMAT(1H ,'Total number of hits in VDET in this run...',I8)
  983 FORMAT(1H ,'Total number of hit wafers in this run.....',I8)
  984 FORMAT(1H ,'Average number of hits/event...............',F8.4)
  985 FORMAT(1H ,'Average number of hits/hit wafer...........',F8.4)
  986 FORMAT(1H ,'Zed strips/hit    (without noise)..........',F8.4)
  987 FORMAT(1H ,'R-Phi strips/hit  (without noise)..........',F8.4)
  988 FORMAT(1H0,'!!!     No digitizings for VDET in this run     !!!')
  989 FORMAT(1H0,'!!!        No hits for VDET in this run         !!!')
C
  999 CONTINUE
      RETURN
      END
