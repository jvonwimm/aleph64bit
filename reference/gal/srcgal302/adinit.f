      SUBROUTINE ADINIT
C------------------------------------------------------------------
C B. BLOCH-DEVAUX  - 850910
C!  Initialises capture file and set up view banks
C
C   Called by user   in ASIPAC   from this .HLB
C   Calls HPLCAP
C   Calls GUINTI,ADXY,ADRZ   from this .HLB
C ---------------------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      CALL HPLCAP(-10)
      CALL GUINTI
      CALL ADXY(1)
      CALL ADRZ(2)
      RETURN
      END
