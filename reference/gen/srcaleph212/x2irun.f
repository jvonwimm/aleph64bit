      SUBROUTINE X2IRUN (IRUN,FHIST,IRET)
C
C --------------------------------------------------
C! Initial run routine for level2 trigger.
C
C   Author: T. Medcalf  10/9/87
C
C? Call routines to set up constants, Bos bank names,
C? and look-up tables.
C - Input  :  IRUN / I     = current run number
C             FHIST / L    = histogram flag
C
C - Output :  IRET / I     = return flag
C                            0 means  OK
C                            2 FATAL error in X2LUTS
C --------------------------------------------------
      SAVE
      PARAMETER (NIGN = 3,NPEXI = 11,NPUSE = 8,IOS = 2,NBMAX = 62,
     +   NVMAX = 31,NTMAX = 256,ILM = 15,IUM = 31,ISMX = 9,ICHX = 4,
     +   MSKLN = 60 )
      COMMON /X2CONS/ TDVELO,ADVELO,IGNPAD(NIGN),IPDCON(NPEXI)
     +  ,RADPAD(NPUSE,IOS),CLOCKR
     +  ,NTBINS(IOS),ZACPMM,IZBMAX
     +  ,ITHETA(IOS,0:NTMAX,NPUSE),ITHSUB(IOS,0:NTMAX,NPUSE)
     +  ,ITHOVR(IOS,0:NTMAX,NPUSE),ITHOSB(IOS,0:NTMAX,NPUSE)
     +  ,IHTMAX(IOS,NBMAX),ITHRSH(NPUSE),ITVOTE(0:NVMAX,0:NVMAX)
     +  ,IPADPR(IOS,ICHX),IZRDLK(ILM,IUM,ISMX,ICHX,IOS)
     +  ,IX2PRL,IRWDTH,IDIGNZ
     +  ,MASCON(NBMAX,IOS,IOS),IX2MSK(MSKLN),IX2HIS,IX2RUN
      INTEGER IRUN,IRET
      LOGICAL FHIST
C --------------------------------------------------
C
      IX2RUN = IRUN
      IX2HIS = 0
      IF (FHIST) IX2HIS = 1
      IRET = 0
C
C - read data card X2RU if any
      CALL X2DFCD
C
C - create name-indices and set working bank indices to 0
      CALL X2NAMI
C
C - Create look up tables used by the electronics
      CALL X2LUTS (IRET)
C
      RETURN
      END
