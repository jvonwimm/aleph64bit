      SUBROUTINE TPFILL(IRET)
C
C! Fast sim : Fill the arrays concerning Landau fluctuations and
C! ExB effect from BOS_BANKs TIND,TWRD,TLAN,TEXB and TMSH
C
C  Called from : TSINIT
C  Calls       : UCOPY, WBANK
C
C   Input:            none
C
C   Output:     /FASTER/   -- NITLAN, to get index of bank TLAN
C                          -- NITIND, to get index of bank TIND
C                          -- INTWRD, index of work bank TWRD
C               PASSED:    -- IRET  , =1 in case of BOS problem
C
C  P. Janot  - 10 May 88
C Modifications  :
C      1.  P. Janot  01 sep 88 Replace named bank TWRD with the
C                              corresponding work bank, since it
C                              can be determined with TIND bank.
C          Description of this bank:
C
C          TWRD :  IW(INTWRD+1)   : Number of word/word number (=1)
C                  IW(INTWRD+2)   : Number of word numbers (=100)
C                      ...
C                  IW(INTWRD+2+N) : Word number in TLAN for N primary
C                                   cluster case probabilities, 1<N<100.
C
C      2.  P. Janot  06 sep 88 Get TLAN, TIND, TEXB and TMSH from
C                              ALEPH data base.
C------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C  TPCOND  conditions under which this simulation
C  will be performed
C
      COMMON /DEBUGS/ NTPCDD,NCALDD,NTPCDT,NCALDT,NTPCDA,NCALDA,
     &                NTPCDC,NCALDC,NTPCDS,NCALDS,NTPCDE,NCALDE,
     &                NTPCDI,NCALDI,NTPCSA,NCALSA,NTPCDR,NCALDR,
     &                LTDEBU
      LOGICAL LTDEBU
      COMMON /SIMLEV/ ILEVEL
      CHARACTER*4 ILEVEL
      COMMON /GENRUN/ NUMRUN,MXEVNT,NFEVNT,INSEED(3),LEVPRO
      COMMON /RFILES/ TRKFIL,DIGFIL,HISFIL
      CHARACTER*64 TRKFIL,DIGFIL,HISFIL
      COMMON /TLFLAG/ LTWDIG,LTPDIG,LTTDIG,LWREDC,FTPC90,LPRGEO,
     &                LHISST,LTPCSA,LRDN32,REPIO,WEPIO,LDROP,LWRITE
      COMMON /TRANSP/ MXTRAN,CFIELD,BCFGEV,BCFMEV,
     &                        DRFVEL,SIGMA,SIGTR,ITRCON
      COMMON /TPCLOK/ TPANBN,TPDGBN,NLSHAP,NSHPOF
      COMMON /AVLNCH/ NPOLYA,AMPLIT,GRANNO(1000)
      COMMON /COUPCN/ CUTOFF,NCPAD,EFFCP,SIGW,SIGH,HAXCUT
      COMMON /TGCPCN/ TREFCP,SIGR,SIGARC,RAXCUT,TCSCUT
      COMMON /DEFAUL/ PEDDEF,SPEDEF,SGADEF,SDIDEF,WPSCAL,NWSMAX,THRZTW,
     &                LTHRSH,NPRESP,NPOSTS,MINLEN,
     &                LTHRS2,NPRES2,NPOST2,MINLE2
      COMMON /SHAOPT/ WIRNRM,PADNRM,TRGNRM
C
      LOGICAL LTWDIG,LTPDIG,LTTDIG,LPRGEO,
     &        LWREDC,LTPCSA,LHISST,FTPC90,LRND32,
     &        REPIO,WEPIO,LDROP,LWRITE
C
      LOGICAL LTDIGT(3)
      EQUIVALENCE (LTWDIG,LTDIGT(1))
C
      REAL FACNRM(3)
      EQUIVALENCE (WIRNRM,FACNRM(1))
C
C
C  FASTER : variables used in fast simulation
C
      COMMON / LANDAU / NITLAN,NITIND,INTWRD
      COMMON / EXBEFF / XPROP,TTTT,XXXX(1001),XSHFT(50)
      COMMON / EVT / IEVNT,ISECT
      COMMON / T3TR / JSEGT,NSEGT,ITYPE,WIRRAD(4),WIRPHI(4)
     &               ,AVTIM(4),NELE(4),NCL,SIGT(4)
      COMMON / TPSG / CC(3),XX(3),TOTDIS,XLEN,ISTY,IE
      COMMON / XBIN / IBIN(4),NAVBIN(4),NB
      DIMENSION XEX(4,4)
      PARAMETER (SQ2=1.4142136,SQ3=1.7320508)
C
      DIMENSION NIBANK(4)
      CHARACTER*4 NABK(4)
      DATA NABK/'TLAN','TIND','TMSH','TEXB'/
      DATA LBASE/4/
      IRET = 0
C
C  Get ExB and Landau tables from ALEPH data base
C
      DO 10 IABK=1,4
        IND = MDARD(IW,LBASE,NABK(IABK),0)
        IF(IND .LE. 0) THEN
          WRITE(6,*) '+++ TPFILL +++ No ',NABK(IABK), 'bank in Dbase'
          GOTO 998
        ENDIF
        NIBANK(IABK) = NAMIND(NABK(IABK))
10    CONTINUE
      NITLAN =    NIBANK(1)
      NITIND =    NIBANK(2)
      INTLAN = IW(NIBANK(1))
      INTIND = IW(NIBANK(2))
      INTMSH = IW(NIBANK(3))
      INTEXB = IW(NIBANK(4))

C
C  Create work-bank TWRD,
C
      INTWRD = 0
      CALL WBANK(IW,INTWRD,102,*997)
C
C  and fill it as specified above.
C
      IW(INTWRD+1) = 1
      IW(INTWRD+2) = 100
      DO 50 IIND  = 1,99
        IINDEX = IW(INTIND+2+IIND)
        IF(IINDEX.EQ.0) THEN
          IW(INTWRD+2+IIND) = 0
          GOTO 50
        ENDIF
        DO 51 JIND = IIND+1,100
          JINDEX = IW(INTIND+2+JIND)
          IF(JINDEX.EQ.0) GOTO 51
          IW(INTWRD+2+IIND) = JINDEX - IINDEX
          GOTO 50
51      CONTINUE
50    CONTINUE
      IW(INTWRD+2+100) = IW(INTLAN+2)-IW(INTIND+2+100)+1
C
C  Assign values to XPROP and TTTT. Fill arrays XXXX(1001) and
C  XSHFT(50) for ExB effect.
C
      IGAP = INTEXB + 2 +(ITRCON-1)*IW(INTEXB+1)
      XPROP  = RW(IGAP+1)
      TTTT   = RW(IGAP+2)
      CALL UCOPY(RW(IGAP+3),XSHFT(1),50)
C
      DO 20 IX=1,1001
        IGAP = INTMSH + 2 + (IX-1)*IW(INTMSH+1) + ITRCON
        XXXX(IX) = RW(IGAP)
20    CONTINUE
C
C  Drop TEXB and TMSH BANKs.
C
      CALL BDROP(IW,'TEXB')
      CALL BDROP(IW,'TMSH')
      RETURN
C---------------------------------------------------------------------
998   IRET = 1
      WRITE(6,1001)
1001  FORMAT(//10X,'       **** TPFILL Warning ****'/
     &         10X,' Problems with the Landau Fluctuation file'/
     &         10X,'           Stop execution'//)
      RETURN
997   IRET=1
      WRITE(6,1002)
      RETURN
1002  FORMAT(//10X,'       **** TPFILL Warning ****'/
     &         10X,'    Not enough space for creating TWRD'/
     &         10X,'           Stop execution'//)
      END
