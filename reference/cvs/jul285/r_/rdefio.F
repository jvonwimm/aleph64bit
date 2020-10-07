      SUBROUTINE RDEFIO (IERIO)
C----------------------------------------------------------------------
C!  - Define formats for input and output files
C!
C!  Author        S. Orteu        18-MAR-1987
C!  Modified      J.Knobloch      15-DEC-1988
C!   Modified :- E. Lancon              7-MAR-1991
C!              Remove the MINI option
C!  Modified      J.Sedgbeer      01-MAR-1991 Add IXTR bank to POT.
C!  Modified      J.Sedgbeer      25-SEP-1991 Add IZDB bank to POT.
C!   Modified :- E. Lancon             27-JAN-1992
C!              Remove the DST option
C!              Do not open output file anymore, done by ABRSEL (alephli
C!              Add IERIO as argument
C!
C!   Outputs:
C!        -   IERIO           I/    Error Return Code
C!                                  0 = No error
C!                                  1 = Invalid Output Option
C!                                  3 = Too Many Output Banks
C!
C!   Modified :- E. Lancon             17-MAY-1993
C!        remove call to RFORMA put it in RINRUN
C!   Modified :- E. Lancon             20_SEP-1993
C!        call ALFMT to defined format for required output banks
C!   Modified :- P. Comas            4-MAY-1994
C!        Add new (due to the change in the readout) trigger banks
C!        to the POT
C!   Modified :- P. Comas            2-MAY-1995
C!        New banks PWEI and LPAS to POT
C!   Modified :- P. Comas           29-MAY-1995
C!        New bank HLOD to POT
C!   Modified :- P. Comas           27-JUL-1995
C!        New MC bank X1IP to POT
C!   Modified :- P. Comas           15-NOV-1995
C!        New bank PTPX for pad dE/dx to POT
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
#include "bcs.h"
      EXTERNAL LNBLNK
      EXTERNAL ALFMT
      INTEGER  ALFMT
      CHARACTER*75 FORMA
      CHARACTER FNAME*120,FTYPE*8,FDEVI*80,CBANK*4
      CHARACTER POTBK*4
      CHARACTER*5 PBANK(15)
      CHARACTER*4 CHAINT,TLIST(MAXFPO)
C
C?  Nasty should go in RLUNIT com deck at next julia version
C
      DATA JULFMT / 17 /
C----------------------------------------------------------------------
      IERIO = 0
C    output formats on cards
      FTYPRL(1) = 'NATI'
      FTYPRL(2) = 'EPIO'
      FTYPRL(3) = 'NO  '
C    output formats for BOS
      FORMRL(1) = 'FORT'
      FORMRL(2) = 'EPIO'
      FORMRL(3) = 'NO  '
C    types of output
      OTYPRL(1) = 'DALI'
      OTYPRL(2) = 'FILO'
      OTYPRL(3) = 'DST '
C    default lists for the 3 output types
C
C    first DALI
C
      OUTLIS(1) = ' '
C
C    DST
C
      OUTLIS(3) =
     +'EVEHASEVKINEVERTDHEAKEVHFKINFVER'//
     +'BOMPBOMRBOMBBOME'//
     +'EGPR'//
     +'FPOLKPOLFZFRFPOI'//
     +'HLTU'//
     +'LUPALIDTPLPDPLSD'//
     +'MUIDMUEXMHITMCADHMADHROAD4CD'//
     +'PFRFPFRTFRID'//
     +'PSCOSFTRPSPO'//
     +'PEMHPHMHPITMPASL'//
     +'PYERPYFRYV0V'//
     +'PCRLPCOBPCHYPECOPEHYPEPT'//
     +'PTEXEIDTPCPAPPRLPHCOPHHY'//
     +'PFERPFHRPEWIEJET'//
     +'BCNTBHITBPTRREVHRTLOPTST'//
     +'X1ADX3ECX3EWX3HCX3ITX3L2X3LUX3TOX3TPX3X3MUDGHPDIERRF'//
     +'JBERJESTECRQOSTSX3TMHCCVHINF'//
     +'VDCOVCPLVDXYVDZTVDTDVDHTVDFKVUFK'//
     +'PPOBPPDSX1TIXTBPXTEBXTRBXTCNX2DF'//
     +'VFHLVFLGVFPHVCOM'//
     +'EWHERTLSPT2XPCOI'//
     +'PIDIPRPWLOLEMTHR'//
     +'PTBCPTNC'//
     +'PESTETDI'//
     +'PGACPGID'//
     +'PHSTHTUB'//
     +'EFOLHCTEECTEETKCSILHSILUSIXASPDASCLS'//
     +'IXTRIZDBX1RGX1ERERORLPASPWEIHLODX1IPVWRLVDMRVDGCPTPX'
      L3=LNBLNK(OUTLIS(3))
C
C    POT  = DST
C
      OUTLIS(2) = OUTLIS(3)
C
      TEMLIS=' '
      LREC = 0
C
C Define Output file format
C
      IF (IW(NAMIND('MINI')).NE.0) THEN
        WRITE (LOUTRL,'(2a)')
     &    '0REDFIO- MINI Is No Longer A Valid Output Format - Use POT '
        IERIO = 1
        GOTO 999
      ENDIF
C
      DO 1 I=1,NWRTMX
        KTYP=IW(NAMIND(OTYPRL(I)))
        IF (KTYP.EQ.0)THEN
          LUNIRL(I)=0
          JOUTRL(I)=NOOUTP
        ELSE
          ANYORL=.TRUE.
          LUNIRL(I)=0
          JOUTRL(I)=NATOUT
        ENDIF
    1 CONTINUE
C
C     handle ADDD card
C
      KADDD = IW(NAMIND('ADDD'))
    2 CONTINUE
      IF(KADDD.NE.0)THEN
        I=IW(KADDD-2)
        IF(I.EQ.0)I=1
        DO 3 J=1,IW(KADDD)
          POTBK = CHAINT(IW(KADDD+J))
          L=LNBLNK(OUTLIS(I))
          IF(L.EQ.0)THEN
            OUTLIS(I)=POTBK
          ELSE
            OUTLIS(I)=OUTLIS(I)(1:L)//POTBK
          ENDIF
    3   CONTINUE
        KADDD=IW(KADDD-1)
        GOTO 2
      ENDIF
C
C     handle REMD data card
C
      KREMD = IW(NAMIND('REMD'))
   31 CONTINUE
      IF(KREMD.NE.0)THEN
        I=IW(KREMD-2)
        IF(I.EQ.0)I=1
        DO 5 J=1,IW(KREMD)
          L=LNBLNK(OUTLIS(I))
          NB=L/4
          CBANK=CHAINT(IW(KREMD+J))
          LTEM=LNBLNK(TEMLIS)
          IF(LTEM.EQ.0)THEN
            TEMLIS=CBANK
          ELSE
            TEMLIS=TEMLIS(:LTEM)//CBANK
          ENDIF
          DO 4 N=1,NB
            I1=(N-1)*4+1
            I2=I1+3
            IF(OUTLIS(I)(I1:I2).EQ.CBANK)
     +         OUTLIS(I)=OUTLIS(I)(:I1-1)//OUTLIS(I)(I2+1:)
    4     CONTINUE
    5   CONTINUE
        KREMD=IW(KREMD-1)
        GOTO 31
      ENDIF
C
C     Clean up, sort and print list of banks on each output stream
C
      DO 20 I=1,NWRTMX
        IF(JOUTRL(I).NE.NOOUTP)THEN
          DO 12 J=1,15
   12     PBANK(J)=' '
          WRITE(LOUTRL,'(2A)')'0Banks written to ',OTYPRL(I)
          L=LNBLNK(OUTLIS(I))
          NB=L/4
          K=0
C
C       First clean up
C
          IF(NB.GT.MAXFPO)THEN
            DO 29 M=1,20
              WRITE(LOUTRL,'(A,I4,A,I4)')
     +          ' -TOO MANY BANKS ON OUTPUT LIST-',
     +          NB, ' - LIMITED TO -',MAXFPO
              IERIO = 3
   29       CONTINUE
            GOTO 999
          ENDIF
          DO 30 N=1,NB
            I1=(N-1)*4+1
            I2=I1+3
   30     TLIST(N)=OUTLIS(I)(I1:I2)
C
C      sort the banknames on a list
C      but leave the first one in place (EVEH)
C
          CALL SORCHA(TLIST(2),1,4,NB-1,1)
C
C          put the names back - eliminate doubles
C
          N=0
          DO 35 M=1,NB
            IF(M.EQ.1.OR.TLIST(M).NE.TLIST(M-1))THEN
              N=N+1
              I1=(N-1)*4+1
              I2=I1+3
              OUTLIS(I)(I1:I2)=TLIST(M)
            ELSEIF ( M.GT.1 .AND. TLIST(M).EQ.TLIST(M-1) ) THEN
              WRITE (LOUTRL,'(3A)') ' ++++ BANK ',TLIST(M),
     &          ' Duplicated in Output List Definition '
            ENDIF
   35     CONTINUE
C
C?  Check bank formats and write output bank list to julia log file
C
          NB=N
          DO 15 N=1,NB
            I1=(N-1)*4+1
            I2=I1+3
            K=K+1
            CBANK = OUTLIS(I)(I1:I2)
            IER = ALFMT (JULFMT, CBANK, FORMA)
            IF ( IER.NE.0 ) THEN
              WRITE (LOUTRL,'(3A)') ' ++++ RDEFIO : Bank ',CBANK,
     &          ' has no format defined'
            ENDIF
            PBANK(K)=' '//CBANK
            IF(K.EQ.15)THEN
              WRITE(LOUTRL,'(20X,15A)')PBANK
              DO 14 J=1,15
   14         PBANK(J)=' '
              K=0
            ENDIF
   15     CONTINUE
          IF(K.NE.0) WRITE(LOUTRL,'(20X,15A)')PBANK
        ENDIF
   20 CONTINUE
C
C     open the file for monitoring if requested
C
      CALL RMONII
C
  999 RETURN
C
      END
#endif
