      SUBROUTINE MINFIL
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill POT banks from Mini-DST.
C
C     Author: Stephen Haywood      03-Apr-90
C     Modify: Stephen Haywood      08-Jun-90   for NOCH etc
C     Modify: Stephen Haywood      17-Feb-93
C     Modify: Agnieszka Jacholkowska  22-Nov-94
C     Modify: Agnieszka Jacholkowska  30-Apr-97
C
C     POT banks are filled as far as is possible from the Mini-DST.
C     This is done provided the POT banks do not exist already.
C     If the cards NOCH etc are present, then the corresponding banks
C     will not be filled.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      DIMENSION IPV(10),IAV(10)
      LOGICAL FIRST, OKCH,OKV0,OKCO,OKPC,OKGA,OKEF,OKEJ,OKMC
      LOGICAL XW,XXW
      SAVE FIRST, OKCH,OKV0,OKCO,OKPC,OKGA,OKEF,OKEJ,OKMC,
     &        LLOLE,LXTRB,LFRFT,LFRTL,LPYER,LYV0V,LPYFR,LPCPA,LPCRL,
     &  LPECO,LPHCO,LEGID,LEGPC,LPGID,LPGPC,LEIDT,LFRID,LEFOL,LEJET,
     &  LFKIN,LFVER,LFZFR,LPTMA,LPTML,LPASL,LPITM,
     &  LDTBP,LDTRA,LDVER,LDNEU,LDCRL,LDECO,LDHCO,LDGAM,LDFOT,
     &  LDGID,LDGPC,LDEID,LDENF,LDJET,LDTMC,LDVMC,LDFMC,LDGAC,
     &  LDMSC,LDDLT,LDMLT,LDLJT,LDBTG,LDTHR,LDKNK,LDWES,LDENW
      DATA FIRST / .TRUE. /
      DATA OKCH,OKV0,OKCO,OKPC,OKGA,OKEF,OKEJ,OKMC / 8*.TRUE. /
      DATA    LLOLE,LXTRB,LFRFT,LFRTL,LPYER,LYV0V,LPYFR,LPCPA,LPCRL,
     &  LPECO,LPHCO,LEGID,LEGPC,LPGID,LPGPC,LEIDT,LFRID,LEFOL,LEJET,
     &  LFKIN,LFVER,LFZFR,LPTMA,LPTML,LPASL,LPITM,LPGAC,LPMSC,
     &  LDTBP,LDTRA,LDVER,LDNEU,LDCRL,LDECO,LDHCO,LDGAM,LDFOT,
     &  LDGID,LDGPC,LDEID,LDENF,LDJET,LDTMC,LDVMC,LDFMC,LDGAC,LDMSC
     &  / 47*0 /
      DATA    LPDLT,LPMLT,LPLJT,LDDLT,LDMLT,LDLJT,
     &        LNBIP,LPTHR,LDBTG,LDTHR,LDKNK,LDWES,LDENW /13*0/
C
#include "bmacro.h"
C
C++   Useful logical functions:
C++   XW checks existence of bank with NAMIND LBNK.
C++   XXW checks existence of Mini and absence of POT banks.
C
      XW(LBNK) = IW(LBNK).GT.0
      XXW(LMIN,LPOT) = XW(LMIN) .AND. .NOT.XW(LPOT)
C
C++   Initialisation.
C
      IF(FIRST) THEN
C
C++      Print the version number.
C
         WRITE(6,'(//40X,''**** Mini-DST version'',I5,'' ****'')')
     &   MINVSN(DUMMY)         
C
C++      Obtain named indices for Mini banks.
C
         LDTBP = NAMIND('DTBP')
         LDTRA = NAMIND('DTRA')
         LDVER = NAMIND('DVER')
         LDNEU = NAMIND('DNEU')
         LDCRL = NAMIND('DCRL')
         LDECO = NAMIND('DECO')
         LDHCO = NAMIND('DHCO')
         LDGAM = NAMIND('DGAM')
         LDFOT = NAMIND('DFOT')
         LDGID = NAMIND('DGID')
         LDGPC = NAMIND('DGPC')
         LDEID = NAMIND('DEID')
         LDENF = NAMIND('DENF')
         LDENW = NAMIND('DENW')
         LDJET = NAMIND('DJET')
         LDTMC = NAMIND('DTMC')
         LDVMC = NAMIND('DVMC')
         LDFMC = NAMIND('DFMC')
C
C++      Obtain named indices for POT banks.
C
         LLOLE = NAMIND('LOLE')
         LXTRB = NAMIND('XTRB')
         LFRFT = NAMIND('FRFT')
         LFRTL = NAMIND('FRTL')
         LPYER = NAMIND('PYER')
         LYV0V = NAMIND('YV0V')
         LPYFR = NAMIND('PYFR')
         LPCPA = NAMIND('PCPA')
         LPCRL = NAMIND('PCRL')
         LPECO = NAMIND('PECO')
         LPHCO = NAMIND('PHCO')
         LEGID = NAMIND('EGID')
         LEGPC = NAMIND('EGPC')
         LDGAC = NAMIND('DGAC')
         LDMSC = NAMIND('DMSC')
         LDDLT = NAMIND('DDLT')
         LDMLT = NAMIND('DMLT')
         LDLJT = NAMIND('DLJT')
         LDBTG = NAMIND('DBTG')
         LDTHR = NAMIND('DTHR')
         LDKNK = NAMIND('DKNK')
         LYKNK = NAMIND('YKNK')
         LDWES = NAMIND('DWES')
         LPGID = NAMIND('PGID')
         LPGPC = NAMIND('PGPC')
         LPGAC = NAMIND('PGAC')
         LPMSC = NAMIND('PMSC')
         LPDLT = NAMIND('PDLT')
         LPMLT = NAMIND('PMLT')
         LPLJT = NAMIND('PLJT')
         LPTHR = NAMIND('PTHR')
         LNBIP = NAMIND('NBIP')
         LEIDT = NAMIND('EIDT')
         LFRID = NAMIND('FRID')
         LEFOL = NAMIND('EFOL')
         LEJET = NAMIND('EJET')
         LFKIN = NAMIND('FKIN')
         LFVER = NAMIND('FVER')
         LFZFR = NAMIND('FZFR')
         LPTMA = NAMIND('PTMA')
         LPTML = NAMIND('PTML')
         LPASL = NAMIND('PASL')
         LPITM = NAMIND('PITM')
C
C++      Determine which POT banks should be filled.
C
         OKCH = NLINK('NOCH',0).EQ.0
         OKV0 = NLINK('NOV0',0).EQ.0
         OKCO = NLINK('NOCO',0).EQ.0
         OKPC = NLINK('NOPC',0).EQ.0
         OKGA = NLINK('NOGA',0).EQ.0
         OKEF = NLINK('EFLW',0).GT.0
         OKEJ = NLINK('EFLJ',0).GT.0
         OKMC = NLINK('NOMC',0).EQ.0
         IF (.NOT.OKCH) OKV0 = .FALSE.
         IF (OKEJ) OKEF = .TRUE.
         IF (OKEF) OKCH = .TRUE.
C
C++      See if we are reading MC; if not, then don't fill MC banks.
C
         CALL ALVSN(ITYP,IPV,IAV,IYR)
         IF (IPV(1).EQ.0) OKMC = .FALSE.
C
         FIRST = .FALSE.
      ENDIF
C
C++   Try to create POT/Julia banks if they are missing.
C++   The requirement on Mini banks may not be complete, but their
C++   presence is checked in the corresponding subroutines.
C
      IF (.NOT.XW(LLOLE)             ) CALL MINLOL
      IF (XXW(LDTBP,LXTRB)           ) CALL MINXTR
      IF (XXW(LDTRA,LFRFT) .AND. OKCH) CALL MINFRF
      IF (XXW(LDTRA,LFRTL) .AND. OKCH) CALL MINFRT
      IF (XXW(LDVER,LPYER) .AND. OKCH) CALL MINPYE
      IF (XXW(LDTRA,LYV0V) .AND. OKV0) CALL MINYV0
      IF (XXW(LDVER,LPYFR) .AND. OKCH) CALL MINPYF
      IF (XXW(LDNEU,LPCPA) .AND. OKPC) CALL MINPCQ
      IF (XXW(LDCRL,LPCRL) .AND. OKCO) CALL MINPCR
      IF (XXW(LDECO,LPECO) .AND. OKCO) CALL MINPEC
      IF (XXW(LDHCO,LPHCO) .AND. OKCO) CALL MINPHC
      IF (XXW(LDGAM,LEGID) .AND. OKGA) CALL MINEGI
      IF (XXW(LDGAC,LPGAC) .AND. OKGA) CALL MINPGA
      IF (XXW(LDMSC,LPMSC) .AND. OKCH) CALL MINPSC
      IF (XXW(LDDLT,LPDLT)           ) CALL MINPDL
      IF (XXW(LDMLT,LPMLT)           ) CALL MINPML
      IF (XXW(LDLJT,LPLJT)           ) CALL MINPJL
      IF (XXW(LDTHR,LPTHR)           ) CALL MINPTH
      IF (XXW(LDKNK,LYKNK)           ) CALL MINYNK
      IF (XXW(LDBTG,LNBIP)           ) CALL MINBIP
      IF (XXW(LDGID,LPGID) .AND. OKGA) CALL MINPGI
      IF (XXW(LDGPC,LPGPC) .AND. OKGA) CALL MINPGP
      IF (XXW(LDEID,LEIDT)           ) CALL MINEIT
      IF (XXW(LDTRA,LFRID)           ) CALL MINFRI
      IF (XXW(LDENF,LEFOL) .AND. OKEF) CALL MINEFO
      IF (XXW(LDJET,LEJET) .AND. OKEJ) CALL MINEJE
C
C++   Monte-Carlo truth information.
C
      IF (.NOT.OKMC) GOTO 1000
C
      IF (XXW(LDTMC,LFKIN)           ) CALL MINFKI
      IF (XXW(LDVMC,LFVER)           ) CALL MINFVE
      IF (XXW(LDFMC,LFZFR)           ) CALL MINFZF
C
C++   For Monte-Carlo, unpack PTMA and PTML or PASL and PITM to enable
C++   matching.
C++   This may not be done by AUNPCK if there is no PTEX on Mini-DST.
C++   Put Julia banks on 'E' list in case 'S' list gets lost.
C
      IF ((XW(LPTMA).OR.XW(LPTML)) .AND. OKCH) THEN
         CALL PTGMAJ(' ',IER)
      ENDIF
C
C++   The use of PITMAJ requires the magnetic field.
C
      IF ((XW(LPASL).OR.XW(LPITM)) .AND. OKCH) THEN
         FIELD = ALFIEL(DUMMY)
         CALL PITMAJ(' ',FIELD,IER)
      ENDIF
C
C++   Drop any updated banks.
C
 1000 CALL MINUPD('DROP')
C
      RETURN
      END
#endif
