*DK asrust
      SUBROUTINE ASRUST
C ----------------------------------------------------------------------
C. - F.RANJARD - 850325
C! Build the galeph run header banks
C    ATIT,ACUT,AFID,ARUN,APRO,AJOB,AKIN,ASIM
C
C - modified by : F.Ranjard -911002
C                 modified word(JAJOSD) in AJOB bank
C                 the word contains 1 bit per detector starting
C                 at bit 0. The bit is set to 1 if the detector
C                 was selected on the SETS data card.
C                 add SICAL run conditions in ARUN bank
C
C. - called by    ASIRUN                           from this .HLB
C. - Calls        BLIST                            from BOS.HLB
C                 UCOPY, BUNCH                     from CERNlibs
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "gcbank.h"
#include "version.h"
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
#include "ajjpar.h"
#include "asimjj.h"
#include "alcons.h"
#include "alfgeo.h"
      INTEGER IDET(LDET)
      EXTERNAL NAMIND
C ----------------------------------------------------------------------
C
C - get indices of Axxx banks
      JATIT = IW(NAMIND('ATIT'))
      JAKIN = IW(NAMIND('AKIN'))
      JACUT = IW(NAMIND('ACUT'))
      JAFID = IW(NAMIND('AFID'))
      JAJOB = IW(NAMIND('AJOB'))
      JARUN = IW(NAMIND('ARUN'))
      JAPRO = IW(NAMIND('APRO'))
      CALL BLIST (IW,'C+','ATITAKINACUTAFIDAJOBARUNAPRO')
      CALL BKFMT ('ACUT','2I,(A,5F)')
      CALL BKFMT ('AFID','2I,(F)')
      CALL BKFMT ('AJOB','I')
      CALL BKFMT ('AKIN','2I,A,(F)')
      CALL BKFMT ('APRO','I')
      CALL BKFMT ('ARUN','I')
      CALL BKFMT ('ATIT','2I,(A)')
C
C - Build ASIM with date of the geometry
C
      IF (IW(NAMIND('ASIM')).EQ.0) THEN
         CALL ALBOS ('ASIM',0,LASIMA+LMHLEN,JASIM,IGARB)
         IW(JASIM+LMHCOL) = LASIMA
         IW(JASIM+LMHROW) = 1
         IW(JASIM+LMHLEN+JASIYM) = IDATJO
         CALL BKFMT ('ASIM','I')
      ENDIF
      CALL BLIST (IW,'C+','ASIM')
C
C - Build AJOB with all job flags set by data cards
      CALL ALBOS ('AJOB',0,LAJOBA+LMHLEN,JAJOB,IGARB)
      IW(JAJOB+LMHCOL) = LAJOBA
      IW(JAJOB+LMHROW) = 1
      KAJOB = JAJOB + LMHLEN
      IW(KAJOB+JAJOBM) = IBREJO
      IW(KAJOB+JAJORM) = IPACJO
      IW(KAJOB+JAJOJD) = JDATJO
      IW(KAJOB+JAJOJT) = JTIMJO
      IW(KAJOB+JAJOGV) = NINT(VERSJO*10.)
      IW(KAJOB+JAJOAV) = NINT(AVERJO*10.)
      IW(KAJOB+JAJOGI) = MGETJO
      IW(KAJOB+JAJOSO) = MSAVJO
      DO 10 I=1,LDET
         IDET(I) = 0
         IF (IDETJO(I).NE.0) IDET(I) = 1
 10   CONTINUE
      CALL BUNCH (IDET,IW(KAJOB+JAJOSD),LDET,1)
      CALL BUNCH (IGEOJO,IW(KAJOB+JAJOGC),LGEO,1)
      IF (FXTKJO) IW(KAJOB+JAJOFT) = 1
      IF (FXSHJO) IW(KAJOB+JAJOFS) = 1
      IW(KAJOB+JAJOFC) = NINT(CUTFJO*1000.)
      IW(KAJOB+JAJODV) = IDAFJO
      IW(KAJOB+JAJODD) = IDCHJO
      IW(KAJOB+JAJOTV) = NINT(TVERJO*100.)
      IW(KAJOB+JAJOCV) = NINT(CORVER*100.)
      IW(KAJOB+JAJOGN) = INT(GVERSN*10000.)
C
C - Build  ATIT with the run title
      IF (JATIT .EQ. 0) THEN
         IF (TRUNJO.NE.' ') THEN
            LTIT = (LENOCC(TRUNJO)+3)/4
            CALL ALBOS ('ATIT',0,LTIT+LMHLEN,JATIT,IGARB)
            IW(JATIT+LMHCOL) = 1
            IW(JATIT+LMHROW) = LTIT
            KATIT = JATIT + LMHLEN
            CALL ALINST(TRUNJO,IW(KATIT+JATIRT),LTIT)
         ENDIF
      ENDIF
C
C - Build  ACUT with the tracking cuts
      CALL ALBOS ('ACUT',0,LACUTA+LMHLEN,JACUT,IGARB)
      IW(JACUT+LMHCOL) = LACUTA
      IW(JACUT+LMHROW) = 1
      KACUT = JACUT + LMHLEN
      IW(KACUT+JACUMN) = INTCHA ('ALEF')
      CALL UCOPY (TCUTJO,RW(KACUT+JACUGC),LTCUT)
C
C - Build AFID with ALFGEO common variables
      CALL ALBOS ('AFID',0,LAFIDA+LMHLEN,JAFID,IGARB)
      IW(JAFID+LMHCOL) = LAFIDA
      IW(JAFID+LMHROW) = 1
      KAFID = JAFID + LMHLEN
      RW(KAFID+JAFIAR) = ALRMAX
      RW(KAFID+JAFIAZ) = ALZMAX
      RW(KAFID+JAFIMF) = ALFIEL
      RW(KAFID+JAFIBE) = ALECMS
C
C - Build APRO with process flags and random generator rootsk
      CALL ALBOS ('APRO',0,LAPROA*LPRO+LMHLEN,JAPRO,IGARB)
      IW(JAPRO+LMHCOL) = LAPROA
      IW(JAPRO+LMHROW) = LPRO
      KAPRO = JAPRO + LMHLEN
      DO 1 I=1,LPRO
         IW(KAPRO+JAPRPF) = IPROJO(I)
         IW(KAPRO+JAPRRG) = IRNDJO(1,I)
         IW(KAPRO+JAPRRG+1) = IRNDJO(2,I)
         IW(KAPRO+JAPRRG+2) = IRNDJO(3,I)
  1   KAPRO = KAPRO + LAPROA
C
C - Build AKIN with the kinematic parameters
      LKIN = MIN (LKINP,NKINJO)
      IF (LKIN .GT. 0) THEN
         IF (JAKIN .EQ. 0) THEN
            CALL ALBOS ('AKIN',0,LKIN+1+LMHLEN,JAKIN,IGARB)
            IW(JAKIN+LMHCOL) = 1
            IW(JAKIN+LMHROW) = LKIN+1
         ENDIF
         KAKIN = JAKIN + LMHLEN
         IW(KAKIN+JAKIKT) = INTCHA (TKINJO)
         CALL UCOPY (BKINJO,RW(KAKIN+JAKIKP),LKIN)
      ENDIF
C
C - Build ARUN with the detector run conditions
      LARUN = MAX (LCVD,LCIT,LCTP,LCEC,LCLC,LCSA,LCHC,LCMU)
      CALL ALBOS ('ARUN',0,LARUN*LDET+LMHLEN,JARUN,IGARB)
      IW(JARUN+LMHCOL) = LARUN
      IW(JARUN+LMHROW) = LDET
      KARUN = JARUN + LMHLEN
      CALL UCOPY (ICVDJO,IW(KARUN+JARURC),LCVD)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICITJO,IW(KARUN+JARURC),LCIT)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICTPJO,IW(KARUN+JARURC),LCTP)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICECJO,IW(KARUN+JARURC),LCEC)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICLCJO,IW(KARUN+JARURC),LCLC)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICSAJO,IW(KARUN+JARURC),LCSA)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICHCJO,IW(KARUN+JARURC),LCHC)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICMUJO,IW(KARUN+JARURC),LCMU)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICSIJO,IW(KARUN+JARURC),LCSI)
C
C - End
 999  CONTINUE
      RETURN
       END
#endif
