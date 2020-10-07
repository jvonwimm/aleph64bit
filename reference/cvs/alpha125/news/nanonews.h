C-----------------------------------------------------------------------
C!   File     : NCDE.INPUT
C!   Author   :- Yves A. Maumary       20-FEB-1991  NANO Version 105
C!   Modified :- Yves A. Maumary       16-OCT-1991  NANO Version 110
C!   Modified :- Yves A. Maumary       11-AUG-1992  NANO Version 111
C!   Modified :- Yves A. Maumary        7-JAN-1993  Add MAXMCT;
C!                                                  changes: MC in NVEC
C!   Modified :- Yves A. Maumary        5-FEB-1993  Add V0 stuff
C!                                                  (G.Graefe)
C!   Modified :- Gerrit Graefe         21-FEB-1994  NANO Version 115
C!                                                  change V0 stuff
C!                                                  remove NDEC,NDUC
C!                                                  add gamma conversions
C!   Modified :- Gerrit Graefe         10-FEB-1995  NANO Version 116
C!                                                  Implement EFLOW bank
C!                                                  change lepton stuff
C!                                                  remove event shape
C!                                                    b - tags
C!                                                  add b-tag from QVSRCH
C!
C!   Description
C!   ===========
C!   Contains COMDECKs for the Nano-DST.
C!   Used in NDSTPnnn ( Nano-DST Production program, run in ALPHA ) and
C!   in SANDYnnn ( Speedy Analysis on the Nano-Dst ).
C!======================================================================
C!.***
C!.*** VARIABLES STARTING WITH NX ARE LOGICALS
C!.*** VARIABLES STARTING WITH NC ARE CHARACTER VARIABLES
C!.***
C
C?.Start of NPCDSH -----------------------------------------------------
C
C!.LNNDSB: List of Names of NDSt Banks
C
C!.NCLPOT: List of POT/DST/MINI banks on this nano-dst
C!.NCLRUN: List of run banks on this nano-dst
C!.NCLNDS: List of Nano-DSt banks on this nano-dst
C?.Length of LISts, BOS name-indexes and positions in LNNDSB
C
C?.Number of PARameters per HYpothesis, NumBeR of HYPothesis
C!.PARMPH(I,J): PARaMeter I (used in NUPAHY subroutine) for Particle
C!.             Hypothesis J
C
C
C!.NXPAHY(IHYP): T if PArticle HYpothesis bit set for hyp. IHYP
C!.   IHYP = 1 : electron hypothesis
C!.          2 : muon hypothesis
C!.          3 : pion hypothesis
C!.          4 : kaon hypothesis
C!.          5 : proton hypothesis
C?.Card Names for particle Hypothesis bits
C?.PArticle MASses of the hypothesis
C
C!.Counters: NEPROC: Number of Events PROCessed
C!.          NEWRIT: Number of Events WRITten to the Nano-DST
C!.          NRWRIT: Number of Run records WRITten
C!.          NTWRIT: Total records WRITten
C?.Nano-DST output file unit: NUOUTP
C?.Lost events output file unit: NULOST
C?.Write counter for lost events: NEVTLO
C?.Buffers for lost events : IRUNLO,IEVTLO
C
C?.Number of Nano-DST Parameters (selection parameters)
C?.IOFLAG: indicates which ndst banks will be written
C?.        bit position correspond to position of name in string LNNDSB
C!.MINTRK: MINimum number of charged TRacKs
C!.PCUTMI: lower momentum CUT for charged tracks
C!.PCUTMA: upper momentum CUT for charged tracks
C!.THCCUT: Cosine THeta CUT for charged tracks
C!.NITCUT: minimum number of ITC coordinates for charged tracks
C!.NTPCUT: minimum number of TPC coordinates for charged tracks
C!.BD0CUT: upper D0 CUT for charged tracks
C!.BZ0CUT: upper Z0 CUT for charged tracks
C!.PCUMIJ: lower momentum CUT for charged tracks before clustering
C!.PCUMAJ: upper momentum CUT for charged tracks before clustering
C!.THCCUJ: Cosine THeta CUT for charged tracks before clustering
C!.NITCUJ: minimum number of ITC coordinates for charged tracks before
C!.        clustering
C!.NTPCUJ: minimum number of TPC coordinates for charged tracks before
C!.        clustering
C!.BD0CUJ: upper D0 CUT for charged tracks before clustering
C!.BZ0CUJ: upper Z0 CUT for charged tracks before clustering
C!.BMYCUT: mass cut for jet algorithm QJMMCL
C!.ENEVIS: VISible ENErgy for jet algorithm QJMMCL
C!.PHOCUT: lower energy CUT for PHOtons
C!.ALIVER: Alephlib version
C!.ECHMIN: MINimum CHarged Energy to accept the event
C!.RECOOP: REConstructed Objects OPtion (available opt.: 'RE','CO','CH')
C!.V0CHSI: minimum CHi square increase of SIngle track from V0 fitting back to
C!.        the main vertex.
C!.V0CHBO: minimum CHi square increase of BOth tracks from V0 fitting back to
C!.        the main vertex.
C
C?.NXWREV: indicates if the event should be written
C!.NXBANK(I): T if bank is wanted
C!            (I is the position of the bank name in string LNNDSB)
C!.NXONLY(I): if T => write only if bank is not empty
C!            (I is the position of the bank name in string LNNDSB)
C?.NXNDST: T if the NDST bank should be written into the run record
C
C!.NCPROD: Version of NDSTPROD
C!.NCALPH: ALPHa version and correction file used for production
C!.NCDATE: DATE of the production
C
C?.NXWRLO: Flag for LOst events list output file
C?.NXLUMO: Flag for XLUMOK rejection
C?.NXHVBI: Flag for rejection if ITC or TPC voltage not OK
C?.NXVDBI: Flag for XVDEOK rejection
C?.NXECBI: Flag for rejection if ECAL voltage not OK
C
C?.NXHVOK: Flag for ITC and TPC voltage OK
C?.NXDEOK: Flag for TPC dE/dx voltage and calibration OK
C?.NXVDOK: Flag for good VDET data
C?.NXMUOK: Flag for QMUIDO (.FALSE. = failure)
C?.NXEFOK: Flag for energy flow (Janot's ENFLW) (.FALSE. = failure)
C?.NXLEPT: Flag for LEPTAG (T = OK)
C?.NXQVSR: Flag for QVSRCH (F = OK / T = minor error)
C?.NXQVSS: Flag for QVSRCH (F = OK / T = serious error)
C?.NXSRCH: Flag for QVSRCH (F failed to run QVSRCH)
C?.NXGIPT: T = event useful for QIPBTAG)
C?.NXIPBT: Flag for QIPBTAG (T = OK)
C?.NXGAMP: Flag for GAMPEX (T = OK)
C?.NXPGAC: Flag for GAMPEX (T = GAMPECK/ F = GAMPEX)
C
C?.NXQFVX: Flag for QFNDIP vertex (T=QFNDIP,F=JULIA)
C?.NXQFFA: Flag for QFNDIP failed (T=failed,F=OK)
C
C!.MCOPTI: options used for Monte-Carlo production
C!.        bit  0 set if real data
C!.        bit  1 = ISEL : gluons selection ( 1 = keep gluons )
C!.        bit  2 = ISIN : flag for using the single particle mode
C!.                             = 1 read the following flags
C!.                             = 0 ignore the following flags
C!.                        When you select a single particle, only that
C!.                        final particle and the quark will be kept
C!.                        in the NDMC bank, for those events of the
C!.                        chosen flavor. For the flavors where the
C!.                        flag is 0, all tracks are kept.
C!.        bit  3 = IPI  : pion
C!.        bit  4 = IKA  : kaon
C!.        bit  5 = IMU  : muon
C!.        bit  6 = IEL  : electron
C!.        bit  7 = INI  : neutrino
C!.        bit  8 = IGA  : gamma
C!.        bit  9 = IPR  : proton
C!.        bit 10 = INE  : neutron
C!.        bit 11 = IUQ  : u quark
C!.        bit 12 = IDQ  : d quark
C!.        bit 13 = ISQ  : s quark
C!.        bit 14 = ICQ  : c quark
C!.        bit 15 = IBQ  : b quark
C!.        bit 16 = ITQ  : t quark
C!.PMINMC: minimum momentum for MC tracks in the fragmentation
C!.        (-1. for real data)
C!.LFLAVR: event flavor (7 = no FKIN bank)
C
C!.NXYRMI: if T => perform V0 search with XRMIST package
C!.NYRNPF: number of parameter for the fit (6,7,8 or 9)
C
C!.REFMIN: minimum energy for object to be considered for energy flow
C!.RMYCUT: mass cut for ENFLW jets (QJMMCL algorithm)
C!.THRUST: Thrust
C
C!.NXRQEI: if T => run QEIDO (if JULIA<254)
C?.MAXimum number of ELectron or MUon Candidates, JETs, photons, TRacKs,
C?.Monte Carlo Tracks, V0 Candidates
C
C?.NCORSP: correction file used for production of current nanoDST input
C?.        file
C
C!.IFILL : Fill number
C!.BEAMX : X,Y,Z values of beam crossing for this run , in [cm]
C!.DBEAMX: Errors on BEAMX , in [cm]
C!.NHADBX: Number of HADronic events used to compute BeamX , dbeamx
C?.End of NPCDSH -------------------------------------------------------
C
C
C?.Start of NCDESH -----------------------------------------------------
C
C!.Mathematical and physical constants
C!.QQPI   = pi
C!.QQE    = e
C!.QQ2PI  = 2 * pi
C!.QQPIH  = pi / 2
C!.QQRADP = 180 / pi
C!.QQC    = speed of light [cm/s]
C!.QQH    = Planck constant / ( 2 * pi ) [GeV/s]
C!.QQHC   = QQH * QQC
C!.QQIRP  = speed of light [cm/KGauss]
C
C!.Logical statement functions (defined in NMACRO)
C!.XSIG(I)   : T if relative error on momentum available for track I
C!.XSIGOV(I) : T if relative error on momentum OVerflow for track I
C!.XTEX(I)   : T if dE/dX information available for track I
C!.XEID(I)   : T if Electron estimators available for track I
C!.XUID(I)   : T if mUon estimators available for track I
C!.XPART(I,J): T if particle hypothesis bit J TRUE for track I
C!.            J = 1 : electron
C!.            J = 2 : muon
C!.            J = 3 : pion
C!.            J = 4 : kaon
C!.            J = 5 : proton
C!.XEL(I)    : T if electron hypothesis bit TRUE for track I
C!.XMU(I)    : T if muon hypothesis bit TRUE for track I
C!.XPI(I)    : T if pion hypothesis bit TRUE for track I
C!.XKA(I)    : T if kaon hypothesis bit TRUE for track I
C!.XPR(I)    : T if proton hypothesis bit TRUE for track I
C!.XFRF(I)   : T if CHI square per d. o. f. is available for track I
C!.XC2OV(I)  : T if CHI square per d. o. f. OVerflow for track I
C!.XMCNE(I)  : T if this MC "track" is a neutral particle
C!.XMCGA(I)  : T if this MC "track" is a gamma
C!.XMCCH(I)  : T if this MC "track" is a charged particle
C!.XRIMOV(I) : T if measured ionisation OVerflow for track I
C!.XRSIOV(I) : T if rel. error on the dE/dx OVerflow for track I
C!.XV0SVA(I) : T if ambiguous secondary vertex (V0s only)
C!.XV0KIA(I) : T if ambiguous with another hypothesis (V0s only)
C!.XV0TRA(I) : T if V0 shares a track with another V0 (V0s only)
C!.XK0(I)    : T if V0 fulfills kaon hypothesis (V0s only)
C!.XLA(I)    : T if V0 fulfills lambda hypothesis (V0s only)
C!.XAL(I)    : T if V0 fulfills antilambda hypothesis (V0s only)
C!.XGA(I)    : T if V0 fulfills gamma hypothesis (V0s only)
C
C?.MAXimum number of input FILes
C
C?.MAXimum number of "tracks" (rows in NVEC)
C
C!.Input/Output UNITS
C!.KUCARD : CARD input = 7
C!.KUPRNT : log file = 6
C!.KUPTER : terminal = 76 or 0 (if batch)
C!.KUSEVT : logical unit for the SEVT cards file = 18
C!.KUHIST : HISTogram output = 19
C
C?.POINTers
C?.KOEVEH : Origin of EVEH bank
C?.KCEVEH : number of words per row in EVEH
C?.KOQVEC : Origin of QVEC bank
C?.KCQVEC : number of words per row in QVEC
C?.KONVEC : Origin of NVEC bank
C?.KCNVEC : number of words per row in NVEC
C?.KONDAR : Origin of NDAR bank
C?.KCNDAR : number of words per row in NDAR
C?.KONDBM : Origin of NDBM bank
C?.KCNDBM : number of words per row in NDBM
C?.KONDCL : Origin of NDCL bank
C?.KCNDCL : number of words per row in NDCL
C?.KONDDE : Origin of NDDE bank
C?.KCNDDE : number of words per row in NDDE
C?.KONDEJ : Origin of NDEJ bank
C?.KCNDEJ : number of words per row in NDEJ
C?.KONDGC : Origin of NDGC bank
C?.KCNDGC : number of words per row in NDGC
C?.KONDHE : Origin of NDHE bank
C?.KCNDHE : number of words per row in NDHE
C?.KONBIP : Origin of NBIP bank
C?.KCNBIP : number of words per row in NBIP
C?.KONDJT : Origin of NDJT bank
C?.KCNDJT : number of words per row in NDJT
C?.KONDLV : Origin of NDLV bank
C?.KCNDLV : number of words per row in NDLV
C?.KONDMS : Origin of NDMS bank
C?.KCNDMS : number of words per row in NDMS
C?.KONDOB : Origin of NDOB bank
C?.KCNDOB : number of words per row in NDOB
C?.KONDPH : Origin of NDPH bank
C?.KCNDPH : number of words per row in NDPH
C?.KONDTK : Origin of NDTK bank
C?.KCNDTK : number of words per row in NDTK
C?.KONDNT : Origin of NDNT bank
C?.KCNDNT : number of words per row in NDNT
C?.KONDV0 : Origin of NDV0 bank
C?.KCNDV0 : number of words per row in NDV0
C?.KONDCA : Origin of NDCA bank
C?.KCNDCA : number of words per row in NDCA
C?.KOPART : Origin of PART bank
C?.KCPART : number of words per row in PART
C?.KONEHE : Origin of NEHE bank
C?.KCNEHE : number of words per row in NEHE
C
C!.KFCHT : First CHarged Track
C!.KLCHT : Last CHarged Track
C
C!.KFACT : First Charged Track (including 'bad tracks' from V0s and gamma
C!convs)
C!.KLACT : Last Charged Track (including 'bad tracks' from V0s and gamma convs)
C!.KFBCT : First 'Bad' Charged Track (from V0s and gamma convs)
C!.KLBCT : Last 'Bad' Charged Track (from V0sand gamma convs)
C
C!.The event is split into 2 hemispheres which axis is the thrust axis
C!.KFPH1 : First Positive Track in Hemisphere 1
C!.KLPH1 : Last Positive Track in Hemisphere 1
C!.KFNH1 : First Negative Track in Hemisphere 1
C!.KLNH1 : Last Negative Track in Hemisphere 1
C!.KFPH2 : First Positive Track in Hemisphere 2
C!.KLPH2 : Last Positive Track in Hemisphere 2
C!.KFNH2 : First Negative Track in Hemisphere 2
C!.KLNH2 : Last Negative Track in Hemisphere 2
C
C!.KFJET : First JET
C!.KLJET : Last JET
C
C!.KFGAM : First GAMma
C!.KLGAM : Last GAMma
C
C!.KFGH1 : First GAMma in Hemisphere 1
C!.KLGH1 : Last GAMma in Hemisphere 1
C!.KFGH2 : First GAMma in Hemisphere 2
C!.KLGH2 : Last GAMma in Hemisphere 2
C
C!.KFMCT : First Monte-Carlo Track
C!.KLMCT : Last Monte-Carlo Track
C!.KFMUT : First Monte-Carlo Unstable Track
C!.KLMUT : Last Monte-Carlo Unstable Track
C!.KFMST : First Monte-Carlo Stable Track
C!.KLMST : Last Monte-Carlo Stable Track
C
C!.KFV0T : First V0 (bank NDV0)
C!.KLV0T : Last V0 (bank NDV0)
C
C!.KFGCO : First converted gamma (bank NDGC)
C!.KLGCO : Last converted gamma (bank NDGC)
C
C!.KFEFT : First EFLOW object (bank NDOB)
C!.KLEFT : Last EFLOW object (bank NDOB)
C
C!.KTHRU : Thrust axis
C
C!.KQ    : Initial quark (direct daughter of the Z0)
C!.KQBAR : Initial anti-quark (direct daughter of the Z0)
C!.KFPAR : First final parton
C!.KLPAR : Last final parton
C
C?.VARIAbles :
C!.KRUN  : RUN number
C!.KEVT  : EVenT number
C!.KNEVT : Number of EVenTs processed
C!.KNCHT : Number of CHarged Tracks
C!.KNACT : Number of Charged Tracks (including 'bad' tracks from V0s)
C!.KNBCT : Number of 'Bad' Charged Tracks (from V0s)
C!.KNPH1 : Number of Positive Tracks in Hemisphere 1
C!.KNNH1 : Number of Negative Tracks in Hemisphere 1
C!.KNPH2 : Number of Positive Tracks in Hemisphere 2
C!.KNNH2 : Number of Negative Tracks in Hemisphere 2
C!.KNJET : Number of JETs (in bank NDJT)
C!.KNGAM : Number of GAMmas (in bank NDPH)
C!.KNGH1 : Number of GAMmas in Hemisphere 1
C!.KNGH2 : Number of GAMmas in Hemisphere 2
C!.KNLEC : Number of LEpton Candidates (in bank NDCL)
C!.KNMUT : Number of Monte-Carlo Unstable Tracks (in bank NDNT)
C!.KNMST : Number of Monte-Carlo Stable Tracks (in bank NDMS)
C!.KNMCT : Number of Monte-Carlo Tracks
C!.KNV0T : Number of V0 candidates in NDV0
C!.KNGCO : Number of converted gammas in NDGC
C!.KNEFT : Number of EFLOW objects in NDOB
C!.KNPART: Number of particles in PART
C!.QTHRU : Value of thrust
C!.QTTHE : Theta of thrust axis
C!.QTPHI : Phi of thrust axis
C!.KRUNR : RUN number from the run bank RUNH
C?.KNREIN: number of records read in for the current input file
C?.KNEFIL: number of events read in for the current input file
C
C!.QELEP : LEP center of mass Energy [GeV/c**2]
C!.QMFLD : Magnetic FieLD [kG]
C!.KREVDS: Detector status word from REHV bank
C!.QMCUT : Current mass cut value for the jets
C!.QSMCUT: Mass cut value for the jets at beginning of event analysis
C!.QSYCUT: Ycut value for the jets at beginning of event analysis
C
C?.NCHIST: name of histogram output file
C
C?.NXWRHI: T if histogram output file wanted
C?.NXMASS: if T the tracks are given mass according to first
C?.        particle hypothesis bit set
C!.XLUMOK: T if event used to compute luminosity
C!.XMCEV : T if Monte-Carlo event
C?.NXNEWF: T for new file (triggers a call to NEWFIL
C?.NXNEWR: T for a new run, when the run record has not yet been
C?.        written out
C?.NXOUTP: T if event output is wanted (a FILO card has been found)
C?.NXCOPY: T if COPY job
C?.NXJSPY: T if the job name should be changed (VAX only)
C?.NXWOUT: T if the event should be written out
C?.NXNOPH: T if histogram printing not wanted
C?.NXEVOL: T if the jets have to be evolved before calling NUEVNT
C?.XNSEQ:  T if NSEQ card present
C
C!.KFLAV : event FLAVor
C!.NJETB : jet bank to be used (default=0 => NDJT)
C?.NFFREE: First FREE track in bank NVEC
C?.NRONVE: Number of ROws in bank NVEC
C
C?.ICARDS: contains run and event number of selected events for which
C?.        SEVT cards are to be produced. 1=KRUN 2=KEVT
C?.NSELEV: number of selected events
C?.MAXSEV: maximum number of events selected for SEVT cards
C
C?.NVERSP: version used for production of current nanoDST input file
C?.NVERSS: version of SANDY running
C?.NCORSS: SANDY correction file
C
C?.SAVERS: SANDY version
C
C!.QPI0DO (J.-P. Lees):
C!.IQPI0           : return code  0-->OK, 1-->0, 2-->N pi0 > MXPI0
C!.PIOMOM(4,MXPI0) : PI0 refitted 4 momentum
C!.IPI0GAM(2,MXPI0): gam 1 & 2 number in the GAM section
C!.CHIPI0(MXPI0)   : chi2 value after refit (-999. if no convergence)
C!.ITYPI0(MXPI0)   : pi0 type, see below
C!.Description of pi0 types:
C!. TY=1: 2 photons in same PECO, with N=2 photons in the PECO
C!. TY=2: 2 photons in same PECO, with N>2 photons in the PECO
C!. TY=3: 2 photons in different PECO, with N=1 photons in each PECO
C!. TY=4: 2 photons in different PECO, with N>1 photons in one PECO
C?.End of NCDESH -------------------------------------------------------
C
C?.NORDER:INTERNAL USE
C?.On Nth row of NDTK, you have the ISTORE(N) row of common NCOMTK,
C?.or the IALPHA(ISTORE(N)) ALPHA track
C!======================================================================