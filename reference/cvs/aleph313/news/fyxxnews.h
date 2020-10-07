CKEY FYXX MCARLO KINE
C! FYXX package : build FYxx banks (kinematics)
 The FYXX package translates and filters MONTE CARLO tracks from one
 set of banks (KINE,VERT,KHIS,KPOL,KVOL) to another set
 (FKIN,FVER,FPOI,FPOL)  which is more convenient for analysis. A few
 utility routines are also included.
 When the package is used to
 translate KINE etc. to FKIN etc. certain types of 'uninteresting'
 tracks may be dropped: those tracks which originate
 in the ITC electronics or TPC endplate, tracks which originate in
 the calorimiters, and tracks whose momenta are below a certain
 threshold. Certain tracks will be kept regardless of these cuts,
 namely tracks which came from the event generator.

 The FYXX package must be initialized with a call to FYIRUN :
      CALL FYIRUN(0.,TKDRO,SHDRO,CUTRA)
 INPUTS:
       TKDRO       = IF true THEN drop history of interactions
                     in ITC electronics and TPC endplate
       SHDRO       = IF true THEN drop history of interactions
                     in calorimeters
       CUTRA       = IF not 0. THEN drop track with momentum
                     below CUTRA

  and for each event there must be a call to FYKINE :
      CALL FYKINE(DROP,GARB,IBFUL)
 - input arguments :
        DROP     = character flag
                   if = 'DROP' then drop KINE,VERT,KHIS,KVOL,KPOL banks
        GARB     = character flag
                   if = 'GARB' then make a garbage collection
 - output argument :
        IBFUL    = -1 means not enough space in BOS array

 ! 980313 corr.file no.5 to ALEPHLIB 30.7
    FYKILL : Don't kill vertices in TPC passive material at large angles
             (membrane et al., resistor chain, laser mirrors and prisms)
                                                                (S.Wasserbaech)
 
 ! 980401 corr.file no.6 to ALEPHLIB 30.7
    FYFKIN : Fill FSHO monte carlo bank from KSHO bank (B.Bloch)
    FYIRUN : Add FSHO bank to E list                   (B.Bloch)
    FYKINE : Drop also KSHO bank                       (B.Bloch)
    FYTOKI : Handle also KSHO/FSHO banks               (B.Bloch)

 ! ALEPHLIB 30.3
    FYRELA.H : Increase MAXMCX to 2000 (from 1000) to cope with PHOT02
               events with too many KINE banks                   (W.Manner)

 ! 960823 corr.file no.1 to ALEPHLIB 21.5
    FYTREE - reset the number of kept tracks if some tracks were kept
             without the vertex origin.
    FYFKIN - print FKIN/FVER when FYDEB flag is defined.

 ! ALEPHLIB 21.5
   FYIRUN : Fix a string concatenation (M.Cattaneo 22-July-96)

 ! ALEPHLIB 21.3
   FYRELA.H : Remove # as first character of DOC line, breaks CPP

 ! ALEPHLIB 20.5
   FYINEV : remove reference to ELISAD variable which was not set.

 ! ALEPHLIB 15.6
   FYKILL : Correction for very rare cases giving a faulty FKIN bank
            (J.Boucrot)

