C! YTOP summary
      COMMON /YSUMTO/NEPVTO,NRPVTO,ATPVTO,AMPVTO,ACPVTO,APPVTO(3),
     +               NRCOTO,AMCOTO,ACCOTO,ARCOTO,
     +               NRK0TO,AMK0TO,ACK0TO,ATK0TO,
     +               NRLATO,AMLATO,ACLATO,ATLATO,
     +               NRLBTO,AMLBTO,ACLBTO,ATLBTO,
     +               KYFLAG(20)
#if defined(DOC)
C     NEPVTO ...... nb of events processed
C                   by the TOPOLOGY primary vertex routine
C     NRPVTO ...... nb of events with a reconstructed primary
C                   vertex by the TOPOLOGY routine
C     ATPVTO ...... average nb of trakcs in the primary vertex
C     AMPVTO ...... average momentum of tracks in primary vertex
C     ACPVTO ...... average normalized chi2 of primary vertex
C     APPVTO ...... average primary vertex position : x,y,z
C     NRCOTO ...... nb of photon conversions
C     AMCOTO ...... average momentum of converted photon
C     ACCOTO ...... average chi2 of conversion vertex
C     ARCOTO ...... average conversion radius
C     NRK0TO ...... nb of K0's found
C     AMK0TO ...... average momentum of K0
C     ACK0TO ...... average chi2 of K0 vertex
C     ATK0TO ...... average K0 lifetime
C     NRLATO ...... nb of Lambdas found
C     AMLATO ...... average momentum of Lambda
C     ACLATO ...... average chi2 of Lambda vertex
C     ATLATO ...... average Lambda lifetime
C     NRLBTO ...... nb of Lambda-bar's found
C     AMLBTO ...... average momentum of Lambda-bar
C     ACLBTO ...... average chi2 of Lambda-bar vertex
C     ATLBTO ...... average Lambda-bar lifetime
C     KYFLAG ...... summary of messages written during a run
C          1 ...... beam crossing not found in data base, use defaults
C          2 ...... problem in PIDAS : particle assignment
C          3 ...... problem in YTSTRK : track selection
C          4 ...... problem in YTCONV : gamma conversion search
C          5 ...... problem in YTRV0S : V0 search
C          6 ...... problem in YTOPVT : no primary vertex found
C          7 ...... problem in YTOSVT : secondary vertex search
C          8 ...... problem in YTOHIS : PYER bank not found
C          9 ...... problem in YTOPVT : no M-V hits bank found
C         10 ...... YTOPVT : no primary vertex reconstructed
C         11 ...... problem in YTOPVT : no space to create PYFR
C         12 ...... problem in YTOPVT : no space to create PYER
C         13 ...... problem in YTOPVT : no FRFT bank found
C
#endif
