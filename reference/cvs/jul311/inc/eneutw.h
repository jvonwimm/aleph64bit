      COMMON / ENEUTW / CLUREN(3,3,3),JFMXEN,ITMXEN,KMAXEN,MDTMEN,
     + NRTMEN,LRTMEN,NVTMEN,KRTMEN,KSTMEN,CRACEN(3),PRSMEN,
     + KLASEN,ETAGEN(6),PROBEN(3),CHILEN,KTYPEN(2),RAPOEN,HCLTEN,
     + SOM4EN,XGEREN(3),TETAEN,PHIZEN,UGEREN(3),SMGAEN,COSIEN,AINEEN,
     +   IBONEN,ALPHEN,BETAEN,BESAEN,UNSAEN,ALONEN,ECLTEN,ECLSEN(3),
     +   STTUEN,ST1UEN,ST2UEN,ST3UEN,TBAREN,PBAREN,SIGPEN
#if defined(DOC)
C
C!  Neutral shower parameters from a neutral cluster.
C
C   CLUREN : 3 by 3 towers around max.
C   JFMXEN : Max. storey PHI
C   ITMXEN : Max. storey THETA
C   KMAXEN : Max. storey stack
C   MDTMEN : Max. storey module
C   NRTMEN : Max. storey region
C   LRTMEN : Max. storey at the limit of 2 regions.
C   KRTMEN : Max. storey near a crack.
C   NVTMEN : Overlap code.
C   KSTMEN : Crack in the "secondary detector".
C   CRACEN : 3 words useful to treat the cracks.
C   PRSMEN : Materialisation probability.
C   KLASEN : Stacks content identification
C   ETAGEN : Staks limits in rad. lengths.
C   PROBEN : Identification parameters.
C   CHILEN : Identification Chi2.
C   KTYPEN : Identified type.
C   RAPOEN : Distance from shower origin to ALEPH center.
C   HCLTEN : Corrected energy if hadron.
C   SOM4EN : Fraction of signal in 4 towers.
C   XGEREN : Starting point in the absolute system.
C   TETAEN : Line of flight , teta angle.
C   PHIZEN : Line of flight , phi angle.
C   UGEREN : Normalised direction.
C   SMGAEN : Depth of materialisation in rad. lengths.
C   COSIEN : Cos(incidence angle)
C   AINEEN : 1 / COSIEN - 1
C   IBONEN : Parameters determination succesful if IBONEN = 0
C   ALPHEN : Alpha parameter deduced from stacks content.
C   BETAEN : Beta parameter deduced from stacks content.
C   BESAEN : BETAEN / ALPHEN
C   UNSAEN : 1 / ALPHEN
C   ALONEN : Normalisation to 1 of the longitudinal distribution.
C   ECLTEN : Corrected energy.
C   ECLSEN : Energy per stack.
C   STTUEN : Calculated energy.
C   ST1UEN : Calculated energy stack 1
C   ST2UEN : Calculated energy stack 2
C   ST3UEN : Calculated energy stack 3
C   TBAREN : Theta barycenter angle.
C   PBAREN : Phi barycenter angle.
C   SIGPEN : Transverse estimator.
#endif
