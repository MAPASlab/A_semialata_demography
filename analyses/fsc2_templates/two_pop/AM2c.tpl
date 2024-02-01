//Parameters for the coalescence simulation program : fastsimcoal2.exe
2 samples to simulate :
//Population effective sizes (number of genes)
$NPOP0$
$NPOP1$
//Samples sizes and samples age 
60
60
//Growth rates	: negative growth implies population expansion
0
0
//Number of migration matrices : 0 implies no migration between demes
3
//Migration matrix 0: No recent migration
0 0
0 0
//Migration matrix 1: Migration for some time after divergence
0 $MIG01$ 
$MIG10$ 0
//Migration matrix 2: No migration
0 0
0 0
//historical event: time, source, sink, migrants, new deme size, new growth rate, migration matrix index
5 historical event
$TCHG0$ 0 0 0 $NCHG0$ 0 0 absoluteResize
$TCHG1$ 1 1 0 $NCHG1$ 0 0 absoluteResize
$TMIG_STOP$ 0 0 0 1 0 1
$TDIV$ 1 0 1 $NANC$ 0 2 absoluteResize
$TCHGA$ 0 0 0 $NANCA$ 0 2 absoluteResize
//Number of independent loci [chromosome] 
1 0
//Per chromosome: Number of contiguous linkage Block: a block is a set of contiguous loci
1
//per Block:data type, number of loci, per generation recombination and mutation rates and optional parameters
FREQ 1 0 1.0e-8 OUTEXP

