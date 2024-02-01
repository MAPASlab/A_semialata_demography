//Parameters for the coalescence simulation program : fastsimcoal2.exe
4 samples to simulate :
//Population effective sizes (number of genes)
$NPOP0$
$NPOP1$
$NPOP2$
$NPOP3$
//Samples sizes and samples age 
20
20
20
20
//Growth rates	: negative growth implies population expansion
0
0
0
0
//Number of migration matrices : 0 implies no migration between demes
3
//Migration matrix 0: current migration
0 0 0 0
0 0 $MIG12$ 0
0 $MIG21$ 0 0
0 0 0 0
//Migration matrix 1: migration between ancestral pops
0 0 $MIG02$ 0
0 0 0 0
$MIG20$ 0 0 0
0 0 0 0
//Migration matrix 2: no migration
0 0 0 0
0 0 0 0
0 0 0 0
0 0 0 0
//historical event: time, source, sink, migrants, new deme size, new growth rate, migration matrix index
9 historical event
$TCHG0$ 0 0 0 $NCHG0$ 0 0 absoluteResize
$TCHG1$ 1 1 0 $NCHG1$ 0 0 absoluteResize
$TCHG2$ 2 2 0 $NCHG2$ 0 0 absoluteResize
$TCHG3$ 3 3 0 $NCHG3$ 0 0 absoluteResize
$TDIV01$ 1 0 1 $NPOP01$ 0 1 absoluteResize
$TDIV01$ 1 1 0 0 0 1 // deme 1 is killed
$TDIV23$ 3 2 1 $NPOP23$ 0 1 absoluteResize
$TDIV23$ 3 3 0 0 0 1 // deme 3 is killed
$TDIV$ 2 0 1 $NANC$ 0 2 absoluteResize
//Number of independent loci [chromosome] 
1 0
//Per chromosome: Number of contiguous linkage Block: a block is a set of contiguous loci
1
//per Block:data type, number of loci, per generation recombination and mutation rates and optional parameters
FREQ 1 0 1.0e-8 OUTEXP
