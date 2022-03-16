This program computes a pairwise distance matrix from a matrix of measurements or binary data (0/1 or +/-) using the corresponding algorithm described in the paper. The user may then infer the phylogeny of the taxa in the input file using the Neighbor-joining method (Saitou and Nei, 1987), with the option of obtaining bootstrap support and rooting the tree on an outgroup. The tree can be output as an image or text (Newick format) file. If the user is interested in using the distance matrix itself (e.g., for use in another phylogenetics program), it can also be saved as a .csv, .meg (for use with the software MEGA) or .nex (for use with the software PAUP, Mesquite, etc.) file.
INSTRUCTIONS:
Input file: The input file needs to be a comma-delimited (.csv) file with rows (records) for taxa and columns (fields) for the recording criteria. PhyloM can accommodate numerical measurements as well as binary (0/1 or +/-) recording criteria, with “?” used for missing values. It is assumed that the measurement data have been normalized to allow valid comparisons among measurement criteria. The user clicks on File|Open in the main menu in the GUI, which brings up a window, Select Data Type that informs the program about the type of data being imported. Upon successfully importing the data file, the tab, Data Table displays the data in the form of a grid for easy and efficient inspection. Any formatting errors in the data trigger a message and an error log that can be saved.  In addition, the user can view the error(s) in the Data Table tab where the offending cells are flagged by means of a pair of asterisks flanking the data, as well as a pointer to the specific character/digit. 
Distance matrix: After successful import of an error-free data file, the user clicks on Analysis|Make Distance Matrix in the main menu to compute the pairwise distance matrix. This opens up a new tab, Distance Matrix, where the pairwise distances (lower triangle) can be viewed in the form of a grid. If the user wishes to use the distance matrix in another program, there are several formatting choices: .csv, MEGA (for use in MEGA (Kumar, et al., 2016), or NEXUS (for use in programs such as PAUP (Swofford, 1998), Mesquite (Maddison and Maddison, 2017), etc.)
Phylogenetic inference: After the pairwise distance matrix has been computed, the user can click on Analysis|Make Tree in the main menu for phylogenetic analysis using the Neighbor-joining method (Saitou and Nei, 1987). After clicking on the OK button in the subsequent Information message, another window opens up called Bootstrap Options. The number of bootstrap replicates can be left at the default of 100 or changed to any desired number. Upon clicking on OK, the inferred tree is brought up in the Tree Viewer.
Tree Viewer has several functionalities built into it. The File menu allows for saving the tree as an image (JPEG) or text (Newick format) file. Clicking on Root the Tree brings up the options to root the tree by changing the rectangular tree to a topology-only tree for easy viewing, assigning a label to each branch, and simultaneously bringing up another window named Select Root. This latter window lists all the branch labels and allows the user to select a root (Set Root button). The tree with the labeled branches can be viewed on the side for easy branch selection. The Select Root window has two other options. One allows the user to set the root at a desired point along the length of the chosen branch by moving the slider in Select Root to the desired position. This feature can be used if there is information on the proportional times before diversification of the two sister clades after splitting from the root, and can be useful for computing or calibrating times of divergence. Select Root also contains the option to display the rooted tree as a rectangular tree or just the topology. Clicking on Set Root after choosing the most basal internal branch allows the current tree to be viewed as a rooted rectangular tree or rooted topology only by choosing the appropriate radio button. Note that the original tree, inferred using Neighbor-joining, is unrooted. The Display menu in Tree Viewer has four options that allow the user to display the inferred unrooted tree or the rooted tree as only the topology or a rectangular tree. Tree Viewer also displays SBL (the sum of branch lengths), Tree (the type of tree currently being displayed), and Root (if rooted, the label of the branch that the tree is rooted on).
NOTE: Please report any bugs or issues with the program to the corresponding author. 

References
Kumar, S., Stecher, G. and Tamura, K. (2016), MEGA7: Molecular Evolutionary Genetics Analysis Version 7.0 for Bigger Datasets. Mol Biol Evol;33(7):1870-1874.
Maddison, W.P. and Maddison, D.R. (2017). Mesquite: a modular system for evolutionary analysis. Release 3.31. http://mesquiteproject.org.
Saitou, N. and Nei, M. (1987), The Neighbor-Joining Method - a New Method for Reconstructing Phylogenetic Trees. Mol Biol Evol;4(4):406-425.
Swofford, D.L. (1998), PAUP*: Phylogenetic Analysis Using Parsimony (and Other Methods). Sunderland, MA: Sinauer Associates.

