# EDA Bioassay Analysis
Workflow description
Imane Al Gharib (2021)

Workflow and parameter description for the analysis of biological screening data.

Before an analysis can be run, a working directory must be set using the 'setwd()' function or via the Session menu in the menu bar.
This sets the default location of any files that are read into R and where the results will be saved. In this active working directory,
there must be a YAML configuration file (config.yml), an R file with the main function to run the analysis (run_analysis.R), a folder named "scripts",
which contains the necessary scripts to run the analysis and the template for the HTML report, and the data that will be analyzed.

In order to run the analysis, certain parameters must be defined in the configurtion file (config.yml). Once the parameters are defined,
you simply run the 'run_analysis.R' script and the workflow analysis of bioassay data will be performed from start to finish. The results of the analysis
are saved in a result directory that is created in the working directory. The results comprise a HTML report with the results, such as figures and tables,
the individual figures as PNG-files, an Excel file with the fitted procedure blank data and their corresponding details, such as the residuals and the SD,
and if hits are selected, an Excel file with the identified hits and their corresponding details.

In the configuration file (config.yml), the parameters 'pb_fm_file', 'pb_p_file', 'directory_name' and 'ylab' must be provided. If these parameters are provided, the analysis
will only visualize and model the procedure blank, and return its results. The remaining parameters are optional.
Make sure that there is always a space between the colon and the argument that you provide. A few parameters allow multiple arguments, including 'sample_fm_file',
'sample_p_file', 'sample_plate_id', 'neg_control' and 'pos_control'. Multiple arguments should be defined in the following manner:
parameter:
   - argument 1
   - argument 2
Make sure only spaces are used as indentation.

Each parameter of the configuration file (config.yml) is explained below:

pb_fm_file: path to the fractiomate file of the procedure blank. This must be a Text file (.txt).

pb_p_file: path to the platereader file of the procedure blank. This could be a Text file (.txt) or an Excel file (.xls/.xlsx) from the Cytation 5
multimode platereader. If an Excel file (.xls/.xlsx) is provided, a design file is required, which can be defined at parameter 'design_file'.
The platereader Excel file parser assumes that each plate is given in matrix form, of which the row and column indices are also indicated, with an
additional column on the right of the plate matrix containing the wavelength. At this point, the platereader Excel file parser does not accept
data of which each plate has been measured more than once (iteration > 1); it only accepts data containing single measurements of each plate.

directory_name: name of the result directory that will be created in the working directory by the script itself. Here, results of the analysis are saved.
This must be a non-existent directory or else the execution of the analysis stopped.

report_title: title of the report that is created (e.g. TTR-Binding Assay - Serum)

design_file: path to the design file that must be provided with the platereader Excel file at 'pb_p_file'. This must be an Excel file (.xls/.xlsx) as well.
The design file must be a file that contains at least three columns: m_order, m_plate and m_levels.
   - m_order is the column containg the positionnumbers of the plates in the file.
   - m_plate is the column containing the plate IDs, which must correspond to the positionnumbers of the plates as given in order. Procedure blank plates must always have plate ID 'PB'.
   - m_levels is the column containing what is being measured in a plate, which must at least have the level 'target'. 'background' represent the plates in which only the
   background fluorescence intensity is measured. 'target' represent plates in which the target molecule is added to measure the binding of the
   fluorescein labeled ligand to the target molecule. The 'background' plates will be used to correct for background fluorescence by subtracting it from the 'target' plates. If 'background'
   plates are not present in the data, 'target' values are used for further analysis without correction.
   It is important, that the number of measurements in the design file corresponds with the number of plates in the platereader file (pb_p_file).
Example of the design file format:
      m_order   m_plate   m_levels
      4         3         buffer
      5         PB        background
      10        1         target
This means that plate number 5 in the file contains the procedure blank (PB) with added fluorescein (background) and plate number 10 contains the sample 1  with the added target molecule.

ylab: Label for the y-axes of the plots, which is usually what is measured (e.g. "FITC bound to TTR" or "Cell viability").

sample_fm_file: path to the fractiomate file(s) of the environmental sample of which hits should be identified. Must be provided if 'sample_p_file' is provided.
Multiple arguments are allowed. The path to the fractiomate file of each replicate must then be provided. In the YAML configuration file, replicates can be stated as follows:
sample_fm_file:
   - data/Antibiotics assay/20201216_FractioMate_112957_EFF1 AB.txt
   - data/Antibiotics assay/20201216_FractioMate_120405_EFF2 AB.txt
Use spaces as indentation!

sample_p_file: path to the platereader file(s) of the environmental sample of which hits should be identified. If pb_p_file is an Excel file (.xls/.xlsx) that also
contains the platereader data of the environmental sample, a sample_p_file is not required. Otherwise, this must be a Text file (.txt). Multiple arguments are allowed.
The path to the platereader file of each replicate must then be provided; they can be stated in a similar way as explained above at 'sample_fm_file'.

sample_plate_id: plate ID of the environmental sample(s). Should be provided if environmental samples are analyzed for hit selection. For platereader files in Excel format,
sample_plate_id should correspond to plate IDs in the design file. Multiple arguments are allowed.

IMPORTANT! Make sure to always supply the sample_fm_file, sample_p_file (if necessary) and plate IDs in the same order. Thus, if you have supplied sample fractiomate files,
for example, in the order Plate 2 - Plate 3 - Plate 1, the sample platereader files and plate IDs that correspond with these sample fractiomate files should be supplied in the same order.
In addition, no more than 6 samples or replicates can be analyzed at once in order to preserve the quality of the figures.

sd_threshold: the threshold of the number of standard deviations that is used to identify bioactive hits. Defaults to 3.

plate_threshold: the minimum number of replicates in which a fraction should be selected as hit in order to be considered a hit. Should only be applied if replicates
are being analyzed. Defaults to NULL.

normalization: whether or not to normalize the data (defaults to FALSE, which means data is not normalized by default).
   - If TRUE, negative and/or positive control well positions on the plate should be provided.
   - If only negative control well positions are provided, the data is normalized to the negative control:
     (RO_well/mean(RO_neg_control) * 100).
   - If only positive control well positions are provided, the data is normalized to the positive control:
     (RO_well/mean(RO_pos_control) * 100).
   - If both negative and positive control well positions are provided, the data is standardized:
     ((RO_well - mean(RO_pos_control))/(mean(RO_neg_control) - mean(RO_pos_control)) * 100).

neg_control: negative control positions on the plate (e.g. "A1" and "A2"). If there are multiple negative control positions,
state them like this:
neg_control:
   - A1
   - A2
Use spaces as indentation!

pos_control: positive control positions on the plate (e.g. "G1" and "G2"). Multiple arguments allowed.

iteration_time: time (in seconds) between measurements of the same plate (defaults to 60). Is only used if multiple iterations are present in the platereader Text file (.txt).

iteration1: first number of the interval of the iterations that should be used to calculate a slope (e.g. Resazurin conversion,
            1 by default). Is only used if multiple iterations are present in the platereader Text file  (.txt).

iteration2: second number of the interval of the iterations that should be used to calculate a slope (e.g. Resazurin conversion,
            4 by default). Is only used if multiple iterations are present in the platereader Text file  (.txt).
