# Relationship between reward-related brain activity and opportunities to sit


## R script
This repository contains an `R` script "Reward_Positivity_Data_Analysis.R" to analyze reward associated with sitting down or squatting while undergoing electroencephalographic (EEG) recording.

## Data for analyses in R
The data used in the R script is available in CSV format in the "Data_R_models" directory.

## Raw behavioral data
The raw behavioral data for each participant is available in the "Data" directory. Each participant has two logfiles from which behavioral data were extracted. The larger logfile contains details about each event in every trial and can be ignored. The smaller logfile contains the subject name (subject), which stimulus was chosen (stim_chosen), whether the trial was a sit or stand trial (instruct), and whether a reward was received (reward). The other variables can be ignored. A separate directory contains a spreadsheet with the questionnaire data. Data for the main experiment begins with Row 14. The relevant columns (variables) include: Subject ID, Weight, Height, Age, Sex, custom fatigue questions (pre-study), Multidimensional Fatigue Inventory questions, custom fatigue questions (post-study), Rating of Perceived Exertion (RPE) for Sit Trials, RPE for Stand Trials, Preference for Standing vs. Sitting, International Physical Activity Questionnaire (IPAQ) items for a typical week, IPAQ for the present day, exercise attitudes questions, exercise dependence question, and awareness of sit stimulus probability manipulation.

## signal processing record
The "signal processing record.xlsx" spreadsheet located in the main direcctory provides notes about EEG signal processing, with each row representing a subject and columns representing: subject number, notes about EEG, any interpolated electrodes, when the reward positivity window began and ended, and how many trials were included for each condition.

## EEG data
There are three EEG directories. The "EEG_raw_data" directory contains the three files for each participant (.eeg, .vmrk, .vhdr). The "EEG_processing_history_files" directory contains the files to process each subject’s raw EEG and to process grand average event-related potentials (.ehst2, .hfinf2). The "EEG_exported_data_files" directory contains files for each participants’ independent component analysis (ICA) (and reverse ICA), for example “ss_64” and “ss_64_i”, respectively. Each participant also has a file (“ss_64_Artifact_Marking_All_Peaks”) that shows the amplitude of the reward positivity for each trial at each midline electrode. Some participants have two such files, one for the reward positivity calculated from a window beginning at 230 ms, and one for the reward positivity calculated from a window beginning at 250 ms.

## Material
The "Material" directory contains the files required to run the Presentation software script that presents the experimental stimuli.
