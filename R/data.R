
#' Database of cytotoxicity
#'
#' This database contains three items: the Experiment list, the dataset with the technical replicates and the summarized dataset.
#'
#' @format A list with 3 dataset:
#' \itemize{
#' \item{\strong{myprocesseddata}}
#' \describe{
#' A dataset with 33,053 rows and 16 columns:
#'   \item{Well}{Well id}
#'   \item{Experiment_id}{The experiment id. e.g. "IEXP62"}
#'   \item{Support_type}{The support type (P100)}
#'   \item{Product_Family}{The Product Family (e.g. CBC190A)}
#'   \item{Product}{The Product (e.g. CBC190A_B)}
#'   \item{Purification}{A letter that differentiate the Purification}
#'   \item{Model_type}{The Model type (e.g. CALU_3)}
#'   \item{Model_Family}{The Model Family (e.g. LC)}
#'   \item{Dose}{The dose (numeric)}
#'   \item{Dose_unit}{The Dose unit (e.g. ug_ml)}
#'   \item{Tec_Replicate}{Id of the Technical Replicates}
#'   \item{Status}{If the product is Released or Retired}
#'   \item{Absorbance}{Value of Absorbance}
#'   \item{Corrected_value}{Value of Corrected value}
#'   \item{Cytotoxicity}{Value of Cytotoxicity}
#'   \item{Vitality}{Value of Vitality}
#'   }
#'  \item{\strong{mydataset}}
#'  \describe{
#'  A dataset with 16,536 rows and 13 columns:
#'   \item{Experiment_id}{The experiment id. e.g. "IEXP62"}
#'   \item{Product_Family}{The Product Family (e.g. CBC190A)}
#'   \item{Product}{The Product (e.g. CBC190A_B)}
#'   \item{Purification}{A letter that differentiate the Purification}
#'   \item{Model_type}{The Model type (e.g. CALU_3)}
#'   \item{Model_Family}{The Model Family (e.g. LC)}
#'   \item{Dose}{The dose (numeric)}
#'   \item{Cytotoxicity.nreps}{Number of averaged Technical Replicates}
#'   \item{Cytotoxicity.average}{Averaged Cytotoxicity}
#'   \item{Cytotoxicity.sd}{Standard deviation of Cytotoxicity}
#'   \item{Vitality.average}{Averaged Vitality}
#'   \item{Corrected_value}{Value of Corrected value}
#'   \item{CV}{Coefficient of variation}
#'  }
#'  \item{\strong{exp_list}}
#'  \describe{
#'   \item{Experiment_id}{The experiment id. e.g. "IEXP62"}
#'   \item{Instrument}{Instrument used. "EZ_READ_2000" or "4300_CHROMATE_PLATE_READER"}
#'   \item{Scan}{If the scan is "Single" or "Double".}
#'   \item{Wavelength}{Wavelength used (e.g. A490)}
#'   \item{Path}{The path of the file used}
#'   \item{File}{The file name (e.g. P100.xlsx)}
#'  }
#' }
#' @source \url{...}
"database_cyto"




#' Database of D1
#'
#' This database contains three items: the Experiment list, the dataset with the technical replicates and the summarized dataset.
#'
#' @format A list with 3 dataset:
#' \itemize{
#' \item{\strong{myprocesseddata}}
#' \describe{
#' A dataset with 1,260 rows and 21 columns:
#'   \item{Well}{Well id}
#'   \item{Cytotoxicity}{Value of Cytotoxicity}
#'   \item{Vitality}{Value of Vitality}
#'   \item{CD80}{value of marker}
#'   \item{CD40}{value of marker}
#'   \item{`MHC-II`}{value of marker}
#'   \item{Experiment_id}{The experiment id. e.g. "IEXP62"}
#'   \item{Experiment_type}{Type of experiment (e.g. IMMUNEPHENOTYPE)}
#'   \item{Analysis_type}{Type of analysis (e.g. bioassay)}
#'   \item{Instrument}{Instrument used. "BD_ACCURI_C6" or "MACS_QUANT_16"}
#'   \item{Instrument_type}{The instrument type (e.g. FACS)}
#'   \item{Support}{The support used (96_well_plate)}
#'   \item{Product_Family}{The Product Family (e.g. CBC190A)}
#'   \item{Product}{The Product (e.g. CBC190A_B)}
#'   \item{Purification}{A letter that differentiate the Purification}
#'   \item{Model_type}{The Model type (e.g. CALU_3)}
#'   \item{Model_Family}{The Model Family (e.g. LC)}
#'   \item{Dose}{The dose (numeric)}
#'   \item{Dose_unit}{The Dose unit (e.g. ug_ml)}
#'   \item{Tec_Replicate}{Id of the Technical Replicates}
#'   \item{Status}{If the product is Released or Retired}
#'   \item{Organism}{The organism (e.g. "mouse")}
#'   }
#'  \item{\strong{mydataset}}
#'  \describe{
#'  A dataset with 648 rows and 12 columns:
#'   \item{Experiment_id}{The experiment id. e.g. "IEXP62"}
#'   \item{Product_Family}{The Product Family (e.g. CBC190A)}
#'   \item{Product}{The Product (e.g. CBC190A_B)}
#'   \item{Purification}{A letter that differentiate the Purification}
#'   \item{Model_type}{The Model type (D1)}
#'   \item{Dose}{The dose (numeric)}
#'   \item{Cytotoxicity.nreps}{Number of averaged Technical Replicates}
#'   \item{Cytotoxicity.average}{Averaged Cytotoxicity}
#'   \item{Cytotoxicity.sd}{Standard deviation of Cytotoxicity}
#'   \item{Cytotoxicity.CV}{Coefficient of variation for Cytotoxicity}
#'   \item{Vitality.average}{Averaged Vitality}
#'   \item{Vitality.CV}{Coefficient of variation for Vitality}
#'  }
#'  \item{\strong{exp_list}}
#'  \describe{
#'   \item{Experiment_id}{The experiment id. e.g. "IEXP62"}
#'   \item{Instrument}{Instrument used. "BD_ACCURI_C6" or "MACS_QUANT_16"}
#'   \item{Mean_MFI}{Column names for the Mean_MFI separated by a comma ","}
#'   \item{Vitality}{Column name of the vitality column}
#'   \item{Markers}{Markers used}
#'   \item{Marker_vitality}{Marker used for the vitality}
#'   \item{Color_marker}{Color marker separated by a comma ","}
#'   \item{Color_vitality}{Color marker for vitality}
#'   \item{Sections}{Section to be used separated by a comma ","}
#'   \item{Sections_Vitality}{Sections for the Vitality}
#'   \item{Path}{The path of the file used}
#'   \item{File}{The file name (e.g. D1_21.xlsx)}
#'  }
#' }
#' @source \url{...}
"database_D1"
