#' @title Create a parccvizieR object
#'
#' @description
#' \code{parccvizieR} is the primary workflow function that generates the
#' parccvizieR object
#'
#' @param results a PARCC roster, or summative file
#' @param local_roster optional.  additional roster data to supplement
#' the data in the PARCC file.
#' @param ... additional arguments
#' @examples
#'\dontrun{
#' pviz <- parccvizieR(sf = ex_sf)
#'
#' is.parccvizieR(pviz)
#' }
#' @export

parccvizieR <- function(results, local_roster = NA, verbose = FALSE, ...) {
  UseMethod("parccvizieR")
}

#' @export
parccvizieR.default <- function(results, local_roster = NA, verbose = FALSE, ...) {

  #detect file type
  result_names <- names(results)

  #process the results file
  result_type <- detect_result_type(result_names)

  if (result_type == character(0)) {
    stop("Unknown PARCC results type.")
  } else if (result_type == 'PARCC') {
    message('Detected PARCC Summative File')

    #process summative data file
  } else if (result_type == 'LA') {
    message('Detected Louisiana Roster file')

    #process louisiana data file
  }

  #return parccvizieR object
  out <- NA
  class(parccviz) <- "parccvizieR"

  out
}

detect_result_type <- function(df) {

  LA <- c("District.Code", "District.Name", "School.Code", "School.Name",
    "Last.Name", "First.Name", "Middle.Initial", "Student.ID", "Grade",
    "Gender", "Ethnicity", "Education.Classification", "Economically.Disadvantaged.Status",
    "Limited.English.Proficient", "Migrant", "Section.504", "ELA.Scale.Score",
    "ELA.Achievement.Level", "ELA.Reading.Performance", "ELA.Writing.Performance",
    "ELA.Informational.Text", "Reading.Literary.Text", "Reading.Vocabulary",
    "Written.Expression", "Written.Knowledge...Use.of.Language.Convention",
    "Math.Scale.Score", "Math.Achievement.Level", "Major.Content",
    "Expressing.Mathematical.Reasoning", "Modeling...Application",
    "Additional...Supporting.Content")

  PARCC <- c("Record Type", "Multiple Record Flag", "Reported Summative Score Flag (only for record type 01)",
    "Reported Roster Flag (only for record type 02 or 03)", "Report Suppression Code\n(only for record type 01)",
    "Report Suppression Action (only for record type 01)", "Assessment Year",
    "PBA Type 03 category", "EOY Type 03 category", "State Abbreviation",
    "Responsible District Identifier", "Responsible District Name",
    "Responsible School/Institution Identifier", "Responsible School/Institution Name",
    "PBA Testing District Identifier", "PBA Testing District Name",
    "PBA Testing School/Institution Identifier", "PBA Testing School/Institution Name",
    "EOY Testing District Identifier", "EOY Testing District Name",
    "EOY Testing School/Institution Identifier", "EOY Testing School/Institution Name",
    "PARCC Student Identifier", "State Student Identifier", "Local Student Identifier",
    "First Name", "Middle Name", "Last Name", "Sex", "Birthdate",
    "Optional State Data 1", "Grade Level When Assessed", "Hispanic or Latino Ethnicity",
    "American Indian or Alaska Native", "Asian", "Black or African American",
    "Native Hawaiian or Other Pacific Islander", "White", "Two or More Races",
    "Filler Race Field", "Federal Race/Ethnicity", "English Learner",
    "Title III Limited English Proficient Participation Status",
    "Gifted and Talented", "Migrant Status", "Economic Disadvantage Status",
    "Student With Disabilities", "Primary Disability Type", "Staff Member Identifier",
    "Assessment Accommodation: English learner (EL)", "Assessment Accommodation: 504",
    "Assessment Accommodation: Individualized Educational Plan (IEP)",
    "Frequent Breaks", "Separate/Alternate Location", "Small Testing Group",
    "Specialized Equipment or Furniture", "Specified Area or Setting",
    "Time Of Day", "Answer Masking", "Color Contrast", "Text-to-Speech for Mathematics",
    "Human Reader or Human Signer for Mathematics", "ASL Video",
    "Screen Reader OR other Assistive Technology (AT) Application",
    "Closed Captioning for ELA/L", "Human Reader or Human Signer for ELA/L",
    "Refreshable Braille Display for ELA/L", "Tactile Graphics",
    "Text-to-Speech for ELA/L", "Answers Recorded in Test Book",
    "Braille Response", "Calculation Device and Mathematics Tools",
    "ELA/L Constructed Response", "ELA/L Selected Response or Technology Enhanced Items",
    "Mathematics Response", "Monitor Test Response", "Word Prediction",
    "General Administration Directions Clarified in Student\x92s Native Language (by test administrator)",
    "General Administration Directions Read Aloud and Repeated as Needed in Student\x92s Native Language (by test administrator)",
    "Mathematics Response \x96 EL", "Translation of the Mathematics Assessment in Text-to-Speech",
    "Translation of the Mathematics Assessment Online", "Word to Word Dictionary (English/Native Language)",
    "Extended Time", "Alternate Representation \x96 Paper Test",
    "Translation of the Mathematics Assessment in Paper", "Human Reader or Human Signer",
    "Large Print", "Braille with Tactile Graphics", "Optional State Data 2",
    "Optional State Data 3", "Optional State Data 4", "Optional State Data 5",
    "Optional State Data 6", "Optional State Data 7", "Optional State Data 8",
    "Filler", "Test Code", "Assessment Grade", "Subject", "PBA Form ID",
    "EOY Form ID", "PBA Student Test UUID", "EOY Student Test UUID",
    "Summative Score Record UUID", "PBA Total Test Items", "PBA Test Attemptedness Flag",
    "EOY Total Test Items", "EOY Test Attemptedness Flag", "PBA Total Test Items Attempted",
    "PBA Unit 1 Total Number of Items", "PBA Unit 1 Number of Attempted Items",
    "PBA Unit 2 Total Number of Items", "PBA Unit 2 Number of Attempted Items",
    "PBA Unit 3 Total Number of Items", "PBA Unit 3 Number of Attempted Items",
    "PBA Unit 4 Total Number of Items", "PBA Unit  4 Number of Attempted Items",
    "PBA Unit 5 Total Number of Items", "PBA Unit  5 Number of Attempted Items",
    "EOY Total Test Items Attempted", "EOY Unit 1 Total Number of Items",
    "EOY Unit 1 Number of Attempted Items", "EOY Unit 2 Total Number of Items",
    "EOY Unit 2 Number of Attempted Items", "EOY Unit 3 Total Number of Items",
    "EOY Unit 3 Number of Attempted Items", "EOY Unit 4 Total Number of Items",
    "EOY Unit  4 Number of Attempted Items", "EOY Unit 5 Total Number of Items",
    "EOY Unit  5 Number of Attempted Items", "PBA Not Tested Reason",
    "EOY Not Tested Reason", "PBA Void PBA/EOY Score Reason", "EOY Void PBA/EOY Score Reason",
    "Filler", "Filler", "Summative Scale Score", "Summative CSEM",
    "Summative Performance Level", "Summative Reading Scale Score",
    "Summative Reading CSEM", "Summative Writing Scale Score", "Summative Writing CSEM",
    "Subclaim 1 Category", "Subclaim 2  Category", "Subclaim 3 Category",
    "Subclaim 4  Category", "Subclaim 5  Category", "Subclaim 6 Category",
    "Filler", "Filler", "Filler")

  results_files <- list('LA' = LA, 'PARCC' = PARCC)

  mask <- vapply(
    X = results_files, FUN = function(x) setequal(df, x), logical(1)
  )
  out <- names(results_files)[mask]

  return(out)
}

