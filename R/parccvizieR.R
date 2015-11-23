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

  PARCC <- c("recordType", "multipleRecordFlag", "reportedSummativeScoreFlag",
    "reportedRosterFlag", "reportSuppressionCode", "reportSuppressionAction",
    "assessmentYear", "pba03Category", "eoy03Category", "stateAbbreviation",
    "responsibleDistrictIdentifier", "responsibleDistrictName", "responsibleSchoolInstitutionIdentifier",
    "responsibleSchoolInstitutionName", "pbaTestingDistrictIdentifier",
    "pbaTestingDistrictName", "pbaTestingSchoolInstitutionIdentifier",
    "pbaTestingSchoolInstitutionName", "eoyTestingDistrictIdentifier",
    "eoyTestingDistrictName", "eoyTestingSchoolInstitutionIdentifier",
    "eoyTestingSchoolInstitutionName", "parccStudentIdentifier",
    "stateStudentIdentifier", "localStudentIdentifier", "firstName",
    "middleName", "lastName", "sex", "birthdate", "optionalStateData1",
    "gradeLevelWhenAssessed", "hispanicOrLatinoEthnicity", "americanIndianOrAlaskaNative",
    "asian", "blackOrAfricanAmerican", "nativeHawaiianOrOtherPacificIslander",
    "white", "twoOrMoreRaces", "fillerRaceField", "federalRaceEthnicity",
    "englishLearner", "titleIIILimitedEnglishProficientParticipationStatus",
    "giftedAndTalented", "migrantStatus", "economicDisadvantageStatus",
    "studentWithDisabilities", "primaryDisabilityType", "staffMemberIdentifier",
    "assessmentAccommodationEnglishLearner", "assessmentAccommodation504",
    "assessmentAccommodationIndividualizedEducationalPlan", "frequentBreaks",
    "separateAlternateLocation", "smallTestingGroup", "specializedEquipmentOrFurniture",
    "specifiedAreaOrSetting", "timeOfDay", "answerMasking", "colorContrast",
    "textToSpeechForMathematics", "humanReaderOrHumanSignerForMathematics",
    "aslVideo", "screenReaderOrOtherAssistiveTechnologyApplication",
    "closedCaptioningForElal", "humanReaderOrHumanSignerForElal",
    "refreshableBrailleDisplayForElal", "tactileGraphics", "textToSpeechForElal",
    "answersRecordedInTestBook", "brailleResponse", "calculationDeviceAndMathematicsTools",
    "elalConstructedResponse", "elalSelectedResponseOrTechnologyEnhancedItems",
    "mathematicsResponse", "monitorTestResponse", "wordPrediction",
    "administrationDirectionsClarifiedInStudentsNativeLanguage",
    "administrationDirectionsReadAloudInStudentsNativeLanguage",
    "mathematicsResponseEl", "translationOfTheMathematicsAssessmentInTextToSpeech",
    "translationOfTheMathematicsAssessmentOnline", "wordToWordDictionary",
    "extendedTime", "alternateRepresentationPaperTest", "translationOfTheMathematicsAssessmentInPaper",
    "humanReaderOrHumanSigner", "largePrint", "brailleWithTactileGraphics",
    "optionalStateData2", "optionalStateData3", "optionalStateData4",
    "optionalStateData5", "optionalStateData6", "optionalStateData7",
    "optionalStateData8", "filler", "testCode", "assessmentGrade",
    "subject", "pbaFormId", "eoyFormId", "pbaStudentTestUuid", "eoyStudentTestUuid",
    "summativeScoreRecordUuid", "pbaTotalTestItems", "pbaTestAttemptednessFlag",
    "eoyTotalTestItems", "eoyTestAttemptednessFlag", "pbaTotalTestItemsAttempted",
    "pbaUnit1TotalNumberOfItems", "pbaUnit1NumberOfAttemptedItems",
    "pbaUnit2TotalNumberOfItems", "pbaUnit2NumberOfAttemptedItems",
    "pbaUnit3TotalNumberOfItems", "pbaUnit3NumberOfAttemptedItems",
    "pbaUnit4TotalNumberOfItems", "pbaUnit4NumberOfAttemptedItems",
    "pbaUnit5TotalNumberOfItems", "pbaUnit5NumberOfAttemptedItems",
    "eoyTotalTestItemsAttempted", "eoyUnit1TotalNumberOfItems", "eoyUnit1NumberOfAttemptedItems",
    "eoyUnit2TotalNumberOfItems", "eoyUnit2NumberOfAttemptedItems",
    "eoyUnit3TotalNumberOfItems", "eoyUnit3NumberOfAttemptedItems",
    "eoyUnit4TotalNumberOfItems", "eoyUnit4NumberOfAttemptedItems",
    "eoyUnit5TotalNumberOfItems", "eoyUnit5NumberOfAttemptedItems",
    "pbaNotTestedReason", "eoyNotTestedReason", "pbaVoidPbaEoyScoreReason",
    "eoyVoidPbaEoyScoreReason", "filler.1", "filler.2", "summativeScaleScore",
    "summativeCsem", "summativePerformanceLevel", "summativeReadingScaleScore",
    "summativeReadingCsem", "summativeWritingScaleScore", "summativeWritingCsem",
    "subclaim1Category", "subclaim2Category", "subclaim3Category",
    "subclaim4Category", "subclaim5Category", "subclaim6Category",
    "filler.3", "filler.4", "filler.5")

  results_files <- list('LA' = LA, 'PARCC' = PARCC)

  mask <- vapply(
    X = results_files, FUN = function(x) setequal(df, x), logical(1)
  )
  out <- names(results_files)[mask]

  return(out)
}

