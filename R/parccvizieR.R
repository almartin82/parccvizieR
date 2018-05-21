#' @title Create a parccvizieR object
#'
#' @description
#' \code{parccvizieR} is the primary workflow function that generates the
#' parccvizieR object
#'
#' @param results_location a file, or directory of files, containing PARCC
#' results.  parccvizieR supports NJ summative record files, but is
#' architected to gracefully extend support to other PARCC consortium states
#' @param local_roster optional.  additional roster data to supplement
#' the data in the PARCC file.
#' @param verbose print status updates as the data is processed?
#' default is TRUE
#' @examples
#'\dontrun{
#' pviz <- parccvizieR(sf = ex_sf)
#'
#' is.parccvizieR(pviz)
#' }
#' @export

parccvizieR <- function(results, local_roster = NA, verbose = TRUE) {
  UseMethod("parccvizieR")
}

#' @export
parccvizieR.default <- function(results, local_roster = NA, verbose = FALSE, ...) {

  #read in results.  get a list of one OR MORE paths to files.
  raw <- read_raw_results(results)

  #for EACH FILE, determine the layout
  file_origins <- map_chr(
    raw,
    detect_result_file_origin
  )

  #use map2df on the list of files and list of layouts to process the data

  #return parccvizieR object
  out <- NA
  class(out) <- "parccvizieR"
}


#' Title
#'
#' @param x the results argument from parccvizieR.  either a path to one file
#' OR a folder of files
#'
#' @return character vector of full paths to results files
#' @export

read_raw_results <- function(x) {

  #if we can find 'dot csv' in the results argument,
  #assume that we have a path to a SINGLE FILE and return that.
  if (grepl('.csv', x, ignore.case = TRUE)) {
    out <- file.path(dirname(x), basename(x))
  #otherwise assume we have a directory.  in that case, return a vector of
  #paths.
  } else {
    out <- dir(path = x, pattern = "csv", ignore.case = TRUE,
        recursive = TRUE, full.names = TRUE)
  }

  out
}


#' Detect Type/Origin of Raw PARCC Results File
#'
#' @description PARCC is a consortium, and (we believe) each state has some
#' flexibility in how it reports the core PARCC results.  For instance, MA has
#' a hybrid assessment that includes PARCC content and state content.  This
#' function detects the type of file by looking at the names/columns, and
#' processes accordingly.
#'
#' @param df_path
#'
#' @return character string representing type detected
#' @export

detect_result_file_origin <- function(df_path) {

  df_names <- suppressMessages(suppressWarnings(readr::read_csv(df_path))) %>%
    names()

  NJ_SRF15 <- c("recordType", "multipleRecordFlag", "reportedSummativeScoreFlag",
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
    "eoyVoidPbaEoyScoreReason", "filler_1", "filler_2", "summativeScaleScore",
    "summativeCsem", "summativePerformanceLevel", "summativeReadingScaleScore",
    "summativeReadingCsem", "summativeWritingScaleScore", "summativeWritingCsem",
    "subclaim1Category", "subclaim2Category", "subclaim3Category",
    "subclaim4Category", "subclaim5Category", "subclaim6Category",
    "filler_3", "filler_4", "filler_5")

  NJ_SRF16 <- c("StateAbbreviation", "TestingDistrictCode", "TestingSchoolCode",
    "ResponsibleDistrictCode", "ResponsibleSchoolCode", "StateStudentIdentifier",
    "LocalStudentIdentifier", "PARCCStudentIdentifier", "LastOrSurname",
    "FirstName", "MiddleName", "Birthdate", "Sex", "StateField1",
    "GradeLevelWhenAssessed", "HispanicOrLatinoEthnicity", "AmericanIndianOrAlaskaNative",
    "Asian", "BlackOrAfricanAmerican", "NativeHawaiianOrOtherPacificIslander",
    "White", "FillerField", "TwoOrMoreRaces", "EnglishLearnerEL",
    "TitleIIILimitedEnglishProficientParticipationStatus", "GiftedandTalented",
    "MigrantStatus", "EconomicDisadvantageStatus", "StudentWithDisabilities",
    "PrimaryDisabilityType", "StateField2", "StateField3", "StateField4",
    "StateField5", "StateField6", "StateField7", "StateField8", "StateField9",
    "StateField10", "StateField11", "StateField12", "Filler", "ClassName",
    "TestAdministrator", "StaffMemberIdentifier", "TestCode", "Filler_1",
    "Retest", "ELAccommodation", "FrequentBreaks", "SeparateAlternateLocation",
    "SmallTestingGroup", "SpecializedEquipmentOrFurniture", "SpecifiedAreaOrSetting",
    "TimeOfDay", "AnswerMasking", "FillerField_1", "ColorContrast",
    "ASLVideo", "AssistiveTechnologyScreenReader", "AssistiveTechnologyNonScreenReader",
    "ClosedCaptioningForELAL", "RefreshableBrailleDisplayForELAL",
    "AlternateRepresentationPaperTest", "LargePrint", "BrailleWithTactileGraphics",
    "StudentReadsAssessmentAloudToThemselves", "HumanSignerForTestDirections",
    "AnswersRecordedInTestBook", "BrailleResponse", "CalculationDeviceAndMathematicsTools",
    "ELALConstructedResponse", "ELALSelectedResponseOrTechnologyEnhancedItems",
    "MathematicsResponse", "MonitorTestResponse", "WordPrediction",
    "AdministrationDirectionsClarifiedinStudentsNativeLanguage",
    "AdministrationDirectionsReadAloudinStudentsNativeLanguage",
    "MathematicsResponseEL", "TranslationOfTheMathematicsAssessment",
    "WordtoWordDictionaryEnglishNativeLanguage", "TextToSpeech",
    "HumanReaderOrHumanSigner", "UniqueAccommodation", "EmergencyAccommodation",
    "ExtendedTime", "StudentTestUUID", "PaperFormID", "OnlineFormID",
    "TestStatus", "TotalTestItems", "TestAttemptednessFlag", "TotalTestItemsAttempted",
    "PaperUnit1TotalTestItems", "PaperSection1NumberofAttemptedItems",
    "PaperSection2TotalTestItems", "PaperSection2NumberofAttemptedItems",
    "PaperSection3TotalTestItems", "PaperSection3NumberofAttemptedItems",
    "PaperSection4TotalTestItems", "PaperSection4NumberofAttemptedItems",
    "StudentUnit1TestUUID", "Unit1FormID", "Unit1TotalTestItems",
    "Unit1NumberofAttemptedItems", "StudentUnit2TestUUID", "Unit2FormID",
    "Unit2TotalTestItems", "Unit2NumberOfAttemptedItems", "StudentUnit3TestUUID",
    "Unit3FormID", "Unit3TotalTestItems", "Unit3NumberOfAttemptedItems",
    "StudentUnit4TestUUID", "Unit4FormID", "Unit4TotalTestItems",
    "Unit4NumberofAttemptedItems", "NotTestedCode", "NotTestedReason",
    "VoidScoreCode", "VoidScoreReason", "ShipReportDistrictCode",
    "ShipReportSchoolCode", "Summative Flag", "MultipleTestRegistration",
    "RosterFlag", "ReportSuppressionCode", "ReportSuppressionAction",
    "AttemptCreateDate", "Unit1OnlineTestStartDateTime", "Unit1OnlineTestEndDateTime",
    "Unit2OnlineTestStartDateTime", "Unit2OnlineTestEndDateTime",
    "Unit3OnlineTestStartDateTime", "Unit3OnlineTestEndDateTime",
    "Unit4OnlineTestStartDateTime", "Unit4OnlineTestEndDateTime",
    "AssessmentYear", "AssessmentGrade", "Subject", "FederalRaceEthnicity",
    "Period", "TestingOrganizationalType", "TestingDistrictName",
    "TestingSchoolName", "ResponsibleOrganizationCodeType", "ResponsibleDistrictName",
    "ResponsibleSchoolName", "Filler_2", "Filler_3", "Filler_4",
    "Filler_5", "Filler_6", "Filler_7", "TestScaleScore", "TestCSEMProbableRange",
    "TestPerformanceLevel", "TestReadingScaleScore", "TestReadingCSEM",
    "TestWritingScaleScore", "TestWritingCSEM", "Subclaim1Category",
    "Subclaim2Category", "Subclaim3Category", "Subclaim4Category",
    "Subclaim5Category", "Subclaim6Category", "Filler_8", "Filler_9",
    "Filler_10", "Filler_11", "TestScoreComplete"
  )

  NJ_SRF17 <- c("StateAbbreviation", "TestingDistrictCode", "TestingSchoolCode",
     "ResponsibleDistrictCode", "ResponsibleSchoolCode", "StateStudentIdentifier",
     "LocalStudentIdentifier", "PARCCStudentIdentifier", "LastOrSurname",
     "FirstName", "MiddleName", "Birthdate", "Sex", "StateField1",
     "GradeLevelWhenAssessed", "HispanicOrLatinoEthnicity", "AmericanIndianOrAlaskaNative",
     "Asian", "BlackOrAfricanAmerican", "NativeHawaiianOrOtherPacificIslander",
     "White", "FillerField1", "TwoOrMoreRaces", "EnglishLearnerEL",
     "TitleIIILimitedEnglishProficientParticipationStatus", "GiftedandTalented",
     "MigrantStatus", "EconomicDisadvantageStatus", "StudentWithDisabilities",
     "PrimaryDisabilityType", "StateField2", "StateField3", "StateField4",
     "StateField5", "StateField6", "StateField7", "StateField8", "StateField9",
     "StateField10", "StateField11", "StateField12", "FillerField2",
     "ClassName", "TestAdministrator", "StaffMemberIdentifier", "TestCode",
     "FillerField", "Retest", "FillerField_1", "FrequentBreaks", "SeparateAlternateLocation",
     "SmallTestingGroup", "SpecializedEquipmentOrFurniture", "SpecifiedAreaOrSetting",
     "TimeOfDay", "AnswerMasking", "StudentReadsAssessmentAloudToThemselves",
     "ColorContrast", "ASLVideo", "AssistiveTechnologyScreenReader",
     "AssistiveTechnologyNonScreenReader", "ClosedCaptioningForELAL",
     "RefreshableBrailleDisplayForELAL", "AlternateRepresentationPaperTest",
     "LargePrint", "BrailleWithTactileGraphics", "FillerField4", "HumanSignerForTestDirections",
     "AnswersRecordedInTestBook", "BrailleResponse", "CalculationDeviceAndMathematicsTools",
     "ELALConstructedResponse", "ELALSelectedResponseOrTechnologyEnhancedItems",
     "MathematicsResponse", "MonitorTestResponse", "WordPredictionForELAL",
     "AdministrationDirectionsClarifiedinStudentsNativeLanguage",
     "AdministrationDirectionsReadAloudinStudentsNativeLanguage",
     "MathematicsResponseEL", "SpanishTransadaptationOfTheMathematicsAssessment",
     "WordtoWordDictionaryEnglishNativeLanguage", "TextToSpeech",
     "HumanReaderOrHumanSigner", "UniqueAccommodation", "EmergencyAccommodation",
     "ExtendedTime", "StudentTestUUID", "PaperFormID", "OnlineFormID",
     "TestStatus", "TotalTestItems", "TestAttemptednessFlag", "TotalTestItemsAttempted",
     "PaperSection1TotalTestItems", "PaperSection1NumberofAttemptedItems",
     "PaperSection2TotalTestItems", "PaperSection2NumberofAttemptedItems",
     "PaperSection3TotalTestItems", "PaperSection3NumberofAttemptedItems",
     "PaperSection4TotalTestItems", "PaperSection4NumberofAttemptedItems",
     "StudentUnit1TestUUID", "Unit1FormID", "Unit1TotalTestItems",
     "Unit1NumberofAttemptedItems", "StudentUnit2TestUUID", "Unit2FormID",
     "Unit2TotalTestItems", "Unit2NumberOfAttemptedItems", "StudentUnit3TestUUID",
     "Unit3FormID", "Unit3TotalTestItems", "Unit3NumberOfAttemptedItems",
     "StudentUnit4TestUUID", "Unit4FormID", "Unit4TotalTestItems",
     "Unit4NumberofAttemptedItems", "NotTestedCode", "NotTestedReason",
     "VoidScoreCode", "VoidScoreReason", "ShipReportDistrictCode",
     "ShipReportSchoolCode", "SummativeFlag", "MultipleTestRegistration",
     "RosterFlag", "ReportSuppressionCode", "ReportSuppressionAction",
     "PaperAttemptCreateDate", "Unit1OnlineTestStartDateTime", "Unit1OnlineTestEndDateTime",
     "Unit2OnlineTestStartDateTime", "Unit2OnlineTestEndDateTime",
     "Unit3OnlineTestStartDateTime", "Unit3OnlineTestEndDateTime",
     "Unit4OnlineTestStartDateTime", "Unit4OnlineTestEndDateTime",
     "AssessmentYear", "AssessmentGrade", "Subject", "FederalRaceEthnicity",
     "Period", "TestingOrganizationalType", "TestingDistrictName",
     "TestingSchoolName", "ResponsibleOrganizationalType", "ResponsibleDistrictName",
     "ResponsibleSchoolName", "FillerField_2", "FillerField_3", "FillerField_4",
     "FillerField_5", "FillerField_6", "FillerField_7", "TestScaleScore",
     "TestCSEMProbableRange", "TestPerformanceLevel", "TestReadingScaleScore",
     "TestReadingCSEM", "TestWritingScaleScore", "TestWritingCSEM",
     "Subclaim1Category", "Subclaim2Category", "Subclaim3Category",
     "Subclaim4Category", "Subclaim5Category", "FillerField5", "FillerField_8",
     "FillerField_9", "FillerField_10", "FillerField_11", "TestScoreComplete",
     "FillerField_12", "FillerField_13", "FillerField_14", "FillerField_15",
     "FillerField_16", "FillerField_17", "FillerField_18", "FillerField_19",
     "FillerField_20", "FillerField_21", "FillerField_22", "FillerField_23",
     "FillerField_24", "FillerField_25", "FillerField_26", "FillerField_27",
     "FillerField7", "FillerField_28", "FillerField_29", "FillerField_30"
  )

  results_files <- list(
    'NJ_SRF15' = NJ_SRF15, 'NJ_SRF16' = NJ_SRF16, 'NJ_SRF17' = NJ_SRF17
  )

  mask <- vapply(
    X = results_files, FUN = function(x) setequal(df_names, x), logical(1)
  )
  out <- names(results_files)[mask]

  if (length(out) == 0) {
    stop(paste0(
      sprintf("Unable to determine file format for %s.", df_path), '\n',
      "parccvizieR currently supports NJ SRF files for 2014-15", '\n' ,
      "through 2017-18.  Please file an issue at:", '\n',
      "https://github.com/almartin82/parccvizieR/issues", '\n',
      "with any details of your result file (eg, State?)."
    ))
  }

  return(out)
}


read_results_file <- function(path, format) {

  if (format == 'NJ_SRF15') {

  } else if (format == 'NJ_SRF16') {

  } else if (format == 'NJ_SRF17') {

  } else if (format == 'NJ_SRF18') {
    #when we see 2017-18 SRF, add something here
  }
}
