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

  #read in results.  get a list of one OR MORE paths to files.

  #for EACH FILE, determine the layout

  #use map2df on the list of files and list of layouts to process the data


  raw <- read_raw_results(results)

  #given the names of the raw file, detect the type
  result_type <- detect_result_file_origin(names(raw))

  if (identical(result_type, character(0))) {
    stop("Unknown PARCC results type.")

  # NJ_SRF is supported.
  } else if (identical(result_type, 'NJ_SRF')) {
    message('Detected NJ PARCC Summative Record File')

    #process NJ_SRF

  # stub. left as an exercise for future development / contributors
  } else if (identical(result_type, 'PARCC')) {
    message('Detected Generic PARCC Summative File')
    message('Please write a processing function for me :)')

    #TODO: process Generic PARCC Summative File data file

  # stub. left as an exercise for future development / contributors
  } else if (identical(result_type, 'LA')) {
    message('Detected Louisiana Roster file')
    message('Please write a processing function for me :)')

    #TODO: process louisiana data file
  }

  #return parccvizieR object
  out <- NA
  class(out) <- "parccvizieR"

  out
}


read_raw_results <- function(x) {

  #if csv is in the results argument, assume that we have a FILE
  if (grepl('.csv', x, ignore.case = TRUE)) {
    raw_path <- x
  #otherwise assume that we have a PATH, return the FIRST csv in that folder
  #this is just to detect the format of the results file we are looking at
  } else {
    raw_path <- dir(path = x, pattern = "csv", ignore.case = TRUE,
        recursive = TRUE, full.names = TRUE)[1]
  }

  #read the raw path
  suppressMessages(suppressWarnings(readr::read_csv(file = raw_path)))
}


#' Detect Type/Origin of Raw PARCC Results File
#'
#' @description PARCC is a consortium, and (we believe) each state has some
#' flexibility in how it reports the core PARCC results.  For instance, MA has
#' a hybrid assessment that includes PARCC content and state content.  This
#' function detects the type of file by looking at the names/columns, and
#' processes accordingly.
#'
#' @param df
#'
#' @return character string representing type detected
#' @export

detect_result_file_origin <- function(df) {

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

  #from 2015, using a sample / released data file.  out of date?
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

  results_files <- list('LA' = LA, 'PARCC' = PARCC, 'NJ_SRF' = NJ_SRF)

  mask <- vapply(
    X = results_files, FUN = function(x) setequal(df, x), logical(1)
  )
  out <- names(results_files)[mask]

  return(out)
}


read_raw_nj_srf <- function(x) {
}
