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
parccvizieR.default <- function(results, local_roster = NA, verbose = TRUE, ...) {

  #read in results.  get a list of one OR MORE paths to files.
  if(verbose) print('Reading in raw result file(s)...')
  raw_files <- read_raw_results(results)
  if(verbose) print(sprintf('Found %s raw result file(s).', length(raw_files)))

  #for EACH FILE, determine the layout
  file_layouts <- map_chr(
    raw_files,
    detect_result_file_layout
  )
  if(verbose) print(
    sprintf('File(s) are in the following format(s): %s.',
            unique(file_layouts) %>% paste(., collapse = ', '))
  )

  #list to house all of the parccvizieR data objects
  out <- list()

  #use map2df on the list of files and list of layouts to
  #process the data
  out$raw <- map2_df(
    .x = raw_files,
    .y = file_layouts,
    .f = ~read_results_file(.x, .y, verbose)
  )

  #determine the best record to keep and return that as the srf object
  if(verbose) print('De-duping multiple test events per student per test per year...')
  out$srf <- dedupe_srf(out$raw) %>%
    filter(rn == 1) %>%
    select(-rn)
  if(verbose) print(
    sprintf('Identified and discarded %s duplicate test events', nrow(out$raw) - nrow(out$srf))
  )

  #set default grouping on the srf to support summary stats
  out$srf <- out$srf %>%
    dplyr::group_by(academic_year,
             subject_area,
             responsible_school_code,
             responsible_school_name,
             assessment_grade_numeric,
             test_code)

  #make the growth object
  if(verbose) print('Generating a growth dataframe...')
  out$growth_df <- generate_growth_data(out$srf)
  if(verbose) print(
    sprintf('Created a growth dataframe with %s rows across %s growth terms',
            nrow(out$growth_df), length(unique(out$growth_df$growth_window)))
  )

  #return parccvizieR object
  class(out) <- c("parccvizieR", class(out))
  class(out$srf) <- c("parccvizieR_srf", class(out$srf))
  class(out$growth_df) <- c("parccvizieR_growth_df", class(out$growth_df))

  #make all the dfs a parccvizieR object
  out$raw <- parccvizieR_data(out$raw)
  out$srf <- parccvizieR_data(out$srf)
  out$growth_df <- parccvizieR_data(out$growth_df)

  if(verbose) say("parccvizieR object created!", "cow")

  out
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


#' Detect Layout/Origin of Raw PARCC Results File
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

detect_result_file_layout <- function(df_path) {

  df_names <- suppressMessages(suppressWarnings(
    readr::read_csv(df_path, progress = FALSE)
  )) %>%
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


#' Read a results file given a path and file format
#'
#' @param path full path to file, output of read_raw_results
#' @param format file format detected, output of detect_result_file_layout
#' @param verbose print status updates as the data is processed?
#' default is FALSE
#'
#' @return data.frame/tbl
#' @export

read_results_file <- function(path, format, verbose = FALSE) {

  file_update <- function(df) {
    sprintf('Processed %s rows representing %s students and %s unique tests.',
            nrow(df), length(unique(df$state_student_identifier)),
            length(unique(df$test_code)))
  }

  if (format == 'NJ_SRF15') {
    if(verbose) print('Processing 2015 NJ SRF...')
    out <- process_nj_srf15(path)
    if(verbose) print(file_update(out))
  } else if (format == 'NJ_SRF16') {
    if(verbose) print('Processing 2016 NJ SRF...')
    out <- process_nj_srf16(path)
    if(verbose) print(file_update(out))
  } else if (format == 'NJ_SRF17') {
    if(verbose) print('Processing 2017 NJ SRF...')
    out <- process_nj_srf17(path)
    if(verbose) print(file_update(out))
  } else if (format == 'NJ_SRF18') {
    if(verbose) print('Processing 2018 NJ SRF...')
    out <- process_nj_srf18(path)
    if(verbose) print(file_update(out))
  }

  out
}


#' Common/universal reading and cleaning of a data file
#'
#' @param path full path to file, output of read_raw_results
#'
#' @return data.frame/tbl

basic_read_and_clean <- function(path) {
  df <- suppressMessages(suppressWarnings(
    readr::read_csv(path, progress = FALSE)
  )) %>%
  janitor::clean_names()

  #remove all filler columns per issue #4
  is_filler <- grepl('^filler_', names(df))
  df <- df[, !is_filler]

  #optionalStateData to StateField
  names(df) <- gsub('optional_state_data', 'state_field', names(df))
  names(df) <- gsub('clarifiedin', 'clarified_in', names(df))
  names(df) <- gsub('aloudin', 'aloud_in', names(df))
  names(df) <- gsub('english_learner_el', 'english_learner', names(df))
  names(df) <- gsub('giftedand_talented', 'gifted_and_talented', names(df))

  names(df) <- gsub('last_name', 'last_or_surname', names(df))

  #straight up a typo in the SRF
  names(df) <- gsub('paper_unit1', 'paper_section1', names(df))

  #sort of unclear?
  names(df) <- gsub('responsible_organization_code_type', 'responsible_organizational_type', names(df))

  names(df) <- gsub('district_identifier', 'district_code', names(df))
  names(df) <- gsub('school_institution', 'school', names(df))
  names(df) <- gsub('school_identifier', 'school_code', names(df))


  #common cleaning for fields in ALL srfs
  df <- df %>%
    #make several columns character
    mutate_at(
      .vars = vars(state_student_identifier,
                   local_student_identifier,
                   state_field8),
      as.character
    ) %>%
    #make columns numeric
    mutate_at(
      .vars = vars(grade_level_when_assessed),
      as.numeric
    ) %>%
    #convert assessment year to end year
    mutate(
      academic_year = str_extract(assessment_year, '[[:digit:]]{4}$'),
      academic_year = as.numeric(academic_year)
    ) %>%
    #create assessment grade numeric for analyzing by test took
    mutate(
      assessment_grade_numeric = gsub(
        'Grade ', '', assessment_grade
      ) %>% as.numeric()
    ) %>%
    #if grade is 99 (missing?  graduated?) use the assessment grade instead
    mutate(
      grade_level_when_assessed = case_when(
        grade_level_when_assessed < 99 ~ grade_level_when_assessed,
        grade_level_when_assessed == 99 ~ assessment_grade_numeric
      )
    ) %>%
    #add in cohort year per #12
    mutate(
      cohort = cohort_year(academic_year, grade_level_when_assessed)
    ) %>%
    #determine the subject area
    mutate(
      subject_area = subject_area(subject)
    )

  df
}


#' Clean columns present in 2016 SRFs and on, but not in the weird 2015 file
#'
#' @param df data frame, product of basic_read
#'
#' @return data.frame/tbl
#' @export

clean_nj_2016_plus <- function(df) {

  #character
  df <- df %>%
    mutate_at(
      .vars = vars(paper_section1numberof_attempted_items),
      as.character
    )

  #numeric
  df <- df %>%
    mutate_at(
      .vars = vars(paper_section1numberof_attempted_items,
                   paper_section1total_test_items,
                   paper_section2numberof_attempted_items,
                   paper_section2total_test_items,
                   paper_section3numberof_attempted_items,
                   paper_section3total_test_items,
                   paper_section4numberof_attempted_items,
                   paper_section4total_test_items,
                   unit1total_test_items,
                   unit2total_test_items,
                   unit3total_test_items,
                   unit4total_test_items,
                   unit4numberof_attempted_items),
      as.numeric
    )

  #date
  df <- df %>%
    mutate_at(
      .vars = vars(unit1online_test_start_date_time,
                   unit1online_test_end_date_time,
                   unit2online_test_start_date_time,
                   unit2online_test_end_date_time,
                   unit3online_test_start_date_time,
                   unit3online_test_end_date_time,
                   unit4online_test_start_date_time,
                   unit4online_test_end_date_time),
      ymd_hms
    )

  df
}


#' Determine Test Subject Area
#'
#' @param x a vector of subjects
#'
#' @return vector of subject areas
#' @export

subject_area <- function(x) {
  case_when(
    x == 'English Language Arts/Literacy' ~ "ELA",
    x == 'Algebra I' ~ "Math",
    x == 'Algebra II' ~ "Math",
    x == 'Geometry' ~ "Math",
    x == 'Mathematics' ~ "Math",
    x == 'Integrated Mathematics I ' ~ "Math"
  )
}


#' Process a NJ SRF file (any year)
#'
#' @inheritParams basic_read_and_clean
#'
#' @return data.frame/tbl
#' @export

process_nj_srf15 <- function(path) {
  basic_read_and_clean(path)
  mutate(test_scale_score == summative_scale_score)
}

#' @rdname process_nj_srf15
#' @export

process_nj_srf16 <- function(path) {
  df <- basic_read_and_clean(path)
  clean_nj_2016_plus(df)
}

#' @rdname process_nj_srf15
#' @export

process_nj_srf17 <- function(path) {
  df <- basic_read_and_clean(path)
  clean_nj_2016_plus(df)
}

#' @rdname process_nj_srf15
#' @export

process_nj_srf18 <- function(path) {
  df <- basic_read_and_clean(path)
  clean_nj_2016_plus(df)
}


#' Title print method for\code{parccvizier} class
#' @description prints to console
#' @param x a \code{parccvizier} object
#' @param ... additional arguments
#' @rdname print
#' @return some details about the object to the console
#' @export

print.parccvizieR <-  function(x, ...) {

  #gather some summary stats
  n_df <- length(x)
  n_sy <- length(unique(x$srf$academic_year))
  min_sy <- min(x$srf$academic_year)
  max_sy <- max(x$srf$academic_year)
  n_students <- length(unique(x$srf$state_student_identifier))
  n_schools <- length(unique(x$srf$responsible_school_name))

  growthseasons <- table(x$growth_df$growth_window) %>% names()
  n_growthseasons <- length(growthseasons)

  cat("A parccvizieR object repesenting:\n- ")
  cat(paste(n_sy))
  cat(" school years from SY")
  cat(paste(min_sy))
  cat(" to SY")
  cat(paste(max_sy))
  cat(";\n- ")
  cat(paste(n_students))
  cat(" unique students from ")
  cat(paste(n_schools))
  cat(" unique schools;\n- and, ")
  cat(paste(n_growthseasons))
  cat(" growth seasons:\n    ")
  cat(paste(growthseasons, collapse = ",\n    "))
}

