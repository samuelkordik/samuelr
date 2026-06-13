test_that("lump_impressions matches by code and description", {
  expect_equal(as.character(lump_impressions(code = "I46.9")), "Cardiac Arrest")
  expect_equal(
    as.character(lump_impressions(desc = "Cardiac Arrest")),
    "Cardiac Arrest"
  )
  expect_equal(
    as.character(lump_impressions(code = "I21.3", tier = "tier2")),
    "STEMI"
  )
})

test_that("description matching is robust to separators, case, and mojibake", {
  variants <- c(
    "Behavioral HealthSuicidal Ideation", # real C1-control-char form
    "Behavioral HealthSuicidal Ideation", # glued CamelCase
    "behavioral health- suicidal ideation", # dashed, lowercase
    "Behavioral Health: Suicidal Ideation" # colon-separated
  )
  out <- lump_impressions(desc = variants, tier = "tier2")
  expect_equal(as.character(out), rep("Suicidal Ideation", 4))
})

test_that("deactivated 'x' prefix is stripped", {
  expect_equal(
    as.character(lump_impressions(desc = "xHeat Related- Exhaustion")),
    "Environmental"
  )
})

test_that("code key and description key never disagree on tier1", {
  g <- samuelr::impression_groups
  paired <- g[!is.na(g$code) & !is.na(g$desc), ]
  by_code <- lump_impressions(code = paired$code)
  by_desc <- lump_impressions(desc = paired$desc)
  expect_equal(as.character(by_code), as.character(by_desc))
})

test_that("code takes precedence, description fills missing codes", {
  out <- lump_impressions(
    desc = c("Cardiac Arrest", "Seizures"),
    code = c("I46.9", NA)
  )
  expect_equal(as.character(out), c("Cardiac Arrest", "Seizure"))
})

test_that("NA in, NA out; unknown values warn and become Other/Unmatched", {
  expect_true(is.na(lump_impressions(desc = NA_character_)))
  expect_warning(
    out <- lump_impressions(desc = "Totally Made Up"),
    "not in taxonomy"
  )
  expect_equal(as.character(out), "Other/Unmatched")
})

test_that("lump_injury_causes groups penetrating trauma", {
  expect_equal(
    as.character(lump_injury_causes(desc = "Stabbing")),
    "Penetrating Trauma - Sharp"
  )
  expect_equal(
    as.character(lump_injury_causes(code = "X95")),
    "Penetrating Trauma - Firearm"
  )
})

test_that("requires at least one key and equal lengths", {
  expect_error(lump_impressions(), "Supply")
  expect_error(
    lump_impressions(desc = c("a", "b"), code = "x"),
    "same length"
  )
})

test_that("separate_desc_code splits combined column and expands lists", {
  df <- data.frame(
    id = 1:3,
    cause = c(
      "Fall from Standing position (W01)",
      "Assault by blunt object (Y00), Contact with blunt object (Y29)",
      "Not Applicable ()"
    )
  )
  out <- separate_desc_code(df, "cause")
  expect_equal(nrow(out), 4L)
  expect_equal(out$cause_desc[2:3], c("Assault by blunt object", "Contact with blunt object"))
  expect_equal(out$cause_code[2:3], c("Y00", "Y29"))
  expect_true(is.na(out$cause_code[4]))
  expect_false("cause" %in% names(out))
})

test_that("separate_desc_code preserves inner parentheses in descriptions", {
  df <- data.frame(x = "Bite (animal)(W55.81)")
  out <- separate_desc_code(df, "x")
  expect_equal(out$x_desc, "Bite (animal)")
  expect_equal(out$x_code, "W55.81")
})
