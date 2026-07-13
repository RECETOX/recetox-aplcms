test_that("Utils: colnames pattern extraction", {
    dataframe <- dplyr::tibble(
        mz = numeric(),
        rt = numeric(),
        sample_1_intensity = numeric(),
        sample_2_intensity = numeric(),
        sample_1_rt = numeric(),
        sample_2_rt = numeric()
    )

    intensity_labels <- extract_pattern_colnames(dataframe, "_intensity")
    rt_labels <- extract_pattern_colnames(dataframe, "_rt")

    expect_equal(intensity_labels, c("sample_1_intensity", "sample_2_intensity"))
    expect_equal(rt_labels, c("sample_1_rt", "sample_2_rt"))
})

patrick::with_parameters_test_that(
  "is_mzml works",
  {
    expect_equal(is_mzml(filepath), expected)
  },
  patrick::cases(
    qc_no_dil_milliq = list(
      filepath = "8_qc_no_dil_milliq.mzml",
      expected = TRUE
    ),
    mbr0 = list(
      filepath = "mbr_test0.mzML",
      expected = TRUE
    ),
    rawfile = list(
      filepath = rawrr::sampleFilePath(),
      expected = FALSE
    )
  )
)