library(OlinkAnalyze)
npx_df1_path <- "./results/sampleid_CDM205YTP0018(微芯-51个样本)_NPX_long.xlsx"
npx_df2_path <- "./results/sampleid_微芯单独样本_NPX.xlsx"
npx_df1 <- read.xlsx(npx_df1_path) %>%
    dplyr::mutate(Project = "P1")
npx_df2 <- read.xlsx(npx_df2_path) %>%
    dplyr::mutate(Project = "P2")
overlap_samples <- intersect((npx_df1 %>%
    dplyr::filter(!grepl("control", SampleID, ignore.case = TRUE)))$SampleID, (npx_df2 %>%
    dplyr::filter(!grepl("control", SampleID, ignore.case = TRUE)))$SampleID)
NPX_normalization <- olink_normalization(df1 = npx_df1, 
                                         df2 = npx_df2, 
                                         overlapping_samples_df1 = overlap_samples, 
                                         df1_project_nr = "P1", 
                                         df2_project_nr = "P2",
                                         reference_project = "P1") %>%
    dplyr::filter(., !.$SampleID == "Samplecontrol")

NPX_normalization_exp <- tidyr::pivot_wider(NPX_normalization[, c("SampleID", "Assay", "NPX", "Project")], names_from = "Assay", values_from = "NPX") %>%
    distinct(SampleID, .keep_all = T)

NPX_normalization_LOD <- NPX_normalization[, c("Assay", "LOD", "Project")] %>%
    distinct() %>%
    tidyr::pivot_wider(., names_from = "Assay", values_from = "LOD") %>%
    dplyr::mutate(SampleID = "LOD") %>%
    dplyr::select(., SampleID, everything())

NPX_normalization_matrix <- rbind(NPX_normalization_exp, NPX_normalization_LOD)
attach(NPX_normalization_matrix)
NPX_normalization_matrix_p1 <- dplyr::filter(NPX_normalization_matrix, Project == "P1")
NPX_normalization_matrix_p2 <- dplyr::filter(NPX_normalization_matrix, Project == "P2")
detach(NPX_normalization_matrix)
# write.xlsx(NPX_normalization_matrix, paste0(dirname(npx_df1_path), "/NPX_normalization_matrix_", gsub(basename(npx_df1_path), pattern = "sampleid_", replacement = "")))
# write.xlsx(NPX_normalization_matrix_p1, paste0(dirname(npx_df1_path), "/NPX_normalization_matrix_P1_", gsub(basename(npx_df1_path), pattern = "sampleid_", replacement = "")))
# write.xlsx(NPX_normalization_matrix_p2, paste0(dirname(npx_df1_path), "/NPX_normalization_matrix_P2_", gsub(basename(npx_df1_path), pattern = "sampleid_", replacement = "")))
write.xlsx(list(matrix=NPX_normalization_matrix,
                matrix_p1=NPX_normalization_matrix_p1,
                matrix_p2=NPX_normalization_matrix_p2),
           paste0(dirname(npx_df1_path), "/NPX_normalization_matrix_", gsub(basename(npx_df1_path), pattern = "sampleid_", replacement = ""))
           )
