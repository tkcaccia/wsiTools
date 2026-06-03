library(Seurat)
library(SeuratData)
library(wsiTools)

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(e) "")
benchmark_dir <- if (nzchar(script_path)) dirname(script_path) else getwd()
save_dir <- file.path(benchmark_dir, "save")
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

slice_images <- c(
  anterior1 = "V1_Mouse_Brain_Sagittal_Anterior_image.tif",
  anterior2 = "V1_Mouse_Brain_Sagittal_Anterior_Section_2_image.tif",
  posterior1 = "V1_Mouse_Brain_Sagittal_Posterior_image.tif",
  posterior2 = "V1_Mouse_Brain_Sagittal_Posterior_Section_2_image.tif"
)
slice_names <- names(slice_images)
slice_images <- file.path(benchmark_dir, slice_images)
names(slice_images) <- slice_names
missing_images <- slice_images[!file.exists(slice_images)]
if (length(missing_images)) {
  stop("Missing high-resolution image(s):\n", paste(missing_images, collapse = "\n"))
}

seurat_rds <- file.path(save_dir, "stxBrain_4slides_merged_seurat.rds")
if (file.exists(seurat_rds)) {
  brain.merge <- readRDS(seurat_rds)
} else {
  brain <- LoadData("stxBrain", type = "anterior1")
  brain2 <- LoadData("stxBrain", type = "anterior2")
  brain3 <- LoadData("stxBrain", type = "posterior1")
  brain4 <- LoadData("stxBrain", type = "posterior2")

  brain <- SCTransform(brain, assay = "Spatial", verbose = FALSE)
  brain2 <- SCTransform(brain2, assay = "Spatial", verbose = FALSE)
  brain3 <- SCTransform(brain3, assay = "Spatial", verbose = FALSE)
  brain4 <- SCTransform(brain4, assay = "Spatial", verbose = FALSE)

  brain.merge <- merge(brain, y = c(brain2, brain3, brain4))
  DefaultAssay(brain.merge) <- "SCT"
  VariableFeatures(brain.merge) <- unique(c(
    VariableFeatures(brain),
    VariableFeatures(brain2),
    VariableFeatures(brain3),
    VariableFeatures(brain4)
  ))
  brain.merge <- RunPCA(brain.merge, verbose = FALSE)
  brain.merge <- FindNeighbors(brain.merge, dims = 1:30)
  brain.merge <- FindClusters(brain.merge, verbose = FALSE)
  brain.merge <- RunUMAP(brain.merge, dims = 1:30)
  saveRDS(brain.merge, seurat_rds)
}

viewer_mode <- if (wsi_has_vips()) "tiles" else "thumbnail"
html <- wsi_viewer_seurat_project(
  seurat = brain.merge,
  images = slice_images,
  image_names = names(slice_images),
  labels = c("Anterior 1", "Anterior 2", "Posterior 1", "Posterior 2"),
  reduction = "pca",
  dims = c(1, 2),
  coordinate_scale = "auto",
  mode = viewer_mode,
  output = file.path(save_dir, "seurat_4slides_project.html"),
  tile_dir = file.path(save_dir, "seurat_4slides_project_tiles"),
  open = interactive(),
  overwrite = TRUE,
  rebuild = FALSE
)

message("Viewer written to: ", html)
message("Mode: ", viewer_mode)
