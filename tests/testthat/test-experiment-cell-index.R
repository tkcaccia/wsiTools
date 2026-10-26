# Synthetic storage/identity fixtures; these never evaluate biological accuracy.
indexed_cell_fixture <- function(rows = 12L) {
  skip_if_not_installed("DBI"); skip_if_not_installed("RSQLite")
  root <- tempfile("synthetic-index-"); dir.create(root)
  image <- file.path(root,"image.ppm")
  writeBin(c(charToRaw("P6\n64 64\n255\n"),as.raw(rep(128,64*64*3))),image)
  ids <- sprintf("cell-%04d",seq_len(rows)); x <- seq_len(rows)*2; y <- rep(10,rows)
  values <- outer(seq_len(rows),seq_len(6),function(i,j) sin(i+j/3)+i*j/10)
  columns <- paste0("feat_",seq_len(6)); colnames(values)<-columns
  feature_path <- file.path(root,"features.csv")
  write.csv(cbind(data.frame(cell_id=ids,x=x,y=y),as.data.frame(values)),feature_path,row.names=FALSE)
  im <- list(path="image.ppm",sha256=wsi_experiment_sha256(image),width=64L,height=64L,mpp_x=.5,mpp_y=.5,
    origin_x=0,origin_y=0,coordinate_space="crop-native-pixels",frame_id="synthetic-frame",
    coordinate_convention="pixel-edge-origin_centres-in-native-pixels")
  f <- list(kind="cellvit-cell",observation_unit="cell",feature_set_id="synthetic-index-features",frame_id=im$frame_id,
    path="features.csv",sha256=wsi_experiment_sha256(feature_path),format="csv",row_count=rows,
    id_column="cell_id",x_column="x",y_column="y",feature_columns=as.list(columns),coordinate_frame="image",
    descriptor=list(backbone="HIPT",checkpoint_sha256=strrep("a",64),runtime_version="SYNTHETIC-interface-test",
      feature_definition="synthetic_analytic_cell_features",feature_count=6L,coordinate_frame="image",
      sampling=list(model_input_mpp=.25,patch_size_px=1024L,patch_overlap_px=64L,token_size_px=16L),
      preprocessing=list(runtime_wheel_sha256=strrep("f",64),normalization="SYNTHETIC",resampling="SYNTHETIC",token_provenance="SYNTHETIC")))
  model <- list(model_id="synthetic",backbone="HIPT",taxonomy="SYNTHETIC",status="available",class_names=list("A","B"),
    checkpoint_sha256=strrep("a",64),score_semantics="uncalibrated_pixel_type_vote_fraction")
  cells <- lapply(seq_len(rows),function(i) list(cell_id=ids[[i]],source_id=as.character(i),source="synthetic-test",
    x=x[[i]],y=y[[i]],predictions=list(synthetic=list(status="predicted",class_id=as.integer(i%%2),
      label=if(i%%2)"B"else"A",probability=i/(rows+1),probabilities=NULL,probability_vector_status="not_exported_by_upstream"))))
  data <- list(schema="cellphenotyper-cell-classification",schema_version=1L,image=im,feature_set=f,models=list(model),cells=cells)
  source <- file.path(root,"classification.json"); jsonlite::write_json(data,source,auto_unbox=TRUE,null="null",digits=17)
  db <- file.path(root,"cells.sqlite"); con<-DBI::dbConnect(RSQLite::SQLite(),db)
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  DBI::dbExecute(con,"CREATE TABLE metadata(key TEXT PRIMARY KEY,value TEXT NOT NULL)")
  DBI::dbExecute(con,"CREATE TABLE cells(row_id INTEGER PRIMARY KEY,cell_id TEXT UNIQUE,source_id TEXT UNIQUE,x REAL,y REAL,xmin REAL,ymin REAL,xmax REAL,ymax REAL,raw_json TEXT)")
  DBI::dbExecute(con,"CREATE TABLE predictions(row_id INTEGER,model_id TEXT,status TEXT,class_id INTEGER,label TEXT,probability REAL,PRIMARY KEY(row_id,model_id))")
  DBI::dbExecute(con,"CREATE TABLE features(row_id INTEGER PRIMARY KEY,values_json TEXT NOT NULL)")
  DBI::dbExecute(con,"CREATE VIRTUAL TABLE bbox_rtree USING rtree(row_id,xmin,xmax,ymin,ymax)")
  DBI::dbWriteTable(con,"cells",data.frame(row_id=seq_len(rows),cell_id=ids,source_id=as.character(seq_len(rows)),
    x=x,y=y,xmin=x-.5,ymin=y-.5,xmax=x+.5,ymax=y+.5,
    raw_json=vapply(cells,jsonlite::toJSON,character(1),auto_unbox=TRUE,null="null",digits=17)),append=TRUE)
  DBI::dbWriteTable(con,"features",data.frame(row_id=seq_len(rows),values_json=vapply(seq_len(rows),
    function(i)as.character(jsonlite::toJSON(unname(values[i,]),auto_unbox=FALSE,digits=17)),character(1))),append=TRUE)
  DBI::dbWriteTable(con,"predictions",data.frame(row_id=seq_len(rows),model_id="synthetic",status="predicted",
    class_id=as.integer(seq_len(rows)%%2),label=ifelse(seq_len(rows)%%2,"B","A"),probability=seq_len(rows)/(rows+1)),append=TRUE)
  DBI::dbExecute(con,"INSERT INTO bbox_rtree SELECT row_id,xmin,xmax,ymin,ymax FROM cells")
  DBI::dbExecute(con,"CREATE INDEX prediction_queue ON predictions(model_id,probability,row_id)")
  header<-data;header$cells<-list()
  metadata<-list(schema="cellphenotyper-cell-index",schema_version=1L,classification=list(path="classification.json",sha256=wsi_experiment_sha256(source)),
    classification_header=header,image=im,feature_set=f,feature_csv_sha256=f$sha256,models=list(model),cell_count=rows,sidecars=list())
  DBI::dbWriteTable(con,"metadata",data.frame(key=names(metadata),value=vapply(metadata,function(value)
    as.character(jsonlite::toJSON(value,auto_unbox=TRUE,null="null",digits=17)),character(1))),append=TRUE)
  DBI::dbDisconnect(con);on.exit(NULL,add=FALSE)
  manifest<-list(schema="cellphenotyper-experiment",schema_version=1L,experiment_id="synthetic-index",name="Synthetic index contract",
    tissues=list(list(tissue_id="t1",name="Synthetic tissue",image=im,feature_sets=list(f),artifacts=list(),
      cell_classification=list(path="classification.json",sha256=wsi_experiment_sha256(source),
        index=list(path="cells.sqlite",sha256=wsi_experiment_sha256(db),format="sqlite")))))
  path<-file.path(root,"experiment.cellphenotyper.json");jsonlite::write_json(manifest,path,auto_unbox=TRUE,null="null",digits=17)
  list(root=root,path=path,db=db,source=source,manifest=manifest)
}

indexed_fixture_rehash <- function(fixture) {
  fixture$manifest$tissues[[1L]]$cell_classification$index$sha256<-wsi_experiment_sha256(fixture$db)
  jsonlite::write_json(fixture$manifest,fixture$path,auto_unbox=TRUE,null="null",digits=17)
  fixture
}

test_that("indexed open loads metadata and bounded pages with exact stable IDs", {
  fixture<-indexed_cell_fixture();on.exit(unlink(fixture$root,recursive=TRUE),add=TRUE)
  experiment<-wsi_read_experiment(fixture$path);catalog<-wsi_experiment_cell_catalog(experiment)[[1L]]
  expect_true(catalog$indexed);expect_length(catalog$cells,0);expect_equal(catalog$cell_count,12)
  review<-wsi_experiment_review_context(experiment,strict=TRUE);expect_length(review$observations,0)
  state<-new.env();state$review_context<-review;state$review_checkpoint<-wsi_review_checkpoint(review)
  context<-wsi_experiment_prediction_context(experiment)
  request<-list(action="experiment_cell_index",mode="queue",tissue_id="t1",model_id="synthetic",threshold=1,limit=3)
  first<-wsi_prediction_response(context,state,request)$cell_index
  expect_length(first$cells,3);expect_true(first$truncated);expect_equal(first$total,12)
  all_zero<-request;all_zero$class_name<-"*";all_zero$threshold<-0
  expect_equal(wsi_prediction_response(context,state,all_zero)$cell_index$total,12)
  review_zero<-all_zero;review_zero$class_name<-"__review__"
  expect_equal(wsi_prediction_response(context,state,review_zero)$cell_index$total,0)
  request$cursor<-first$next_cursor;second<-wsi_prediction_response(context,state,request)$cell_index
  expect_length(intersect(vapply(first$cells,`[[`,character(1),"id"),vapply(second$cells,`[[`,character(1),"id")),0)
  request$class_name<-"A";expect_error(wsi_prediction_response(context,state,request),"cursor belongs")
  expect_error(wsi_prediction_response(context,state,list(action="experiment_cell_index",mode="cell",tissue_id="t1",cell_id="absent")),"absent")
  found<-wsi_prediction_response(context,state,list(action="experiment_cell_index",mode="cell",tissue_id="t1",cell_id=first$cells[[1L]]$id))$cell_index
  expect_identical(found$cells[[1L]]$cell_id,first$cells[[1L]]$cell_id)
})

test_that("viewport counts and density cover the whole query rather than a page sample", {
  fixture<-indexed_cell_fixture();on.exit(unlink(fixture$root,recursive=TRUE),add=TRUE)
  e<-wsi_read_experiment(fixture$path);context<-wsi_experiment_prediction_context(e)
  result<-wsi_prediction_response(context,new.env(),list(action="experiment_cell_index",mode="viewport",tissue_id="t1",limit=3,
    bbox=list(xmin=0,ymin=0,xmax=64,ymax=64)))$cell_index
  expect_true(result$truncated);expect_length(result$cells,3);expect_equal(result$total,12)
  expect_equal(sum(vapply(result$density$bins,`[[`,numeric(1),"count")),12)
  expect_lte(length(result$density$bins),1024)
  wildcard<-wsi_prediction_response(context,new.env(),list(action="experiment_cell_index",mode="viewport",tissue_id="t1",limit=3,
    model_id="synthetic",class_name="*",threshold=0,bbox=list(xmin=0,ymin=0,xmax=64,ymax=64)))$cell_index
  expect_equal(wildcard$total,12);expect_equal(wildcard$density$total,12)
  unknown<-wsi_prediction_response(context,new.env(),list(action="experiment_cell_index",mode="viewport",tissue_id="t1",limit=3,
    model_id="synthetic",class_name="__review__",threshold=0,bbox=list(xmin=0,ymin=0,xmax=64,ymax=64)))$cell_index
  expect_equal(unknown$total,0);expect_length(unknown$cells,0)
  uncertain<-wsi_prediction_response(context,new.env(),list(action="experiment_cell_index",mode="viewport",tissue_id="t1",limit=3,
    model_id="synthetic",class_name="__review__",threshold=.5,bbox=list(xmin=0,ymin=0,xmax=64,ymax=64)))$cell_index
  expect_equal(uncertain$total,6);expect_equal(uncertain$density$total,6)
  expect_error(wsi_prediction_response(context,new.env(),list(action="experiment_cell_index",mode="viewport",tissue_id="t1",limit=3,
    model_id="synthetic",class_name="unknown class",threshold=0,bbox=list(xmin=0,ymin=0,xmax=64,ymax=64))),"Unknown viewport class")
})

test_that("review subsets preserve earlier explicit decisions and fetch only selected features", {
  fixture<-indexed_cell_fixture();on.exit(unlink(fixture$root,recursive=TRUE),add=TRUE)
  e<-wsi_read_experiment(fixture$path);context<-wsi_experiment_prediction_context(e);state<-new.env()
  state$review_context<-wsi_experiment_review_context(e);state$review_checkpoint<-wsi_review_checkpoint(state$review_context)
  payload<-list(action="experiment_cell_index",mode="review_subset",tissue_id="t1",cell_ids=list("cell-0001","cell-0002"))
  first<-wsi_prediction_response(context,state,payload)
  record<-list(id=first$review_context$observations[[1L]]$id,status="reviewed",label="Expert A",reviewer="synthetic tester",updated_at="2026-10-20T10:00:00Z")
  state$review_checkpoint<-wsi_review_checkpoint(state$review_context,review_records=list(record),revision=1L)
  payload$cell_ids<-list("cell-0003","cell-0004");payload$checkpoint<-state$review_checkpoint
  second<-wsi_prediction_response(context,state,payload)
  expect_length(second$review_context$observations,4);expect_identical(second$review_checkpoint$review_records[[1L]]$label,"Expert A")
  expect_equal(second$review_checkpoint$revision,1)
  ids<-rev(vapply(second$review_context$observations,`[[`,character(1),"id"))
  features<-wsi_experiment_index_features(e,"synthetic-index-features",ids)
  expect_equal(dim(features$x),c(4,6));expect_identical(features$observations$id,ids)
  expect_false(any(c("label","source_id","probability")%in%colnames(features$x)))
  expect_null(second$features);expect_null(second$cell_index$features)
})

test_that("stale bytes, hidden SQL schemas and oversized requests fail closed", {
  fixture<-indexed_cell_fixture();on.exit(unlink(fixture$root,recursive=TRUE),add=TRUE)
  e<-wsi_read_experiment(fixture$path)
  con<-DBI::dbConnect(RSQLite::SQLite(),fixture$db);DBI::dbExecute(con,"UPDATE cells SET source_id='tampered' WHERE row_id=1");DBI::dbDisconnect(con)
  expect_error(wsi_experiment_cell_catalog(e),"SHA-256 mismatch")
  fixture<-indexed_fixture_rehash(fixture)
  con<-DBI::dbConnect(RSQLite::SQLite(),fixture$db);DBI::dbExecute(con,"CREATE VIEW hidden AS SELECT * FROM cells");DBI::dbDisconnect(con)
  fixture<-indexed_fixture_rehash(fixture)
  expect_error(wsi_read_experiment(fixture$path),"views and triggers")
})

test_that("actual 100k synthetic producer index stays bounded throughout R review", {
  path<-Sys.getenv("WSITOOLS_INDEXED_100K_MANIFEST","")
  skip_if(!nzchar(path),"Explicit immutable 100k synthetic producer manifest required")
  e<-wsi_read_experiment(path);catalog<-wsi_experiment_cell_catalog(e)[[1L]]
  expect_equal(catalog$cell_count,100000);expect_length(catalog$cells,0)
  context<-wsi_experiment_prediction_context(e);state<-new.env()
  state$review_context<-wsi_experiment_review_context(e,strict=TRUE)
  expect_length(state$review_context$observations,0)
  state$review_checkpoint<-wsi_review_checkpoint(state$review_context)
  tid<-e$tissues[[1L]]$tissue_id
  viewport<-wsi_prediction_response(context,state,list(action="experiment_cell_index",mode="viewport",tissue_id=tid,
    model_id="synthetic",bbox=list(xmin=0,ymin=0,xmax=1024,ymax=512),limit=500L))$cell_index
  expect_length(viewport$cells,500);expect_true(viewport$truncated);expect_equal(viewport$total,100000)
  expect_equal(viewport$density$total,100000);expect_lte(length(viewport$density$bins),1024)
  expect_equal(sum(vapply(viewport$summary,`[[`,numeric(1),"count")),100000)
  ids<-vapply(viewport$cells[1:4],`[[`,character(1),"cell_id")
  subset<-wsi_prediction_response(context,state,list(action="experiment_cell_index",mode="review_subset",tissue_id=tid,cell_ids=as.list(ids)))
  expect_length(subset$review_context$observations,4)
  features<-wsi_experiment_index_features(e,catalog$feature_set_id,vapply(subset$review_context$observations,`[[`,character(1),"id"))
  expect_equal(dim(features$x),c(4,11))
})

test_that("reviewed indexed PLS fitting and whole-slide CSV application stay on SQL subsets", {
  skip_if_not(wsi_has_fastpls(), "Optional CPU fastPLS is required")
  fixture<-indexed_cell_fixture();on.exit(unlink(fixture$root,recursive=TRUE),add=TRUE)
  e<-wsi_read_experiment(fixture$path);context<-wsi_experiment_prediction_context(e);state<-new.env()
  state$review_context<-wsi_experiment_review_context(e);state$review_checkpoint<-wsi_review_checkpoint(state$review_context)
  subset<-wsi_prediction_response(context,state,list(action="experiment_cell_index",mode="review_subset",tissue_id="t1",
    cell_ids=as.list(sprintf("cell-%04d",1:4))))
  ids<-vapply(subset$review_context$observations,`[[`,character(1),"id")
  records<-lapply(seq_along(ids),function(i)list(id=ids[[i]],status="reviewed",label=if(i<=2)"Human A"else"Human B",
    reviewer="synthetic interface tester",updated_at="2026-10-20T10:00:00Z"))
  checkpoint<-wsi_review_checkpoint(state$review_context,selected_ids=ids,review_records=records,revision=1L)
  state$review_checkpoint<-checkpoint
  local_mocked_bindings(wsi_experiment_features=function(...)stop("Full feature CSV materialization is forbidden in this indexed test"),.package="wsiTools")
  result<-wsi_experiment_train_reviewed_response(context,state,list(feature_source="experiment:synthetic-index-features",train_ids=as.list(ids),
    checkpoint=checkpoint,context_id=state$review_context$context_id,source_signature=state$review_context$source_signature,ncomp=2,max_features=4))
  expect_equal(nrow(result),12);expect_equal(attr(result,"total_count"),12)
  expect_true(file.exists(attr(result,"result_path")));expect_true(file.exists(attr(result,"classifier_path")))
  expect_equal(nrow(read.csv(attr(result,"result_path"))),12)
  expect_true(all(result$predicted%in%c("Human A","Human B")))
  expect_length(state$experiment_classifier$training_ids,4)
  expect_length(state$experiment_classifier_registry,1)
  visible<-wsi_prediction_response(context,state,list(action="experiment_cell_index",mode="viewport",tissue_id="t1",class_name="*",
    bbox=list(xmin=0,ymin=0,xmax=64,ymax=64)))$cell_index
  expect_true(all(vapply(visible$cells,function(cell) cell$reviewed_prediction$label%in%c("Human A","Human B"),logical(1))))
  expect_true(all(vapply(visible$cells,function(cell) cell$predictions$synthetic$label%in%c("A","B"),logical(1))))
  preview<-result[1:2,,drop=FALSE];attr(preview,"total_count")<-12
  layer<-wsi_prediction_layer(preview)
  expect_false(layer$visible);expect_match(layer$name,"2 of 12")
})

test_that("immutable source changes and hidden triggers cannot enter indexed browsing", {
  fixture<-indexed_cell_fixture();on.exit(unlink(fixture$root,recursive=TRUE),add=TRUE)
  e<-wsi_read_experiment(fixture$path)
  cat(" ",file=fixture$source,append=TRUE)
  expect_error(wsi_experiment_cell_catalog(e),"SHA-256 mismatch")
  fixture$manifest$tissues[[1L]]$cell_classification$sha256<-wsi_experiment_sha256(fixture$source)
  con<-DBI::dbConnect(RSQLite::SQLite(),fixture$db)
  DBI::dbExecute(con,"CREATE TRIGGER hidden AFTER INSERT ON cells BEGIN DELETE FROM features; END")
  DBI::dbDisconnect(con);fixture<-indexed_fixture_rehash(fixture)
  expect_error(wsi_read_experiment(fixture$path),"views and triggers")
})

indexed_display_edit <- function(fixture, cell = NULL, header = NULL, sql = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con))
  if (!is.null(cell)) {
    value <- jsonlite::fromJSON(DBI::dbGetQuery(con, "SELECT raw_json FROM cells WHERE row_id=1")$raw_json[[1L]], simplifyVector = FALSE)
    value <- cell(value)
    DBI::dbExecute(con, "UPDATE cells SET raw_json=? WHERE row_id=1",
      params = list(as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = 17))))
  }
  if (!is.null(header)) {
    value <- jsonlite::fromJSON(DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key='classification_header'")$value[[1L]], simplifyVector = FALSE)
    DBI::dbExecute(con, "UPDATE metadata SET value=? WHERE key='classification_header'",
      params = list(as.character(jsonlite::toJSON(header(value), auto_unbox = TRUE, null = "null", digits = 17))))
  }
  if (!is.null(sql)) DBI::dbExecute(con, sql)
  DBI::dbDisconnect(con); on.exit(NULL)
  indexed_fixture_rehash(fixture)
}

indexed_display_request <- function(fixture, ...) {
  experiment <- wsi_read_experiment(fixture$path)
  wsi_prediction_response(wsi_experiment_prediction_context(experiment), new.env(),
    c(list(action = "experiment_cell_index", mode = "display_points", tissue_id = "t1", model_id = "synthetic"), list(...)))$cell_index
}

test_that("compact display pages enumerate all cells independently of rich review limits", {
  fixture <- indexed_cell_fixture(); on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  local_mocked_bindings(wsi_cell_index_runtime_rows = function(...) stop("Rich cell materialization is forbidden"),
    wsi_experiment_index_features = function(...) stop("Feature materialization is forbidden"), .package = "wsiTools")
  points <- list(); cursor <- NULL; scope <- NULL
  repeat {
    result <- indexed_display_request(fixture, display_limit = 3L, limit = 99999L, cursor = cursor)
    expect_length(result$points, 3); expect_length(result$cells, 0); expect_equal(result$total, 12)
    expect_null(result$features); expect_null(result$density)
    expect_type(result$query_scope, "character"); expect_length(result$query_scope, 1)
    if (!is.null(scope)) expect_identical(result$query_scope, scope)
    scope <- result$query_scope; points <- c(points, result$points); cursor <- result$next_cursor
    if (is.null(cursor)) break
  }
  expect_length(points, 12); expect_equal(anyDuplicated(vapply(points, `[[`, character(1), "id")), 0L)
  expect_equal(table(vapply(points, function(point) point$predictions$synthetic$label, character(1))), c(A = 6L, B = 6L), ignore_attr = TRUE)
  expect_true(all(vapply(points, function(point) identical(names(point$predictions), "synthetic"), logical(1))))
  expect_true(all(vapply(points, function(point) identical(point$source_frame_id, "synthetic-frame"), logical(1))))
  expect_true(all(vapply(points, function(point) is.null(point$contour) && is.null(point$raw_json) && is.null(point$features), logical(1))))
  expect_length(indexed_display_request(fixture, limit = 1L)$points, 12)
  expect_error(indexed_display_request(fixture, display_limit = 10001), "10000-point")
  expect_error(indexed_display_request(fixture, display_limit = 0), "positive")
})

test_that("compact display scopes bind exact identities, models, filters and native viewport", {
  fixture <- indexed_cell_fixture(); on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  fixture <- indexed_display_edit(fixture, header = function(value) {
    value$models <- c(value$models, list(list(model_id = "unavailable", backbone = "HIPT", taxonomy = "UNAVAILABLE",
      status = "unavailable", reason = "No compatible checkpoint", class_names = list())))
    value
  })
  first <- indexed_display_request(fixture, display_limit = 3)
  for (key in c("source_sha256", "index_sha256", "feature_set_id", "frame_id", "source_frame_id")) {
    request <- list(display_limit = 3); request[[key]] <- "foreign-source"
    expect_error(do.call(indexed_display_request, c(list(fixture), request)), "stale")
  }
  expect_error(indexed_display_request(fixture, cursor = first$next_cursor, class_name = "A"), "different source/filter")
  expect_error(indexed_display_request(fixture, cursor = first$next_cursor, threshold = 0.5), "different source/filter")
  expect_error(indexed_display_request(fixture, cursor = first$next_cursor, bbox = list(xmin = 0, ymin = 0, xmax = 32, ymax = 32)), "different source/filter")
  experiment <- wsi_read_experiment(fixture$path); context <- wsi_experiment_prediction_context(experiment)
  expect_error(wsi_prediction_response(context, new.env(), list(action = "experiment_cell_index", mode = "display_points",
    tissue_id = "t1", model_id = "unavailable", cursor = first$next_cursor)), "different source/filter")
  neutral <- wsi_prediction_response(context, new.env(), list(action = "experiment_cell_index", mode = "display_points",
    tissue_id = "t1", model_id = "unavailable"))$cell_index
  expect_length(neutral$points, 12)
  expect_true(all(vapply(neutral$points, function(point) identical(point$predictions$unavailable$status, "unavailable") &&
    is.null(point$predictions$unavailable$label) && is.null(point$predictions$unavailable$probability), logical(1))))
  oversized <- indexed_display_edit(fixture, header = function(value) { value$models[[2L]]$reason <- strrep("x", 4097); value })
  oversized_experiment <- wsi_read_experiment(oversized$path)
  expect_error(wsi_prediction_response(wsi_experiment_prediction_context(oversized_experiment), new.env(),
    list(action = "experiment_cell_index", mode = "display_points", tissue_id = "t1", model_id = "unavailable")), "bounded scalar schema")
  fixture <- indexed_display_edit(oversized, header = function(value) { value$models[[2L]]$reason <- "No compatible checkpoint"; value })
  expect_equal(indexed_display_request(fixture, class_name = "A")$total, 6)
  expect_equal(indexed_display_request(fixture, class_name = "__review__", threshold = 0.5)$total, 6)
  expect_error(indexed_display_request(fixture, class_name = "unknown"), "Unknown display class")
  expect_error(indexed_display_request(fixture, threshold = 2), "between zero and one")
  view <- indexed_display_request(fixture, bbox = list(xmin = 9, ymin = 0, xmax = 15, ymax = 64))
  expect_equal(view$total, 3); expect_equal(vapply(view$points, `[[`, numeric(1), "x"), c(10, 12, 14))
  expect_error(indexed_display_request(fixture, bbox = list(xmin = 9, ymin = 0, xmax = 9, ymax = 64)), "positive area")
  expect_error(indexed_display_request(fixture, cursor = "not-json"), "Invalid cell display page cursor")
  expect_error(wsi_cell_index_display_points(experiment, experiment$tissues[[1L]],
    wsi_experiment_cell_catalog(experiment)[[1L]], list()), "display model_id")
})

test_that("compact segmentation and tissue scalars preserve explicit unknown geometry", {
  fixture <- indexed_cell_fixture(); on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  fixture <- indexed_display_edit(fixture, cell = function(value) {
    value$segmentation_review_status <- "unavailable"; value$segmentation_disagreement <- TRUE
    value$segmentation_review_required <- TRUE
    value$segmentation_geometry_unavailable_reason <- "Original contour is self-intersecting; no repair applied."
    value$tissue_context <- list(label = "stroma", forbidden_large_polygon = rep(list(c(0, 0)), 100))
    value
  })
  point <- indexed_display_request(fixture, display_limit = 1)$points[[1L]]
  expect_identical(point$segmentation_review_status, "unavailable")
  expect_true(point$segmentation_disagreement); expect_true(point$segmentation_review_required)
  expect_match(point$segmentation_geometry_unavailable_reason, "self-intersecting")
  expect_identical(point$tissue_context, list(label = "stroma"))
  expect_null(point$segmentation_iou); expect_null(point$contour)
  fixture <- indexed_display_edit(fixture, cell = function(value) { value$segmentation_disagreement <- 1; value })
  expect_error(indexed_display_request(fixture), "invalid status or boolean")
  fixture <- indexed_display_edit(fixture, cell = function(value) { value$segmentation_disagreement <- TRUE; value$tissue_context$label <- list(1, 2); value })
  expect_error(indexed_display_request(fixture), "invalid JSON type")
})

test_that("compact display rejects malformed canonical coordinates and model assignments", {
  mutations <- list(
    list(sql = "UPDATE cells SET x=-1 WHERE row_id=1", pattern = "coordinates"),
    list(cell = function(value) { value$x <- NULL; value }, pattern = "coordinates"),
    list(cell = function(value) { value$x <- "2"; value }, pattern = "coordinates"),
    list(sql = "DELETE FROM predictions WHERE row_id=1", pattern = "missing display prediction"),
    list(sql = "UPDATE predictions SET label='wrong' WHERE row_id=1", pattern = "original scalar model evidence"),
    list(sql = "UPDATE predictions SET probability=2 WHERE row_id=1", pattern = "original scalar model evidence"),
    list(cell = function(value) { value$predictions$synthetic$probability_vector_status <- NULL; value }, pattern = "unavailable probability-vector"),
    list(sql = "UPDATE cells SET row_id=-1 WHERE row_id=1", pattern = "positive immutable row identities"))
  for (mutation in mutations) {
    fixture <- indexed_cell_fixture()
    tryCatch({
      fixture <- indexed_display_edit(fixture, cell = mutation$cell, sql = mutation$sql)
      expect_error(indexed_display_request(fixture), mutation$pattern)
    }, finally = unlink(fixture$root, recursive = TRUE))
  }
})

test_that("native unavailable scores remain null and response/scalar budgets stay strict", {
  fixture <- indexed_cell_fixture(); on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  fixture <- indexed_display_edit(fixture, cell = function(value) { value$predictions$synthetic$probability <- NULL; value },
    sql = "UPDATE predictions SET probability=NULL WHERE row_id=1")
  expect_error(indexed_display_request(fixture), "finite uncalibrated fraction")
  fixture <- indexed_display_edit(fixture, header = function(value) {
    value$models[[1L]]$score_semantics <- "uncalibrated_softmax_cell_class_vote_counts"; value
  })
  point <- indexed_display_request(fixture, display_limit = 1)$points[[1L]]
  expect_identical(point$predictions$synthetic$status, "predicted")
  expect_null(point$predictions$synthetic$probability)
  encoded <- jsonlite::toJSON(point, auto_unbox = TRUE, null = "null")
  expect_match(encoded, '"probability":null', fixed = TRUE)
  withr::local_options(wsiTools.cell_index_display_max_response_bytes = 1024L)
  expect_error(indexed_display_request(fixture, display_limit = 3), "bounded response budget")
  options(wsiTools.cell_index_display_max_response_bytes = 8 * 1024^2)
  fixture <- indexed_display_edit(fixture, cell = function(value) { value$source_id <- strrep("x", 5000); value },
    sql = paste0("UPDATE cells SET source_id='", strrep("x", 5000), "' WHERE row_id=1"))
  expect_error(indexed_display_request(fixture), "bounded scalar schema")
})

test_that("actual 100k compact display streams every ID with full class counts and bounded pages", {
  path <- Sys.getenv("WSITOOLS_INDEXED_100K_MANIFEST", "")
  skip_if(!nzchar(path), "Explicit immutable 100k synthetic producer manifest required")
  experiment <- wsi_read_experiment(path); tissue <- experiment$tissues[[1L]]
  context <- wsi_experiment_prediction_context(experiment); state <- new.env()
  local_mocked_bindings(wsi_cell_index_runtime_rows = function(...) stop("Rich cell JSON materialization is forbidden"),
    wsi_experiment_index_features = function(...) stop("Cell feature materialization is forbidden"), .package = "wsiTools")
  ids <- character(); labels <- character(); cursor <- NULL; pages <- 0L; max_bytes <- 0
  elapsed <- system.time(repeat {
    result <- wsi_prediction_response(context, state, list(action = "experiment_cell_index", mode = "display_points",
      tissue_id = tissue$tissue_id, model_id = "synthetic", display_limit = 5000L, cursor = cursor))$cell_index
    pages <- pages + 1L
    expect_lte(length(result$points), 5000); expect_length(result$cells, 0)
    expect_equal(result$total, 100000); expect_null(result$features); expect_null(result$density)
    ids <- c(ids, vapply(result$points, `[[`, character(1), "id"))
    labels <- c(labels, vapply(result$points, function(point) point$predictions$synthetic$label, character(1)))
    max_bytes <- max(max_bytes, nchar(jsonlite::toJSON(list(ok = TRUE, cell_index = result), auto_unbox = TRUE, null = "null", digits = 17), type = "bytes"))
    cursor <- result$next_cursor
    if (is.null(cursor)) break
    if (pages > 20L) stop("Display stream failed to terminate within the exact 100k population")
  })[["elapsed"]]
  expect_equal(pages, 20L); expect_length(ids, 100000); expect_equal(anyDuplicated(ids), 0L)
  counts <- table(labels); expect_length(counts, 5L); expect_true(all(counts == 20000L))
  expect_lte(max_bytes, 8 * 1024^2)
  catalog <- wsi_experiment_cell_catalog(experiment)[[1L]]
  con <- wsi_cell_index_connection(catalog$index_reference)
  expected <- DBI::dbGetQuery(con, "SELECT cell_id FROM cells ORDER BY row_id")$cell_id
  DBI::dbDisconnect(con)
  expect_identical(ids, wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, catalog$feature_set_id, expected))
  message(sprintf("100k compact display: %d pages, %.3f seconds, max %d response bytes", pages, elapsed, max_bytes))
})

indexed_qc_fixture <- function() {
  fixture <- indexed_cell_fixture()
  payload <- jsonlite::fromJSON(fixture$source, simplifyVector = FALSE)
  image <- payload$image
  manifest <- list(schema = "cellphenotyper-cell-features", schema_version = 1L, image = image,
    feature_set = payload$feature_set, source = list(cells_sha256 = strrep("e", 64)))
  manifest_path <- file.path(fixture$root, "feature_manifest.json")
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, null = "null", digits = 17)
  manifest_sha <- wsi_experiment_sha256(manifest_path)
  payload$provenance <- list(feature_manifest_sha256 = manifest_sha)
  jsonlite::write_json(payload, fixture$source, auto_unbox = TRUE, null = "null", digits = 17)
  cells <- payload$cells
  rows <- lapply(seq_along(cells), function(i) {
    unavailable <- i <= 3L
    list(cell_id = cells[[i]]$cell_id, source_id = cells[[i]]$source_id, original_cell = cells[[i]],
      segmentation_review_status = if (unavailable) "unavailable" else "available",
      segmentation_disagreement = unavailable, segmentation_review_required = unavailable,
      segmentation_split_signal = FALSE, segmentation_merge_signal = FALSE,
      segmentation_review_reason = if (unavailable) list("invalid_original_geometry") else list(),
      segmentation_action = "retain_original", segmentation_reference_quality = "model_agreement_only",
      segmentation_auto_adjustment_eligible = FALSE,
      segmentation_geometry_unavailable_reason = if (unavailable) "Synthetic invalid original contour; unchanged." else NULL,
      segmentation_iou = if (unavailable) NULL else .9, segmentation_cellvit_coverage = if (unavailable) NULL else .95,
      segmentation_stardist_coverage = if (unavailable) NULL else .96)
  })
  frame <- image[c("width", "height", "mpp_x", "mpp_y", "coordinate_space", "origin_x", "origin_y")]
  frame$source_image_sha256 <- image$sha256
  report <- list(schema = "cellphenotyper.segmentation-review/v1", frame = frame,
    inputs = list(cellvit_cells = list(sha256 = manifest$source$cells_sha256), source_image = list(sha256 = image$sha256)),
    reference_quality = "model_agreement_only", cell_type_evidence_changed = FALSE,
    original_cell_count = 12L, candidate_cell_count = 12L, rasterized_cell_count = 9L,
    geometry_unavailable_cell_count = 3L, quarantine_invalid_geometry = TRUE, cells = rows)
  review_path <- file.path(fixture$root, "segmentation_qc.json")
  jsonlite::write_json(report, review_path, auto_unbox = TRUE, null = "null", digits = 17)
  review_sha <- wsi_experiment_sha256(review_path)
  sidecar <- list(role = "segmentation_review", path = basename(review_path), sha256 = review_sha,
    schema = report$schema, indexed_table = "segmentation_review", source_frame_id = image$frame_id,
    coordinate_convention = image$coordinate_convention, frame = frame, feature_manifest_sha256 = manifest_sha,
    source_cells_sha256 = manifest$source$cells_sha256, cell_count = 12L, original_cell_count = 12L, candidate_cell_count = 12L,
    rasterized_cell_count = 9L, geometry_unavailable_cell_count = 3L, quarantine_invalid_geometry = TRUE,
    explicit_geometry_status = TRUE, reference_quality = "model_agreement_only", cell_type_evidence_changed = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  metadata <- DBI::dbGetQuery(con, "SELECT key,value FROM metadata")
  update <- function(key, value) DBI::dbExecute(con, "INSERT OR REPLACE INTO metadata(key,value) VALUES(?,?)",
    params = list(key, as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = 17))))
  header <- payload; header$cells <- list()
  update("classification_header", header)
  update("classification", list(path = basename(fixture$source), sha256 = wsi_experiment_sha256(fixture$source)))
  update("feature_manifest", list(path = basename(manifest_path), sha256 = manifest_sha))
  update("sidecars", list(sidecar))
  DBI::dbExecute(con, "CREATE TABLE segmentation_review(row_id INTEGER PRIMARY KEY,cell_id TEXT UNIQUE,source_id TEXT UNIQUE,qc_json TEXT NOT NULL)")
  DBI::dbWriteTable(con, "segmentation_review", data.frame(row_id = seq_along(rows),
    cell_id = vapply(cells, `[[`, character(1), "cell_id"), source_id = vapply(cells, `[[`, character(1), "source_id"),
    qc_json = vapply(rows, function(row) as.character(jsonlite::toJSON(row, auto_unbox = TRUE, null = "null", digits = 17)), character(1))), append = TRUE)
  DBI::dbDisconnect(con)
  ref <- fixture$manifest$tissues[[1L]]$cell_classification
  ref$sha256 <- wsi_experiment_sha256(fixture$source)
  ref$feature_manifest <- list(path = basename(manifest_path), sha256 = manifest_sha)
  ref$segmentation_review <- list(path = basename(review_path), sha256 = review_sha,
    source_frame_id = image$frame_id, coordinate_convention = image$coordinate_convention)
  fixture$manifest$tissues[[1L]]$cell_classification <- ref
  fixture$review_path <- review_path
  indexed_fixture_rehash(fixture)
}

test_that("indexed QC stays separate from immutable cells and hydrates only bounded rich rows", {
  fixture <- indexed_qc_fixture(); on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  experiment <- wsi_read_experiment(fixture$path)
  catalog <- wsi_experiment_cell_catalog(experiment)[[1L]]
  expect_length(catalog$cells, 0L); expect_equal(catalog$segmentation_review$cell_count, 12L)
  expect_equal(catalog$segmentation_review$rasterized_cell_count, 9L)
  expect_equal(catalog$segmentation_review$geometry_unavailable_cell_count, 3L)
  context <- wsi_experiment_prediction_context(experiment); state <- new.env()
  rich <- wsi_prediction_response(context, state, list(action = "experiment_cell_index", mode = "queue", tissue_id = "t1", limit = 2L))$cell_index
  expect_length(rich$cells, 2L)
  first <- rich$cells[[1L]]
  expect_identical(first$segmentation_review_status, "unavailable")
  expect_true(first$segmentation_disagreement); expect_true(first$segmentation_review_required)
  expect_match(first$segmentation_geometry_unavailable_reason, "unchanged")
  for (key in c("segmentation_iou", "segmentation_cellvit_coverage", "segmentation_stardist_coverage")) {
    expect_true(key %in% names(first)); expect_null(first[[key]])
  }
  expect_identical(first$segmentation_review_source_sha256, wsi_experiment_sha256(fixture$review_path))
  con <- wsi_cell_index_connection(catalog$index_reference)
  original <- jsonlite::fromJSON(DBI::dbGetQuery(con, "SELECT raw_json FROM cells WHERE row_id=1")$raw_json[[1L]], simplifyVector = FALSE)
  DBI::dbDisconnect(con)
  expect_identical(first$predictions, original$predictions)
  expect_null(original$segmentation_review_status)
  subset <- wsi_prediction_response(context, state, list(action = "experiment_cell_index", mode = "review_subset", tissue_id = "t1",
    cell_ids = list("cell-0001", "cell-0004")))
  expect_length(subset$review_context$observations, 2L)
  expect_identical(subset$cell_index$cells[[1L]]$segmentation_review_status, "unavailable")
  expect_identical(subset$cell_index$cells[[2L]]$segmentation_review_status, "available")
})

test_that("compact indexed QC display extracts all scalar states without rich QC JSON hydration", {
  fixture <- indexed_qc_fixture(); on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  local_mocked_bindings(wsi_cell_index_hydrate_review = function(...) stop("Rich review hydration is forbidden"),
    wsi_cell_index_runtime_rows = function(...) stop("Rich cell rows are forbidden"), .package = "wsiTools")
  display <- indexed_display_request(fixture)
  expect_length(display$points, 12L); expect_length(display$cells, 0L)
  expect_equal(sum(vapply(display$points, function(point) point$segmentation_review_status == "unavailable", logical(1))), 3L)
  expect_equal(sum(vapply(display$points, function(point) point$segmentation_review_status == "available", logical(1))), 9L)
  expect_true(display$points[[1L]]$segmentation_review_required)
  expect_match(display$points[[1L]]$segmentation_geometry_unavailable_reason, "unchanged")
  expect_null(display$points[[1L]]$segmentation_iou)
  expect_false(display$points[[4L]]$segmentation_disagreement)
  expect_true(all(vapply(display$points, function(point) identical(point$predictions$synthetic$status, "predicted"), logical(1))))
})

test_that("indexed QC rejects stale files, absent bindings and contradictory review rows", {
  for (failure in c("source", "registration", "missing", "null", "geometry", "context")) {
    fixture <- indexed_qc_fixture()
    tryCatch({
      if (failure == "source") {
        cat(" ", file = fixture$review_path, append = TRUE)
        expect_error(wsi_read_experiment(fixture$path), "SHA-256 mismatch")
      } else if (failure == "registration") {
        fixture$manifest$tissues[[1L]]$cell_classification$segmentation_review <- NULL
        jsonlite::write_json(fixture$manifest, fixture$path, auto_unbox = TRUE, null = "null")
        expect_error(wsi_read_experiment(fixture$path), "registered review")
      } else if (failure == "context") {
        fixture$manifest$tissues[[1L]]$cell_classification$tissue_context <- list(path = "not-supported.json")
        jsonlite::write_json(fixture$manifest, fixture$path, auto_unbox = TRUE, null = "null")
        expect_error(wsi_read_experiment(fixture$path), "context sidecar")
      } else {
        con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
        if (failure == "missing") DBI::dbExecute(con, "DELETE FROM segmentation_review WHERE row_id=1") else {
          row <- jsonlite::fromJSON(DBI::dbGetQuery(con, "SELECT qc_json FROM segmentation_review WHERE row_id=1")$qc_json[[1L]], simplifyVector = FALSE)
          if (failure == "null") row$segmentation_iou <- 0 else row$original_cell$x <- 9
          DBI::dbExecute(con, "UPDATE segmentation_review SET qc_json=? WHERE row_id=1",
            params = list(as.character(jsonlite::toJSON(row, auto_unbox = TRUE, null = "null", digits = 17))))
        }
        DBI::dbDisconnect(con); fixture <- indexed_fixture_rehash(fixture)
        if (failure == "missing") expect_error(wsi_read_experiment(fixture$path), "every canonical/source cell") else
          expect_error(indexed_display_request_rich <- {
            experiment <- wsi_read_experiment(fixture$path)
            wsi_prediction_response(wsi_experiment_prediction_context(experiment), new.env(),
              list(action = "experiment_cell_index", mode = "cell", tissue_id = "t1", cell_id = "cell-0001"))
          }, if (failure == "null") "explicit null metrics" else "original geometry")
      }
    }, finally = unlink(fixture$root, recursive = TRUE))
  }
})

test_that("indexed overlap unavailable evidence has a separate explicit opt-in and no invented scores", {
  fixture <- indexed_qc_fixture(); on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  report <- jsonlite::fromJSON(fixture$review_path, simplifyVector = FALSE)
  report$quarantine_invalid_geometry <- FALSE; report$quarantine_overlapping_geometry <- TRUE
  report$invalid_geometry_unavailable_cell_count <- 0L; report$overlapping_geometry_unavailable_cell_count <- 3L
  for (i in 1:3) {
    report$cells[[i]]$segmentation_review_reason <- list("overlapping_original_geometry")
    report$cells[[i]]$segmentation_geometry_unavailable_reason <- "Original native raster collision; every participant retained."
  }
  jsonlite::write_json(report, fixture$review_path, auto_unbox = TRUE, null = "null", digits = 17)
  sha <- wsi_experiment_sha256(fixture$review_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  sidecars <- jsonlite::fromJSON(DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key='sidecars'")$value[[1L]], simplifyVector = FALSE)
  sidecars[[1L]]$sha256 <- sha; sidecars[[1L]]$quarantine_invalid_geometry <- FALSE; sidecars[[1L]]$quarantine_overlapping_geometry <- TRUE
  sidecars[[1L]]$invalid_geometry_unavailable_cell_count <- 0L; sidecars[[1L]]$overlapping_geometry_unavailable_cell_count <- 3L
  DBI::dbExecute(con, "UPDATE metadata SET value=? WHERE key='sidecars'", params = list(as.character(jsonlite::toJSON(sidecars, auto_unbox = TRUE, null = "null", digits = 17))))
  for (i in 1:3) DBI::dbExecute(con, "UPDATE segmentation_review SET qc_json=? WHERE row_id=?", params = list(
    as.character(jsonlite::toJSON(report$cells[[i]], auto_unbox = TRUE, null = "null", digits = 17)), i))
  DBI::dbDisconnect(con)
  fixture$manifest$tissues[[1L]]$cell_classification$segmentation_review$sha256 <- sha
  fixture <- indexed_fixture_rehash(fixture)
  compact <- indexed_display_request(fixture)
  expect_length(compact$points, 12L)
  expect_equal(sum(vapply(compact$points, function(point) point$segmentation_review_status == "unavailable", logical(1))), 3L)
  experiment <- wsi_read_experiment(fixture$path)
  rich <- wsi_prediction_response(wsi_experiment_prediction_context(experiment), new.env(),
    list(action = "experiment_cell_index", mode = "cell", tissue_id = "t1", cell_id = "cell-0001"))$cell_index$cells[[1L]]
  expect_identical(rich$segmentation_review_reason, list("overlapping_original_geometry")); expect_null(rich$segmentation_iou)
  expect_false(rich$segmentation_auto_adjustment_eligible)
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  sidecars[[1L]]$quarantine_overlapping_geometry <- FALSE
  DBI::dbExecute(con, "UPDATE metadata SET value=? WHERE key='sidecars'", params = list(as.character(jsonlite::toJSON(sidecars, auto_unbox = TRUE, null = "null", digits = 17))))
  DBI::dbDisconnect(con); fixture <- indexed_fixture_rehash(fixture)
  expect_error(wsi_read_experiment(fixture$path), "separate explicit quarantine")
})
