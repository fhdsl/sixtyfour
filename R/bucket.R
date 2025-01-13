bucket_checks <- function(bucket) {
  stop_if_not(rlang::is_character(bucket), "bucket must be character")
  stop_if_not(length(bucket) == 1, "length(bucket) != 1")
}

#' Check if an S3 bucket exists
#'
#' @export
#' @param bucket (character) bucket name; must be length 1. required
#' @note internally uses
#' [head_bucket](https://www.paws-r-sdk.com/docs/s3_head_bucket/)
#' @family buckets
#' @return a single boolean (logical)
#' @examples \dontrun{
#' # exists
#' aws_bucket_exists(bucket = "s64-test-2")
#' # does not exist
#' aws_bucket_exists(bucket = "no-bucket")
#' }
aws_bucket_exists <- function(bucket) {
  bucket_checks(bucket)
  res <- tryCatch(
    {
      con_s3()$head_bucket(Bucket = bucket)
    },
    error = function(e) e
  )
  !inherits(res, c("error", "error_response"))
}

#' Create an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to
#' [create_bucket](https://www.paws-r-sdk.com/docs/s3_create_bucket/)
#' @note Requires the env var `AWS_REGION`
#' @return the bucket path (character)
#' @family buckets
#' @examples \dontrun{
#' aws_bucket_create(bucket = "s64-test-2")
#' }
aws_bucket_create <- function(bucket, ...) {
  bucket_checks(bucket)
  con_s3()$create_bucket(
    Bucket = bucket,
    CreateBucketConfiguration =
      list(LocationConstraint = env_var("AWS_REGION")), ...
  )$Location
}

#' Create a bucket if it does not exist
#' @inheritParams aws_bucket_delete
#' @keywords internal
bucket_create_if_not <- function(bucket, force = FALSE) {
  if (!aws_bucket_exists(bucket)) {
    if (!force) {
      if (yesno("{.strong {bucket}} does not exist. Create it?")) {
        cli::cli_inform("Exiting without creating bucket {.strong {bucket}}")
        cli::cli_inform("Run again & respond affirmatively
          or use `aws_bucket_create`")
        return(invisible())
      }
    }
    aws_bucket_create(bucket)
  }
}

#' Delete an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param force (logical) force deletion without going through the prompt.
#' default: `FALSE`. Should only be set to `TRUE` when required for
#' non-interactive use.
#' @param ... named parameters passed on to
#' [delete_bucket](https://www.paws-r-sdk.com/docs/s3_delete_bucket/)
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @family buckets
#' @return `NULL`, invisibly
#' @examplesIf interactive()
#' bucket_name <- "bucket-to-delete-113"
#' if (!aws_bucket_exists(bucket_name)) {
#'   aws_bucket_create(bucket = bucket_name)
#'   aws_buckets()
#'   aws_bucket_delete(bucket = bucket_name)
#'   aws_buckets()
#' }
aws_bucket_delete <- function(bucket, force = FALSE, ...) {
  bucket_checks(bucket)
  if (!force) {
    if (yesno("Are you sure you want to delete {.strong {bucket}}?")) {
      return(invisible())
    }
  }
  con_s3()$delete_bucket(Bucket = bucket, ...)
  return(invisible())
}

#' Delete an S3 bucket
#'
#' Takes care of deleting bucket objects, so that the bucket itself
#' can be deleted cleanly
#'
#' @export
#' @importFrom purrr safely
#' @inheritParams aws_bucket_delete
#' @section What is magical:
#' - Exits early if bucket does not exist
#' - Checks for any objects in the bucket and deletes any present
#' - Deletes bucket after deleting objects
#' @family buckets
#' @family magicians
#' @return `NULL`, invisibly
#' @examplesIf interactive()
#' # bucket does not exist
#' six_bucket_delete("notabucket")
#'
#' # bucket exists w/o objects
#' bucket <- random_string("bucket")
#' aws_bucket_create(bucket)
#' six_bucket_delete(bucket)
#'
#' # bucket exists w/ objects (files and directories with files)
#' bucket <- random_string("bucket")
#' aws_bucket_create(bucket)
#' demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' aws_file_upload(
#'   c(demo_rds_file, links_file),
#'   s3_path(bucket, c(basename(demo_rds_file), basename(links_file)))
#' )
#' aws_file_upload(
#'   c(demo_rds_file, links_file),
#'   s3_path(
#'     bucket, "newfolder",
#'     c(basename(demo_rds_file), basename(links_file))
#'   )
#' )
#' aws_bucket_list_objects(bucket)
#' six_bucket_delete(bucket)
six_bucket_delete <- function(bucket, force = FALSE, ...) {
  msg_no_objects <- c(
    "Are you sure you want to delete {.strong {bucket}}?"
  )
  msg_with_objects <- c(
    "Are you sure you want to delete {.strong {bucket}} ",
    "and its {.strong {length(objects$result$uri)}} objects?"
  )
  bucket_checks(bucket)
  if (!aws_bucket_exists(bucket)) {
    cli_warning("bucket {.strong {bucket}} does not exist; exiting")
    return(invisible())
  }
  list_obs <- safely(aws_bucket_list_objects)
  objects <- list_obs(bucket)
  if (is_empty(objects$result)) {
    cli_info("bucket {.strong {bucket}} has no objects")
    if (!force) {
      if (yesno(msg_no_objects)) {
        return(invisible())
      }
    }
  } else {
    if (!force) {
      if (yesno(msg_with_objects)) {
        return(invisible())
      }
    }
    map(objects$result$uri, \(x) aws_file_delete(x))

    # check for empty folders & delete thoes too
    empties <- list_obs(bucket)
    if (!is_empty(empties$result)) {
      map(empties$result$uri, \(x) aws_file_delete(x))
    }
  }
  cli_info("deleting bucket {.strong {bucket}}")
  aws_bucket_delete(bucket, force = TRUE, ...)
  invisible()
}

#' Download an S3 bucket
#'
#' @export
#' @param bucket (character) bucket name. required
#' @param dest_path (character) destination directory to store files. required
#' @param ... named parameters passed on to [s3fs::s3_dir_download()]
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @family buckets
#' @examplesIf interactive()
#' bucket <- random_string("bucket")
#' aws_bucket_create(bucket = bucket)
#' desc_file <- file.path(system.file(), "DESCRIPTION")
#' aws_file_upload(desc_file, s3_path(bucket, "DESCRIPTION.txt"))
#' aws_file_upload(desc_file, s3_path(bucket, "d_file.txt"))
#' temp_dir <- file.path(tempdir(), bucket)
#' aws_bucket_download(bucket = bucket, dest_path = temp_dir)
#' fs::dir_ls(temp_dir)
#'
#' # cleanup
#' aws_bucket_delete("tmp-bucket-369")
aws_bucket_download <- function(bucket, dest_path, ...) {
  con_s3fs()$dir_download(path = bucket, new_path = dest_path, ...)
}

#' Upload a folder of files to create an S3 bucket
#'
#' @export
#' @importFrom fs fs_bytes
#' @param path (character) local path to a directory. required
#' @param max_batch (fs_bytes) maximum batch size being uploaded with each
#' multipart
#' @param ... named parameters passed on to [s3fs::s3_dir_upload()]
#' @inheritParams aws_bucket_delete
#' @note Requires the env var `AWS_REGION`. This function prompts you to make
#' sure that you want to delete the bucket.
#' @family buckets
#' @details To upload individual files see [aws_file_upload()]
#' @return the s3 format path of the bucket uploaded to
#' @examplesIf interactive()
#' library(fs)
#' tdir <- path(tempdir(), "apples")
#' dir.create(tdir)
#' tfiles <- replicate(n = 10, file_temp(tmp_dir = tdir, ext = ".txt"))
#' invisible(lapply(tfiles, function(x) write.csv(mtcars, x)))
#'
#' bucket_name <- random_string("bucket")
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' aws_bucket_upload(path = tdir, bucket = bucket_name)
#' aws_bucket_list_objects(bucket_name)
#'
#' # cleanup
#' objs <- aws_bucket_list_objects(bucket_name)
#' aws_file_delete(objs$uri)
#' aws_bucket_list_objects(bucket_name)
#' aws_bucket_delete(bucket_name, force = TRUE)
#' aws_bucket_exists(bucket_name)
aws_bucket_upload <- function(
    path, bucket, max_batch = fs::fs_bytes("100MB"), force = FALSE,
    ...) {
  stop_if(rlang::is_missing(path), "{.strong path} is required")
  stop_if(rlang::is_missing(bucket), "{.strong bucket} is required")
  if (!aws_bucket_exists(bucket)) {
    if (!force) {
      if (yesno("{.strong {bucket}} does not exist. Create it?")) {
        cli::cli_inform("Exiting without uploading {.strong {basename(path)}}")
        return(invisible())
      }
    }
    aws_bucket_create(bucket)
  }
  con_s3fs()$dir_upload(
    path = path,
    new_path = bucket,
    max_batch = max_batch,
    ...
  )
  s3_path(bucket)
}

bucket_name <- function(x) {
  first(fs::path_split(first(x))[[1]])
}

#' @importFrom fs path_split
#' @importFrom purrr map_vec
remote_has_keys <- function(x) {
  paths <- fs::path_split(x)
  all(map_vec(paths, \(p) length(p) > 1))
}

#' @importFrom fs is_dir dir_ls
#' @importFrom purrr list_c
explode_file_paths <- function(path) {
  if (any(fs::is_dir(path))) {
    path <- map(path, \(p) {
      if (fs::is_dir(p)) {
        fs::dir_ls(p)
      } else {
        p
      }
    }) %>%
      list_c()
  }
  return(unname(unclass(path)))
}

#' Magically upload a mix of files and directories into a bucket
#'
#' @importFrom purrr map2
#' @export
#' @param path (character) one or more file paths to add to
#' the `bucket`. required. can include directories or files
#' @param remote (character) a vector of paths to use to upload
#' files in `path`. the bucket name is determined from this param.
#' if the vector passed in is length>1 then the base path component
#' must all be the same. required
#' @inheritParams aws_file_copy
#' @param ... named params passed on to
#' [put_object](https://www.paws-r-sdk.com/docs/s3_put_object/)
#' @section What is magical:
#' - Exits early if folder or files do not exist
#' - Creates the bucket if it does not exist
#' - Adds files to the bucket, figuring out the key to use from
#' the supplied path
#' - Function is vectoried for the `path` argument; you can
#' pass in many paths
#' @family buckets
#' @family magicians
#' @return (character) a vector of remote s3 paths where your
#' files are located
#' @examplesIf interactive()
#' # single file, single remote path
#' bucket <- random_string("bucket")
#' demo_rds_file <- file.path(system.file(), "Meta/demo.rds")
#' six_bucket_upload(path = demo_rds_file, remote = bucket)
#'
#' ## a file and a directory - with a single remote path
#' bucket <- random_string("bucket")
#' library(fs)
#' tdir <- path(path_temp(), "mytmp")
#' dir_create(tdir)
#' purrr::map(letters, \(l) file_create(path(tdir, l)))
#' six_bucket_upload(path = c(demo_rds_file, tdir), remote = bucket)
#'
#' ## two files - two values passed to remote path
#' bucket <- random_string("bucket")
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' six_bucket_upload(path = c(demo_rds_file, links_file),
#'  remote = path(bucket, c("afile.txt", "anotherfile.txt")))
six_bucket_upload <- function(path, remote, force = FALSE, ...) {
  stop_if_not(is_character(path), "{.strong path} must be character")
  stop_if_not(is_character(remote), "{.strong remote} must be character")

  path <- explode_file_paths(path)
  stop_if_not(
    all(fs::file_exists(path)),
    "one or more of {.strong path} don't exist"
  )

  if (length(remote) > 1) {
    if (length(unique(fs::path_dir(remote))) != 1) {
      cli_abort(paste0(c(
        " if {.strong remote} length > 1,",
        "first part of each path must be the same"
      ), collapse = " "))
    }
  }

  bucket <- bucket_name(remote)
  bucket_create_if_not(bucket, force)
  if (!aws_bucket_exists(bucket)) {
    cli_warning("bucket {.strong {bucket}} not created; exiting")
    return(invisible())
  }

  if (length(remote) == 1) {
    remote_paths <- remote
    if (length(path) > 1) {
      cli_info("recycling {.strong remote} for each {.strong path}")
      remote_paths <- rep(as.character(remote), times = length(path))
    }
  } else if (length(remote) > 1) {
    if (length(path) != length(remote)) {
      cli_abort(paste0(c("if {.strong length(remote) > 1} then ",
        "{.strong length(path)} == {.strong length(remote)}"), collapse = " "))
    } else {
      remote_paths <- remote
    }
  }

  use_remotes_for_keys <- FALSE
  if (remote_has_keys(remote_paths)) {
    use_remotes_for_keys <- TRUE
  }

  map2(path, remote_paths, \(pth, rem) {
    con_s3()$put_object(
      Bucket = bucket,
      Key = ifelse(use_remotes_for_keys, basename(rem), basename(pth)),
      Body = pth,
      ...
    )
  })
  s3_path(bucket, basename(path))
}

#' List objects in an S3 bucket
#'
#' @export
#' @autoglobal
#' @param bucket (character) bucket name. required
#' @param ... named parameters passed on to
#' [list_objects](https://www.paws-r-sdk.com/docs/s3_list_objects/)
#' @family buckets
#' @return if no objects found, an empty tibble. if tibble has rows each
#' is an S3 bucket, with 8 columns:
#' * bucket_name (character)
#' * key (character)
#' * uri (character)
#' * size (fs::bytes)
#' * type (character)
#' * owner (character)
#' * etag (character)
#' * last_modified (dttm)
#' @examplesIf interactive()
#' bucket_name <- random_string("bucket")
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' aws_file_upload(
#'   links_file,
#'   s3_path(bucket_name, basename(links_file))
#' )
#' aws_bucket_list_objects(bucket = bucket_name)
aws_bucket_list_objects <- function(bucket, ...) {
  out <- con_s3()$list_objects(bucket, ...)
  if (rlang::is_empty(out$Contents)) {
    return(tibble())
  }
  as_tibble(jsonlite::fromJSON(
    jsonlite::toJSON(out$Contents, auto_unbox = TRUE),
    flatten = TRUE
  )) %>%
    mutate(
      bucket = bucket,
      uri = glue("s3://{bucket}/{Key}"),
      Size = fs::as_fs_bytes(Size),
      LastModified = as_datetime(LastModified)
    ) %>%
    rowwise() %>%
    mutate(
      type = if (endsWith(Key, "/")) "directory" else "file"
    ) %>%
    ungroup() %>%
    rename_with(tolower) %>%
    select(bucket, key, uri, size, type, etag, lastmodified, storageclass)
}

#' List S3 buckets
#'
#' @export
#' @inherit aws_bucket_list_objects
#' @details internally uses `s3fs::s3_dir_info()`
#' @note we set `refresh=TRUE` internally to make sure we return up to date
#' information about your buckets rather than what's cached locally
#' @family buckets
#' @examples \dontrun{
#' aws_buckets()
#' }
aws_buckets <- function(...) {
  out <- con_s3fs()$dir_info(refresh = TRUE, ...)
  if (is.data.frame(out) && NROW(out) > 0) {
    as_tibble(out)
  } else {
    tibble()
  }
}

#' Print a tree of the objects in a bucket
#'
#' @export
#' @inheritParams aws_bucket_exists
#' @param recurse (logical) returns all AWS S3 objects in lower sub
#' directories, default: `TRUE`
#' @param ... Additional arguments passed to `s3fs::s3_dir_tree()`
#' @family buckets
#' @return character vector of objects/files within the bucket,
#' printed as a tree
#' @examplesIf interactive()
#' bucket_name <- random_string("bucket")
#' if (!aws_bucket_exists(bucket_name)) aws_bucket_create(bucket_name)
#' links_file <- file.path(system.file(), "Meta/links.rds")
#' pkgs_file <- file.path(system.file(), "Meta/package.rds")
#' demo_file <- file.path(system.file(), "Meta/demo.rds")
#' aws_file_upload(
#'   c(links_file, pkgs_file, demo_file),
#'   s3_path(
#'     bucket_name,
#'     c(
#'       basename(links_file),
#'       basename(pkgs_file),
#'       basename(demo_file)
#'     )
#'   )
#' )
#' aws_bucket_tree(bucket_name)
#'
#' # cleanup
#' objs <- aws_bucket_list_objects(bucket_name)
#' aws_file_delete(objs$uri)
#' aws_bucket_delete(bucket_name, force = TRUE)
#' aws_bucket_exists(bucket_name)
aws_bucket_tree <- function(bucket, recurse = TRUE, ...) {
  con_s3fs()$dir_tree(s3_path(bucket), recurse = recurse, ...)
}
