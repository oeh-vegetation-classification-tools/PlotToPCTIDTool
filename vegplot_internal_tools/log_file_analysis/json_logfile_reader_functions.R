remove_trailing_slashes <- function(x) gsub("/*$", "", x)

download_folder <- function (path,
                             local_path,
                             dtoken = rdrop2::drop_auth(),
                             unzip = TRUE,
                             overwrite = FALSE,
                             progress = interactive(),
                             verbose = interactive()) {
  if (unzip && dir.exists(local_path))
    stop("a directory already exists at ", local_path)
  if (!unzip && file.exists(local_path))
    stop("a file already exists at ", local_path)
  
  path <- remove_trailing_slashes(path)
  local_path <- remove_trailing_slashes(local_path)
  local_parent <- dirname(local_path)
  original_dir_name <- basename(path)
  download_path <- if (unzip) tempfile("dir") else local_path
  
  if (!dir.exists(local_parent)) stop("target parent directory ", local_parent, " not found")
  
  url <- "https://content.dropboxapi.com/2/files/download_zip"
  req <- httr::POST(
    url = url,
    httr::config(token = dtoken),
    httr::add_headers(
      `Dropbox-API-Arg` = jsonlite::toJSON(list(path = paste0("/", path)),
                                           auto_unbox = TRUE)),
    if (progress) httr::progress(),
    httr::write_disk(download_path, overwrite)
  )
  httr::stop_for_status(req)
  if (verbose) {
    size <- file.size(download_path)
    class(size) <- "object_size"
    message(sprintf("Downloaded %s to %s: %s on disk", path,
                    download_path, format(size, units = "auto")))
  }
  if (unzip) {
    if (verbose) message("Unzipping file...")
    new_dir_name <- basename(local_path)
    unzip_path <- tempfile("dir")
    unzip(download_path, exdir = unzip_path)
    file.rename(file.path(unzip_path, original_dir_name),
                file.path(unzip_path, new_dir_name))
    file.copy(file.path(unzip_path, new_dir_name),
              local_parent,
              recursive = TRUE)
  }
  
  TRUE
}