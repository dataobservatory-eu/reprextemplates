library(magick)

# dated.jpg       known EXIF/camera/time metadata
# fake.jpg        misleading extension / content detection
# gps.jpg         structured geographic metadata
# minimal.jpg     structural metadata baseline
# observable.png  different format + textual metadata
# unicode.jpg     XMP/IPTC + Unicode/encoding

fixture_dir <- "tests/testthat/fixtures/embedded"
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

dated_file <- file.path(fixture_dir, "dated.jpg")

# Create a tiny synthetic JPEG
image_blank(
  width = 50,
  height = 50,
  color = "white"
) |>
  image_write(
    path = dated_file,
    format = "jpeg",
    quality = 90
  )


args <- c(
  "-overwrite_original",
  shQuote("-Make=Fixture Camera"),
  shQuote("-Model=Fixture Model"),
  shQuote("-DateTimeOriginal=2026:07:10 14:32:15"),
  shQuote("-CreateDate=2026:07:10 14:32:16"),
  shQuote("-ModifyDate=2026:07:10 14:32:17"),
  shQuote(dated_file)
)

system2("exiftool", args)


png_file <- file.path(
  "tests", "testthat", "fixtures", "embedded", "observable.png"
)

magick::image_blank(
  width = 50,
  height = 50,
  color = "white"
) |>
  magick::image_write(
    path = png_file,
    format = "png"
  )


args <- c(
  "-overwrite_original",
  shQuote("-Title=Observable PNG fixture"),
  shQuote("-Description=Synthetic embedded metadata test"),
  shQuote("-Author=Reprex"),
  shQuote("-CreationTime=2026:07:11 10:20:30"),
  shQuote(png_file)
)

system2("exiftool", args)


minimal_jpg <- file.path(fixture_dir, "minimal.jpg")

magick::image_blank(
  width = 50,
  height = 50,
  color = "white"
) |>
  magick::image_write(
    path = minimal_jpg,
    format = "jpeg",
    quality = 90
  )


gps_file <- file.path(fixture_dir, "gps.jpg")

magick::image_blank(
  width = 50,
  height = 50,
  color = "white"
) |>
  magick::image_write(gps_file, format = "jpeg")

args <- c(
  "-overwrite_original",
  "-GPSLatitude=47.4979",
  "-GPSLatitudeRef=N",
  "-GPSLongitude=19.0402",
  "-GPSLongitudeRef=E",
  shQuote(gps_file)
)

system2("exiftool", args)

fake_jpg <- file.path(fixture_dir, "fake.jpg")

writeLines(
  "This is deliberately not a JPEG.",
  fake_jpg,
  useBytes = TRUE
)


unicode_file <- file.path(fixture_dir, "unicode.jpg")
args_file <- tempfile(fileext = ".args")

magick::image_blank(
  width = 50,
  height = 50,
  color = "white"
) |>
  magick::image_write(unicode_file, format = "jpeg")

text <- paste(
  "-overwrite_original",
  "-charset",
  "exiftool=UTF8",
  "-UserComment=Árvíztűrő tükörfúrógép – Deliņi – Rīga – Tartu",
  "-execute",
  sep = "\n"
)
