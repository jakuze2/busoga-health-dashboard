# Protect a PDF against editing: standard PDF security (AES-128, PDF 1.6). Anyone can open and
# print the file; changing, copying text or extracting pages needs the owner password.
# Written for the simple PDFs made by grDevices::pdf() (plain objects, direct stream lengths,
# no object streams), which is how the briefs are drawn. Needs only the openssl package.

pdf_protect <- function(file, owner_password) {
  stopifnot(nzchar(owner_password))
  PAD <- as.raw(c(0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41, 0x64, 0x00, 0x4E, 0x56, 0xFF, 0xFA, 0x01, 0x08,
                  0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68, 0x3E, 0x80, 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A))
  md5 <- function(x) as.raw(openssl::md5(x))
  pad32 <- function(pw) { b <- charToRaw(enc2utf8(pw)); b <- b[seq_len(min(32, length(b)))]; c(b, PAD)[1:32] }
  rc4 <- function(key, data) {
    S <- 0:255; k <- as.integer(key); j <- 0L
    for (i in 0:255) { j <- (j + S[i + 1] + k[i %% length(k) + 1]) %% 256L; t <- S[i + 1]; S[i + 1] <- S[j + 1]; S[j + 1] <- t }
    d <- as.integer(data); out <- integer(length(d)); i <- 0L; j <- 0L
    for (n in seq_along(d)) { i <- (i + 1L) %% 256L; j <- (j + S[i + 1]) %% 256L
      t <- S[i + 1]; S[i + 1] <- S[j + 1]; S[j + 1] <- t
      out[n] <- bitwXor(d[n], S[(S[i + 1] + S[j + 1]) %% 256L + 1]) }
    as.raw(out)
  }
  le32 <- function(v) { v <- as.numeric(v); if (v < 0) v <- v + 2^32; as.raw(c(v %% 256, (v %/% 256) %% 256, (v %/% 65536) %% 256, (v %/% 16777216) %% 256)) }
  hex <- function(r) paste0("<", paste(format(as.hexmode(as.integer(r)), width = 2), collapse = ""), ">")

  # permissions: print (bits 3 and 12) only; reserved bits 7-8 and 13-32 set
  P <- -1852
  id <- md5(charToRaw(paste(file, Sys.time(), runif(1))))
  # owner entry (algorithm 3), file key (algorithm 2), user entry (algorithm 5), revision 4
  ok <- md5(pad32(owner_password)); for (i in 1:50) ok <- md5(ok[1:16]); ok <- ok[1:16]
  O <- rc4(ok, pad32("")); for (i in 1:19) O <- rc4(as.raw(bitwXor(as.integer(ok), i)), O)
  fk <- md5(c(pad32(""), O, le32(P), id)); for (i in 1:50) fk <- md5(fk[1:16]); fk <- fk[1:16]
  U <- rc4(fk, md5(c(PAD, id))); for (i in 1:19) U <- rc4(as.raw(bitwXor(as.integer(fk), i)), U); U <- c(U, PAD[1:16])
  enc <- function(num, data) {
    key <- md5(c(fk, as.raw(c(num %% 256, (num %/% 256) %% 256, (num %/% 65536) %% 256)), as.raw(c(0, 0)), charToRaw("sAlT")))[1:16]
    iv <- openssl::rand_bytes(16)
    c(iv, openssl::aes_cbc_encrypt(data, key = key, iv = iv))
  }

  raw <- readBin(file, "raw", file.size(file))
  s <- raw; s[s == as.raw(0)] <- as.raw(1)
  str <- rawToChar(s); Encoding(str) <- "bytes"
  m <- gregexpr("(?m)^(\\d+) 0 obj", str, perl = TRUE, useBytes = TRUE)[[1]]
  if (m[1] < 0) stop("no PDF objects found")
  nums <- as.integer(sub(" 0 obj", "", regmatches(str, list(m))[[1]], useBytes = TRUE))
  root <- as.integer(sub(".*/Root (\\d+) 0 R.*", "\\1", sub("(?s).*trailer", "", str, perl = TRUE, useBytes = TRUE), useBytes = TRUE))
  info <- suppressWarnings(as.integer(sub(".*/Info (\\d+) 0 R.*", "\\1", sub("(?s).*trailer", "", str, perl = TRUE, useBytes = TRUE), useBytes = TRUE)))
  out <- list(charToRaw("%PDF-1.6\n%\xE2\xE3\xCF\xD3\n")); offsets <- integer(); pos <- length(out[[1]])
  add <- function(num, bytes) { offsets[as.character(num)] <<- pos; out[[length(out) + 1]] <<- bytes; pos <<- pos + length(bytes) }
  for (k in seq_along(m)) {
    num <- nums[k]; st <- m[k] + attr(m, "match.length")[k]            # first byte after "N 0 obj"
    rest <- substr(str, st, nchar(str, type = "bytes"))
    if (!is.na(info) && num == info) { add(num, charToRaw(sprintf("%d 0 obj\n<< >>\nendobj\n", num))); next }
    sm <- regexpr("stream(\r\n|\n)", rest, useBytes = TRUE)
    em <- regexpr("endobj", rest, fixed = TRUE, useBytes = TRUE)
    if (sm > 0 && sm < em) {
      dict <- substr(rest, 1, sm - 1)
      len <- as.integer(sub("(?s).*/Length (\\d+).*", "\\1", dict, perl = TRUE, useBytes = TRUE))
      if (is.na(len) || grepl("/Length \\d+ \\d+ R", dict, useBytes = TRUE)) stop("indirect stream length not supported")
      d0 <- st + sm - 1 + attr(sm, "match.length")                    # 1-based position of the data in raw
      data <- raw[d0:(d0 + len - 1)]
      e <- enc(num, data)
      dict <- sub("/Length \\d+", paste0("/Length ", length(e)), dict, useBytes = TRUE)
      add(num, c(charToRaw(sprintf("%d 0 obj%s", num, dict)), charToRaw("stream\n"), e, charToRaw("\nendstream\nendobj\n")))
    } else {
      body <- substr(rest, 1, em - 1)
      if (grepl("(", body, fixed = TRUE)) stop("object ", num, " contains a string; not supported")
      add(num, charToRaw(sprintf("%d 0 obj%sendobj\n", num, body)))
    }
  }
  encnum <- max(nums) + 1L
  add(encnum, charToRaw(sprintf(paste0("%d 0 obj\n<< /Filter /Standard /V 4 /R 4 /Length 128 /CF << /StdCF << /AuthEvent /DocOpen /CFM /AESV2 /Length 16 >> >> ",
                                       "/StmF /StdCF /StrF /StdCF /O %s /U %s /P %d /EncryptMetadata true >>\nendobj\n"), encnum, hex(O), hex(U), P)))
  size <- encnum + 1L
  xref <- c("xref", sprintf("0 %d", size), "0000000000 65535 f ")
  for (n in seq_len(size - 1)) { o <- offsets[as.character(n)]
    xref <- c(xref, if (is.na(o)) "0000000000 65535 f " else sprintf("%010d 00000 n ", as.integer(o))) }
  tail <- c(xref, "trailer", sprintf("<< /Size %d /Root %d 0 R /Encrypt %d 0 R /ID [%s %s] >>", size, root, encnum, hex(id), hex(id)),
            "startxref", as.character(pos), "%%EOF", "")
  writeBin(c(unlist(out), charToRaw(paste(tail, collapse = "\n"))), file)
  invisible(file)
}
