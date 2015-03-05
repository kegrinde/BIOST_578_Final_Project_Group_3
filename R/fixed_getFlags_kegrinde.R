regions = regions.merged.y; exons =exons; chromosome = "chrY"; pctcut = 0.8

fixed_getFlags <- function (regions, exons, chromosome, pctcut = 0.8) {
  chr <- state <- NULL
  require(GenomicRanges)
  exons = subset(exons, seqnames == chromosome)
  exons = exons[order(exons$start), ]
  regions = subset(regions, chr == substr(chromosome,4,4))
  stopifnot(length(unique(exons$seqnames)) == 1, length(unique(regions$chr)) == 
              1, unique(exons$seqnames) == paste('chr',unique(regions$chr),sep=''))
  exgr = GRanges(seqnames = Rle(exons$chr), ranges = IRanges(start = exons$start, 
                                                             end = exons$end))
  candidates = subset(regions, state == 3 | state == 4)
  regionsgr = GRanges(seqnames = Rle(candidates$chr), ranges = IRanges(start = candidates$start, 
                                                                       end = candidates$end))
  overlaps = findOverlaps(regionsgr, exgr)
  ex_by_region = split(subjectHits(overlaps), queryHits(overlaps))
  annotate_name = function(i) {
    if (i %in% names(ex_by_region)) {
      inds = ex_by_region[[which(names(ex_by_region) == 
                                   i)]]
      return(exons$exon_id[inds])
    }
    else {
      return(NA)
    }
  }
  annotate_pct = function(i) {
    if (i %in% names(ex_by_region)) {
      inds = ex_by_region[[which(names(ex_by_region) == 
                                   i)]]
      pcts = rep(NA, length(inds))
      rpos = c(candidates$start[i]:candidates$end[i])
      for (exi in 1:length(inds)) {
        expos = c(exons$start[inds[exi]]:exons$end[inds[exi]])
        pcts[exi] = length(intersect(rpos, expos))/length(expos)
      }
      return(pcts)
    }
    else {
      return(NA)
    }
  }
  flag.info = lapply(1:nrow(candidates), annotate_name)
  percent.exon = lapply(1:nrow(candidates), annotate_pct)
  flags = rep(NA, nrow(candidates))
  flags[is.na(flag.info)] = "novel"
  has_enough_ol = function(i) {
    if (!is.na(percent.exon[[i]][1])) {
      return(any(percent.exon[[i]] > pctcut))
    }
    else {
      return(FALSE)
    }
  }
  potential_de = sapply(1:nrow(candidates), has_enough_ol, 
                        USE.NAMES = FALSE)
  flags[potential_de] = "DE exon(s)"
  return(list(flags = flags, flag.info = flag.info, percent.exon = percent.exon))
}