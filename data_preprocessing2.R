# Script to add common names & IUCN URL to host dataset


library(taxize)

mammals = read.csv("distinct-hosts.csv")
mammals$commonName <- lapply(mammals$HostCorrectedName, FUN = function(x) unlist(sci2comm(x)[[1]]))
mammals$commonName <- unlist(as.character(x[["commonName"]]))

# Fix the four that couldn't be identified
mammals$commonName[mammals$HostCorrectedName=="Arctocephalus pusillus"] <- "brown fur seal"
mammals$commonName[mammals$HostCorrectedName=="Caracal caracal"] <- "caracal"
mammals$commonName[mammals$HostCorrectedName=="Equus burchellii"] <- "plains zebra"
mammals$commonName[mammals$HostCorrectedName=="Damaliscus pygargus"] <- "bontebok"

mammals$status <- factor(mammals$type, levels = c("NonThreatened", "Disease", "ThreatOther", "ThreatDisease"), labels = c("NT - Other", "NT - Disease", "T - Other", "T - Disease"))

mammals$IUCNlink <- NA
for (i in 1:nrow(mammals)) {
  url <- paste0("https://www.iucnredlist.org/search?query=", unlist(strsplit(as.character(mammals[i, "HostCorrectedName"]), split= " ")[[1]][1]),"%20",unlist(strsplit(as.character(mammals[i, "HostCorrectedName"]), split= " ")[[1]][2]),"&searchType=species")
  mammals[i, "IUCNlink"] <- paste0("<a href='", url,"'>", url,"</a>")
  
}

mammals %<>% mutate(
  poptext = paste0("Scientific Name:  ", HostCorrectedName, "</br>Common Name:  ", commonName, "</br>Threat Status:  ", status, "</br>Country:  ", country))
write.csv(mammals, "distinct-hosts-mod.csv", row.names = FALSE)

