# Lisa Poggel 
# 12.9.2022
# Extract code co-occurrence data from REFi-QDA xml for import into R shiny app 


library(xml2)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)

# FUNCTION DEFINITIONS 

# Recursively extract nodelist data from each node on one level of a nested refi-qda xml document. 
# First calls xml_find_all() to retrieve a nodeset of all "Code" child nodes of 
# the last node (index m) in the input nodeset (=subcategory of the input category). 
# Then calls xml_attr() to retrieve GUIDs and names of each subcategory from the 
# child node attributes and stores the data in two vectors, temp_guids and temp_labels. 
# Then creates a third vector temp_groups, which contains the name attribute of the parent node. 
# The function then calls itself and the three vectors are passed on as new input. 
# In the second iteration, the data for the second to last node (index m-1) in the 
# input nodeset is extracted and passed on. When m==0, the recursively concatenated
# vectors are combined into a dataframe, which is returned. 
get_nodelist_data <- function(category, guids=c(), labels=c(), groups=c(), m=length(category)){ # takes a category nodeset as input
  if(m == 0) { # recursion anchor
    nodelist <- data.frame(guids, labels, groups)
    colnames(nodelist) <- c("id", "label", "group")
    return(nodelist)
  } else { # m > 0
    # get nodesets of subcategories
    subcategories <- xml_find_all(category[m], "Code") 
    # get guid attributes of subcategories
    temp_guids <- xml_attr(subcategories, "guid")
    # get name attributes of subcategories
    temp_labels <- xml_attr(subcategories, "name")
    # get name of parent node: we need this to filter nodes later
    temp_groups <- rep(xml_attr(category[m], "name"), length(temp_labels))
    # repeat for category[m-1] until lowest nested level
    nodelist <- get_nodelist_data(category, guids=c(guids, temp_guids), labels=c(labels, temp_labels), groups=c(groups, temp_groups),  m=m-1) 
  }
}

# Recursively bind extracted nodelist data for every level of nested refi-qda xml to a dataframe.
# If the input nodeset is not empty, create_nodelist() first calls the function 
# get_nodelist_data() with the input category as input. It then calls itself, but
# this time with the output from xml_find_all(category, "Code") as input. xml_find_all()
# returns a nodeset of all "Code" child nodes of the input category. This nodeset
# is passed onto the get_nodelist_data() function. Create_nodelist thus applies 
# the function get_nodelist_data() to every level of the nested xml data.  
# rbind() recursively binds output to a single dataframe, which is returned. 
create_nodelist <- function(category, m=length(category)) {
  if(m==0) { #recursion anchor
    return(data.frame()) 
  } else {
    return(rbind(get_nodelist_data(category), create_nodelist(xml_find_all(category, "Code"))))
  }
}

# Create a list of paths to each PlainTextSelection that contains a CodeRef with 
# the targetGUIDs matching the guids from the category selected. The targetGUID 
# attribute in each CodeRef element of each PlainTextSelection is the guid of the 
# code applied. Append selection_paths with path to all subcategory guids of the 
# selected category and return the final list of segments (= plaintext selections 
# with same start position) containing subcategories from the selected main category
filter_selections <- function(catguids) {
  guid_paths <- map(selection_paths, paste0, "/Coding/CodeRef[@targetGUID='", catguids, "']")
  guid_paths <- unlist(guid_paths, recursive=TRUE)
  # get a list of paths to each PlainTextSelection that contains a CodeRef with the 
  # targetGUIDs matching the guids from the category we selected
  selections_filtered <- mapply(function(input, paths) {
    xml_path(xml_parent(xml_parent(xml_find_all(input, paths)))) 
  }, input=sources, paths=guid_paths)
  selections_filtered <- unlist(selections_filtered, recursive=TRUE)
  return(selections_filtered)
}
 
find_startpos <- function(input, paths) { # takes a nodeset of text sources and a list of plaintext selections as input
  xml_attr((xml_find_all(input, paths)), "startPosition")
}

find_issue <- function(input, paths) { # takes a nodeset of text sources and a list of plaintext selections as input
  xml_attr((xml_find_all(input, dirname(paths))), "name")
}

# Compare both input lists: compare startPosition attribute of each PlainTextSelection. 
# This should be more accurate than the attribute name, which contains also endPosition. 
# If startPositions are the same: add both targetGUIDs to dataframe columns "source"
# and "target" and return a dataframe of matching plaintext selections. 
match_selections <- function(x_selections, x_startpos, y_startpos, x_issues, y_issues) { #, x_groups
  # get indexes of elements in x_startpos that are also in y_startpos (same segment)
  startpos_matches <- which(x_startpos %in% y_startpos)
  # subset x_selections vector with startpos_matches indices and append path to each plaintext selection
  # Because the startpos_matches vector is indexed the same as the x_selections 
  # vector, the startpos_matches indices can be used to subset this vector.
  x_matches_paths <- paste0(x_selections[startpos_matches], "/Coding/CodeRef")
  # get targetGUIDs for each of the matched plaintext selections and store in a 
  # new vector "matches_guids", which, depending on the order of input vectors,
  # will be the source or target column of our edgelist
  matches_guids <- mapply(function(input, paths) {
    xml_attr((xml_find_all(input, paths)), "targetGUID")
  }, input=sources, paths=x_matches_paths)
  matches_df <- data.frame(matches_guids, "startpos"=x_startpos[startpos_matches], "issue"=x_issues[startpos_matches])
  return(matches_df)
}

# Create four vectors containing start positions and the issue number corresponding 
# to each plaintext selection in the two input vectors x_selections and y_selections. 
# The function match_selections() takes the two input vectors and the four vectors 
# containing corresponding start positions and issue numbers as input. 
create_edgelist <- function(x_selections, y_selections) { # takes two lists of plaintext selections as input
  # get vector of start positions of all plaintext selections from the two input categories
  x_startpos <- mapply(find_startpos, input=sources, paths=x_selections)
  y_startpos <- mapply(find_startpos, input=sources, paths=y_selections)
  # get vector of journal issues of all plaintext selections from the two input categories
  x_issues <- mapply(find_issue, input=sources, paths=x_selections)
  y_issues <- mapply(find_issue, input=sources, paths=y_selections)
  # match selections: note that resulting dfs may have unequal length if multiple 
  # codes were applied from one category in the same segment
  x_df <- match_selections(x_selections, x_startpos, y_startpos, x_issues, y_issues) 
  y_df <- match_selections(y_selections, y_startpos, x_startpos, y_issues, x_issues)
  # join into edgelist
  edgelist <- data.frame()
  edgelist <- merge(x=x_df, y=y_df, by=c("startpos","issue"))
  edgelist <- edgelist[, c(3,4,1,2)] # reorder columns
  # rename columns
  colnames(edgelist)[1] <- "source" 
  colnames(edgelist)[2] <- "target"
  return(edgelist)
}


# Replace ids with names in edgelists and return named edgelist
# Automatically adds group column to final dataframe
prettify_edgelist <- function(x_y_edgelist) {
  # get nodelists for both input categories
  searchstr <- deparse(substitute(x_y_edgelist)) # make variable name string
  x <- str_match_all(searchstr, "[^_]*")[[1]][1,1] # access first match
  y <- str_match_all(searchstr, "[^_]*")[[1]][3,1] # access second match
  x_nodelist <- eval(parse(text=paste0(x, "_nodelist"))) # get nodelist for category x; eval(parse()) converts string to variable name
  y_nodelist <- eval(parse(text=paste0(y, "_nodelist"))) # get nodelist for category y
  # combine nodelists
  combined_nodelist <- rbind(x_nodelist, y_nodelist) 
  # replace ids of first input category with labels
  colnames(combined_nodelist)[1] <- "source"
  edgelist_names <- left_join(x_y_edgelist,  combined_nodelist, by = "source") # left join drops rows from combined_nodelist that do not match x_y_edgelist by column "source" 
  edgelist_names$source <- edgelist_names$label
  # replace ids of second input category with labels
  colnames(combined_nodelist)[1] <- "target"
  edgelist_target_names <- left_join(x_y_edgelist,  combined_nodelist, by = "target") 
  edgelist_names$target <- edgelist_target_names$label
  # clean up renamed edgelist
  edgelist_names$label <- NULL
  return(edgelist_names)
}

# Add edge attributes to edgelist by joining the prettified edgelists of tj,ntj, ag
# and tpi, tos, rel edges. Compare startpos of each row in edgelists of tj, ntj, ag 
# with startpos of tos_selections, tpi_selections, rel_selections, rrel_selections. 
# If startposmatches, add the matching category name as attribute to edgelist. 
add_attributes <- function(x_y_edgelist) {
  # get first category name as string: need this to get edgelists
  searchstr <- deparse(substitute(x_y_edgelist)) # make variable name string
  x <- str_match_all(searchstr, "[^_]*")[[1]][1,1] # access first match
  y <- str_match_all(searchstr, "[^_]*")[[1]][3,1] # access second match
  # add tempi column, then type of statement column, then relations column
  x_y_edgelist <- x_y_edgelist %>% left_join(eval(parse(text=paste0(x, "_tpi_edgelist"))), by=c("source", "startpos", "issue", "group")) %>%
    left_join(eval(parse(text=paste0(x, "_tos_edgelist"))), by=c("source", "startpos", "issue", "group"))
  # add relations column only for feature edgelist
  if(y == "fts"){
    x_y_edgelist <- left_join(x_y_edgelist, eval(parse(text=paste0(x, "_rel_edgelist"))), by=c("source", "startpos", "issue", "group"))
    colnames(x_y_edgelist)[8] <- "relation"
  } 
  # rename new columns
  colnames(x_y_edgelist)[2] <- "target"
  colnames(x_y_edgelist)[6] <- "tempi"
  colnames(x_y_edgelist)[7] <- "type_of_statement"
  return(x_y_edgelist)
}

# Function add_attributes() adapted for unipartite edgelists
add_unipartite_attributes <- function(x_y_z_edgelist) {
  # get first category name as string: need this to get edgelists
  searchstr <- deparse(substitute(x_y_z_edgelist)) # make variable name string
  x <- str_match_all(searchstr, "[^_]*")[[1]][1,1] # access first match
  y <- str_match_all(searchstr, "[^_]*")[[1]][3,1] # access second match
  z <- str_match_all(searchstr, "[^_]*")[[1]][5,1] # access third match
  # add tempi column, then type of statement column, then relations column
  x_y_z_edgelist <- x_y_z_edgelist %>% left_join(eval(parse(text=paste0(x, "_tpi_edgelist"))), by=c("source", "startpos", "issue", "group")) %>%
    left_join(eval(parse(text=paste0(x, "_tos_edgelist"))), by=c("source", "startpos", "issue", "group"))
  # rename new columns
  colnames(x_y_z_edgelist)[6] <- "tempi"
  colnames(x_y_z_edgelist)[7] <- "type_of_statement"
  # add edgelist attributes
  if(z == "fts"){
    # add additional relations column only for feature edgelist
    x_y_z_edgelist <- x_y_z_edgelist %>% left_join(eval(parse(text=paste0(x, "_fts_edgelist"))), by=c("source", "startpos", "issue", "group", "tempi", "type_of_statement"))  
    colnames(x_y_z_edgelist)[8] <- "feature"
    colnames(x_y_z_edgelist)[9] <- "relation" # dont need this? 
    # drop rows that have NA in feature column
    x_y_z_edgelist <- filter(x_y_z_edgelist, !is.na(feature))
  } 
  if(z == "ats"){
    # add additional feature column only for attribute edgelist
    x_y_z_edgelist <- x_y_z_edgelist %>% left_join(eval(parse(text=paste0(x, "_ats_edgelist"))), by=c("source", "startpos", "issue", "group", "tempi", "type_of_statement")) %>% 
      left_join(eval(parse(text=paste0(x, "_fts_edgelist"))), by=c("source", "startpos", "issue", "group", "tempi", "type_of_statement", "relation")) 
    x_y_z_edgelist$relation <- NULL 
    colnames(x_y_z_edgelist)[8] <- "attribute"
    colnames(x_y_z_edgelist)[9] <- "feature"
    # drop rows that have NA in attribute column
    x_y_z_edgelist <- filter(x_y_z_edgelist, !is.na(attribute))
  } 
  if(z == "nrs"){
    # add additional feature column only for attribute edgelist
    x_y_z_edgelist <- x_y_z_edgelist %>% left_join(eval(parse(text=paste0(x, "_nrs_edgelist"))), by=c("source", "startpos", "issue", "group", "tempi", "type_of_statement")) 
    colnames(x_y_z_edgelist)[8] <- "narrative"
    # drop rows that have NA in narrative column
    x_y_z_edgelist <- filter(x_y_z_edgelist, !is.na(narrative))
  }
  if(z == "sh" || z == "psh"){
    # add additional feature column only for attribute edgelist
    x_y_z_edgelist <- x_y_z_edgelist %>% left_join(eval(parse(text=paste0(x, "_", z, "_", "edgelist"))), by=c("source", "startpos", "issue", "group", "tempi", "type_of_statement")) 
    colnames(x_y_z_edgelist)[8] <- "event_or_period"
    # drop rows that have NA in event_or_period column
    x_y_z_edgelist <- filter(x_y_z_edgelist, !is.na(event_or_period))
  }
  # rename target column 
  colnames(x_y_z_edgelist)[2] <- "target"
  # add group info for target column and concatenate with group info for source column into new column "group"
  x_y_z_edgelist <- x_y_z_edgelist %>% left_join(eval(parse(text=paste0(x, "_", "nodelist"))), by=c("target"="label")) %>%
    unite("group", group.x, group.y, sep=", ")
  # drop id column
  x_y_z_edgelist$id <- NULL 
  return(x_y_z_edgelist)
}


# Clean unipartite edgelists: remove loops (=nodes matched with themselves) and 
# duplicates (=duplicate of previous edge with source and target reversed)
clean_unipartite <- function(edgelist){
  # remove loops
  edgelist <- edgelist[edgelist$source!=edgelist$target,]
  # order elements in source and target column alphabetically
  edgelist <- transform(edgelist, source = pmin(source, target), target=pmax(source, target))
  # remove duplicates
  edgelist <- distinct(edgelist) 
  return(edgelist)
}

# Write ouptut to edgelist or nodelist, using the input variable name as filename
write2csv <- function(edgelist) {
  write.csv2(edgelist, file=paste0(deparse(substitute(edgelist)), ".csv"), row.names=FALSE)
}


# GET NETWORK DATA

## Read QDE-XML project file
data <- read_xml("project.qde")
data%>%xml_ns_strip() # strip namespace for the xml to be parsable
sources <- xml_children(data)[3] # select Sources node
# List all xml-paths to each plaintext selection in each text source.
# Plaintext selections contain the codes applied to annotated text segments.
selection_paths <- xml_path(xml_find_all(sources, "//PlainTextSelection"))

## Make Nodelists
codes <- xml_children(data)[2]
main_codes <- xml_find_all(codes, "//Codes/Code") # get xml_nodeset of main categories
# Type of Statement nodelist (Function of statement left out)
tos <- main_codes[1] # get xml_nodeset of main category "Type of Statement"
tos_nodelist <- create_nodelist(tos)
# Narratives nodelist
nrs <- main_codes[4] # get xml_nodeset of main category "Narratives"
nrs_nodelist <- create_nodelist(nrs)
# Features nodelist: features are a subcategory of "definitions", defs_codes[1]
defs_codes <- xml_find_all(main_codes[9], "Code")
fts <- defs_codes[1]
fts_nodelist <- create_nodelist(fts)
# Attribute nodelist: attributes are a subcategory of "definitions", defs_codes[2]
ats <- defs_codes[2]
ats_nodelist <- create_nodelist(ats)
# Events, Periods in Soviet History nodelist
sh <- main_codes[5]
sh_nodelist <- create_nodelist(sh)
# Events, Periods in pre-Soviet History nodelist
psh <- main_codes[6]
psh_nodelist <- create_nodelist(psh)
# Tempi nodelist
tpi <- main_codes[7]
tpi_nodelist <- create_nodelist(tpi)
# Past-Present Relation nodelist
ppr <- main_codes[8]
ppr_nodelist <- create_nodelist(ppr)
# Groups,Locations in Tajikistan nodelist
tj <- main_codes[3]
tj_nodelist <- create_nodelist(tj)
# Groups,Locations outside Tajikistan nodelist
ntj <- main_codes[10]
ntj_nodelist <- create_nodelist(ntj)
# Type of Relation between groups nodelist 
rel <- main_codes[11]
rel_nodelist <- create_nodelist(rel)
# Reason for Relation between groups nodelist 
rrel <- main_codes[12]
rrel_nodelist <- create_nodelist(rrel)
# All groups combined nodelist 
ag_nodelist <- rbind(tj_nodelist, ntj_nodelist)
#ag_nodelist

## Make Edgelists

# Filter plaintext selections by the categories that the user will be able to select as input
nrs_selections <- filter_selections(nrs_nodelist$id) # important: extract column as vector, not list
fts_selections <- filter_selections(fts_nodelist$id)
ats_selections <- filter_selections(ats_nodelist$id)
sh_selections <- filter_selections(sh_nodelist$id)
psh_selections <- filter_selections(psh_nodelist$id)
tj_selections <- filter_selections(tj_nodelist$id)
ntj_selections <- filter_selections(ntj_nodelist$id)
tos_selections <- filter_selections(tos_nodelist$id)
tpi_selections <- filter_selections(tpi_nodelist$id)
ppr_selections <- filter_selections(ppr_nodelist$id)
rel_selections <- filter_selections(rel_nodelist$id)
rrel_selections <- filter_selections(rrel_nodelist$id)
ag_selections <- c(tj_selections, ntj_selections) # all groups and locations combined

# First create edgelists that will be used to add attributes "tempi", 
# "type of statement", and "relation" to edgelists below
tj_tpi_edgelist <- create_edgelist(tj_selections, tpi_selections) 
tj_tpi_edgelist <- prettify_edgelist(tj_tpi_edgelist)
ntj_tpi_edgelist <- create_edgelist(ntj_selections, tpi_selections) 
ntj_tpi_edgelist <- prettify_edgelist(ntj_tpi_edgelist)
ag_tpi_edgelist <- create_edgelist(ag_selections, tpi_selections) 
ag_tpi_edgelist <- prettify_edgelist(ag_tpi_edgelist)
tj_tos_edgelist <- create_edgelist(tj_selections, tos_selections) 
tj_tos_edgelist <- prettify_edgelist(tj_tos_edgelist)
ntj_tos_edgelist <- create_edgelist(ntj_selections, tos_selections) 
ntj_tos_edgelist <- prettify_edgelist(ntj_tos_edgelist)
ag_tos_edgelist <- create_edgelist(ag_selections, tos_selections)
ag_tos_edgelist <- prettify_edgelist(ag_tos_edgelist)
tj_rel_edgelist <- create_edgelist(tj_selections, rel_selections)
tj_rel_edgelist <- prettify_edgelist(tj_rel_edgelist)
ntj_rel_edgelist <- create_edgelist(ntj_selections, rel_selections) 
ntj_rel_edgelist <- prettify_edgelist(ntj_rel_edgelist)
ag_rel_edgelist <- create_edgelist(ag_selections, rel_selections) 
ag_rel_edgelist <- prettify_edgelist(ag_rel_edgelist)
fts_tpi_edgelist <- create_edgelist(fts_selections, tpi_selections) 
fts_tpi_edgelist <- prettify_edgelist(fts_tpi_edgelist)
fts_tos_edgelist <- create_edgelist(fts_selections, tos_selections) 
fts_tos_edgelist <- prettify_edgelist(fts_tos_edgelist)
ats_tpi_edgelist <- create_edgelist(ats_selections, tpi_selections) 
ats_tpi_edgelist <- prettify_edgelist(ats_tpi_edgelist)
ats_tos_edgelist <- create_edgelist(ats_selections, tos_selections) 
ats_tos_edgelist <- prettify_edgelist(ats_tos_edgelist)
nrs_tpi_edgelist <- create_edgelist(nrs_selections, tpi_selections) 
nrs_tpi_edgelist <- prettify_edgelist(nrs_tpi_edgelist)
nrs_tos_edgelist <- create_edgelist(nrs_selections, tos_selections) 
nrs_tos_edgelist <- prettify_edgelist(nrs_tos_edgelist)
sh_tpi_edgelist <- create_edgelist(sh_selections, tpi_selections) 
sh_tpi_edgelist <- prettify_edgelist(sh_tpi_edgelist)
sh_tos_edgelist <- create_edgelist(sh_selections, tos_selections) 
sh_tos_edgelist <- prettify_edgelist(sh_tos_edgelist)
psh_tpi_edgelist <- create_edgelist(psh_selections, tpi_selections) 
psh_tpi_edgelist <- prettify_edgelist(psh_tpi_edgelist)
psh_tos_edgelist <- create_edgelist(psh_selections, tos_selections) 
psh_tos_edgelist <- prettify_edgelist(psh_tos_edgelist)


# Create edgelists for bipartite network visualizations, add attributes
tj_nrs_edgelist <- create_edgelist(tj_selections, nrs_selections) 
tj_nrs_edgelist <- prettify_edgelist(tj_nrs_edgelist)
tj_nrs_edgelist <- add_attributes(tj_nrs_edgelist) 
ntj_nrs_edgelist <- create_edgelist(ntj_selections, nrs_selections) 
ntj_nrs_edgelist <- prettify_edgelist(ntj_nrs_edgelist)
ntj_nrs_edgelist <- add_attributes(ntj_nrs_edgelist) 
ag_nrs_edgelist <- create_edgelist(ag_selections, nrs_selections) 
ag_nrs_edgelist <- prettify_edgelist(ag_nrs_edgelist)
ag_nrs_edgelist <- add_attributes(ag_nrs_edgelist)
tj_fts_edgelist <- create_edgelist(tj_selections, fts_selections) 
tj_fts_edgelist <- prettify_edgelist(tj_fts_edgelist)
tj_fts_edgelist <- add_attributes(tj_fts_edgelist) 
ntj_fts_edgelist <- create_edgelist(ntj_selections, fts_selections) 
ntj_fts_edgelist <- prettify_edgelist(ntj_fts_edgelist)
ntj_fts_edgelist <- add_attributes(ntj_fts_edgelist)
ag_fts_edgelist <- create_edgelist(ag_selections, fts_selections)
ag_fts_edgelist <- prettify_edgelist(ag_fts_edgelist)
ag_fts_edgelist <- add_attributes(ag_fts_edgelist)
tj_ats_edgelist <- create_edgelist(tj_selections, ats_selections)
tj_ats_edgelist <- prettify_edgelist(tj_ats_edgelist)
tj_ats_edgelist <- add_attributes(tj_ats_edgelist)
ntj_ats_edgelist <- create_edgelist(ntj_selections, ats_selections) 
ntj_ats_edgelist <- prettify_edgelist(ntj_ats_edgelist)
ntj_ats_edgelist <- add_attributes(ntj_ats_edgelist)
ag_ats_edgelist <- create_edgelist(ag_selections, ats_selections) 
ag_ats_edgelist <- prettify_edgelist(ag_ats_edgelist)
ag_ats_edgelist <- add_attributes(ag_ats_edgelist)
tj_sh_edgelist <- create_edgelist(tj_selections, sh_selections)
tj_sh_edgelist <- prettify_edgelist(tj_sh_edgelist)
tj_sh_edgelist <- add_attributes(tj_sh_edgelist)
ntj_sh_edgelist <- create_edgelist(ntj_selections, sh_selections) 
ntj_sh_edgelist <- prettify_edgelist(ntj_sh_edgelist)
ntj_sh_edgelist <- add_attributes(ntj_sh_edgelist)
ag_sh_edgelist <- create_edgelist(ag_selections, sh_selections) 
ag_sh_edgelist <- prettify_edgelist(ag_sh_edgelist)
ag_sh_edgelist <- add_attributes(ag_sh_edgelist)
tj_psh_edgelist <- create_edgelist(tj_selections, psh_selections)
tj_psh_edgelist <- prettify_edgelist(tj_psh_edgelist)
tj_psh_edgelist <- add_attributes(tj_psh_edgelist)
ntj_psh_edgelist <- create_edgelist(ntj_selections, psh_selections) 
ntj_psh_edgelist <- prettify_edgelist(ntj_psh_edgelist)
ntj_psh_edgelist <- add_attributes(ntj_psh_edgelist)
ag_psh_edgelist <- create_edgelist(ag_selections, psh_selections) 
ag_psh_edgelist <- prettify_edgelist(ag_psh_edgelist)
ag_psh_edgelist <- add_attributes(ag_psh_edgelist)


# Create edgelists for unipartite network visualizations, add attributes
tj_tj_fts_edgelist <- create_edgelist(tj_selections, tj_selections)
tj_tj_fts_edgelist <- prettify_edgelist(tj_tj_fts_edgelist)
tj_tj_fts_edgelist <- add_unipartite_attributes(tj_tj_fts_edgelist)
tj_tj_fts_edgelist <- clean_unipartite(tj_tj_fts_edgelist)
tj_tj_ats_edgelist <- create_edgelist(tj_selections, tj_selections)
tj_tj_ats_edgelist <- prettify_edgelist(tj_tj_ats_edgelist)
tj_tj_ats_edgelist <- add_unipartite_attributes(tj_tj_ats_edgelist)
tj_tj_ats_edgelist <- clean_unipartite(tj_tj_ats_edgelist)
tj_tj_nrs_edgelist <- create_edgelist(tj_selections, tj_selections)
tj_tj_nrs_edgelist <- prettify_edgelist(tj_tj_nrs_edgelist)
tj_tj_nrs_edgelist <- add_unipartite_attributes(tj_tj_nrs_edgelist)
tj_tj_nrs_edgelist <- clean_unipartite(tj_tj_nrs_edgelist)
tj_tj_sh_edgelist <- create_edgelist(tj_selections, tj_selections)
tj_tj_sh_edgelist <- prettify_edgelist(tj_tj_sh_edgelist)
tj_tj_sh_edgelist <- add_unipartite_attributes(tj_tj_sh_edgelist)
tj_tj_sh_edgelist <- clean_unipartite(tj_tj_sh_edgelist)
tj_tj_psh_edgelist <- create_edgelist(tj_selections, tj_selections)
tj_tj_psh_edgelist <- prettify_edgelist(tj_tj_psh_edgelist)
tj_tj_psh_edgelist <- add_unipartite_attributes(tj_tj_psh_edgelist)
tj_tj_psh_edgelist <- clean_unipartite(tj_tj_psh_edgelist)
ntj_ntj_fts_edgelist <- create_edgelist(ntj_selections, ntj_selections)
ntj_ntj_fts_edgelist <- prettify_edgelist(ntj_ntj_fts_edgelist)
ntj_ntj_fts_edgelist <- add_unipartite_attributes(ntj_ntj_fts_edgelist)
ntj_ntj_fts_edgelist <- clean_unipartite(ntj_ntj_fts_edgelist)
ntj_ntj_ats_edgelist <- create_edgelist(ntj_selections, ntj_selections)
ntj_ntj_ats_edgelist <- prettify_edgelist(ntj_ntj_ats_edgelist)
ntj_ntj_ats_edgelist <- add_unipartite_attributes(ntj_ntj_ats_edgelist)
ntj_ntj_ats_edgelist <- clean_unipartite(ntj_ntj_ats_edgelist)
ntj_ntj_nrs_edgelist <- create_edgelist(ntj_selections, ntj_selections)
ntj_ntj_nrs_edgelist <- prettify_edgelist(ntj_ntj_nrs_edgelist)
ntj_ntj_nrs_edgelist <- add_unipartite_attributes(ntj_ntj_nrs_edgelist)
ntj_ntj_nrs_edgelist <- clean_unipartite(ntj_ntj_nrs_edgelist)
ntj_ntj_sh_edgelist <- create_edgelist(ntj_selections, ntj_selections)
ntj_ntj_sh_edgelist <- prettify_edgelist(ntj_ntj_sh_edgelist)
ntj_ntj_sh_edgelist <- add_unipartite_attributes(ntj_ntj_sh_edgelist)
ntj_ntj_sh_edgelist <- clean_unipartite(ntj_ntj_sh_edgelist)
ntj_ntj_psh_edgelist <- create_edgelist(ntj_selections, ntj_selections)
ntj_ntj_psh_edgelist <- prettify_edgelist(ntj_ntj_psh_edgelist)
ntj_ntj_psh_edgelist <- add_unipartite_attributes(ntj_ntj_psh_edgelist)
ntj_ntj_psh_edgelist <- clean_unipartite(ntj_ntj_psh_edgelist)
ag_ag_fts_edgelist <- create_edgelist(ag_selections, ag_selections)
ag_ag_fts_edgelist <- prettify_edgelist(ag_ag_fts_edgelist)
ag_ag_fts_edgelist <- add_unipartite_attributes(ag_ag_fts_edgelist)
ag_ag_fts_edgelist <- clean_unipartite(ag_ag_fts_edgelist)
ag_ag_ats_edgelist <- create_edgelist(ag_selections, ag_selections)
ag_ag_ats_edgelist <- prettify_edgelist(ag_ag_ats_edgelist)
ag_ag_ats_edgelist <- add_unipartite_attributes(ag_ag_ats_edgelist)
ag_ag_ats_edgelist <- clean_unipartite(ag_ag_ats_edgelist)
ag_ag_nrs_edgelist <- create_edgelist(ag_selections, ag_selections)
ag_ag_nrs_edgelist <- prettify_edgelist(ag_ag_nrs_edgelist)
ag_ag_nrs_edgelist <- add_unipartite_attributes(ag_ag_nrs_edgelist)
ag_ag_nrs_edgelist <- clean_unipartite(ag_ag_nrs_edgelist)
ag_ag_sh_edgelist <- create_edgelist(ag_selections, ag_selections)
ag_ag_sh_edgelist <- prettify_edgelist(ag_ag_sh_edgelist)
ag_ag_sh_edgelist <- add_unipartite_attributes(ag_ag_sh_edgelist)
ag_ag_sh_edgelist <- clean_unipartite(ag_ag_sh_edgelist)
ag_ag_psh_edgelist <- create_edgelist(ag_selections, ag_selections)
ag_ag_psh_edgelist <- prettify_edgelist(ag_ag_psh_edgelist)
ag_ag_psh_edgelist <- add_unipartite_attributes(ag_ag_psh_edgelist)
ag_ag_psh_edgelist <- clean_unipartite(ag_ag_psh_edgelist)


## Write edgelists to csv files
setwd("/Users/gast/Desktop/THESIS/Repos/ma-thesis-shiny/input")
# bipartite
write2csv(tj_nrs_edgelist) 
write2csv(ntj_nrs_edgelist)
write2csv(ag_nrs_edgelist)  
write2csv(tj_fts_edgelist)
write2csv(ntj_fts_edgelist)
write2csv(ag_fts_edgelist)
write2csv(tj_ats_edgelist)
write2csv(ntj_ats_edgelist)
write2csv(ag_ats_edgelist)
write2csv(tj_sh_edgelist)
write2csv(ntj_sh_edgelist)
write2csv(ag_sh_edgelist)
write2csv(tj_psh_edgelist)
write2csv(ntj_psh_edgelist)
write2csv(ag_psh_edgelist)
#unipartite
write2csv(tj_tj_fts_edgelist)
write2csv(tj_tj_ats_edgelist)
write2csv(tj_tj_nrs_edgelist)
write2csv(tj_tj_sh_edgelist)
write2csv(tj_tj_psh_edgelist)
write2csv(ntj_ntj_fts_edgelist)
write2csv(ntj_ntj_ats_edgelist)
write2csv(ntj_ntj_nrs_edgelist)
write2csv(ntj_ntj_sh_edgelist)
write2csv(ntj_ntj_psh_edgelist)
write2csv(ag_ag_fts_edgelist)
write2csv(ag_ag_ats_edgelist)
write2csv(ag_ag_nrs_edgelist)
write2csv(ag_ag_sh_edgelist)
write2csv(ag_ag_psh_edgelist)

## Write nodelists to csv files
write2csv(tj_nodelist)
write2csv(ntj_nodelist)
write2csv(ag_nodelist)
write2csv(nrs_nodelist)
write2csv(fts_nodelist)
write2csv(ats_nodelist)
write2csv(sh_nodelist)
write2csv(psh_nodelist)


