---
feature_image: images/blog/manyenviron_hexlogo.png
image: images/blog/manyenviron_hexlogo.png
title: "How Do Environmental Treaties Connect?"
authors: "James Hollway, Esther Peev, Henrique Sposito, and Bernhard Bieri"
date: "2024-04-23"
output: md_document
---



## Connecting Environmental Treaties

`{manyenviron}` stores several datasets on environmental agreements.

There are many different ways in which treaties could be linked.
For example, a treaty can be substituted by another, complemented by protocols,
changed through amendments, and/or cite another treaty.
To facilitate the identification and connection between the databases in the package, the `manypkgs::code_agreements()` function has been developed to generate a treatyID for treaties in a database.
Each of the `{manyenviron}` dataset contains a treatyID column.
To understand what are the information provided through by the treatyID,
as well as the benefits of using treatyID to spot the relationships between the treaties across several datasets
in a database, please visit this [article](https://globalgov.github.io/manypkgs/articles/agreements.html)
on the `{manypkgs}` website.

Below are two examples of how to visualise treaty linkages for the references and the
memberships databases in `{manyenviron}`.
We use `{manynet}` to create the network plots, more information about the package can be found
[here](https://snlab-ch.github.io/manynet/).

### Visualising How Agreements Connect by References

The graph below illustrates a sample of earliest 25 treaties from the references database. We select only treaties that cite other treaties. The treatyIDs are used to facilitate the reading and illustration of the relationships.


```r
library(dplyr)
library(ggplot2)
library(manynet)
library(stringr)
library(tidygraph)
# Get dataset
references <- manyenviron::references$ECOLEX_REF %>%
  dplyr::distinct() %>%
  dplyr::mutate(year = stringr::str_extract(treatyID2,
                                            "[:digit:]{4}")) %>%
  dplyr::filter(RefType == "Cites") %>%
  dplyr::arrange(year)

# Plot with manynet
manynet::gglineage(references[1:25, c(1, 2, 3)]) +
  labs(title = "Treaty Lineage of Selected Agreements",
       caption = "Source: manyenviron") +
  theme(plot.title = element_text(family = "sans",
                                  size = 18,
                                  hjust = 0.5))
```

We can also take a look at a single lineage of Enviromental treaties.
Let's look at treaties that either cite or ammend the Ramsar Convention on Wetlands of International
Importance Especially as Waterfowl Habitat ("RAMSA_1971A").


```r
library(dplyr)
library(ggplot2)
library(manynet)
library(stringr)
# Subset dataset
ramsa <- manyenviron::references$ECOLEX_REF %>%
  dplyr::distinct() %>%
  dplyr::mutate(year = stringr::str_extract(treatyID2,
                                            "[:digit:]{4}")) %>%
  dplyr::filter(treatyID2 == "RAMSA_1971A") %>%
  dplyr::arrange(year)

# Get agreement titles, join information, and plot
manyenviron::agreements$ECOLEX %>%
  dplyr::select(Title, treatyID) %>%
  dplyr::rename(treatyID1 = treatyID) %>%
  dplyr::right_join(ramsa) %>%
  dplyr::rename(to = treatyID2,
                from = Title) %>%
  dplyr::select(to, from, RefType) %>%
  manynet::as_tidygraph() %>%
  tidygraph::activate(edges) %>%
  manynet::autographr(edge_color = "RefType")
```

<img src="/blog/how-do-environmental-treaties-connect_files/figure-html/ramsar-1.png" width="672" style="display: block; margin: auto;" />

### Visualising How Agreements Connect by Country Membership

The graph below illustrates the connection between parties and treaties for multilateral environmental agreements signed into force in 1980 using the memberships database.


```r
# Subset and plot data
manyenviron::memberships$IEADB_MEM %>%
  dplyr::mutate(year = stringr::str_extract(manyID,
                                            "[:digit:]{4}")) %>%
  dplyr::filter(year == "1980") %>%
  dplyr::select(treatyID, stateID, year) %>%
  group_by(treatyID) %>%
  filter( n() > 2) %>% # keep only multilateral treaties
  manynet::as_tidygraph() %>%
  tidygraph::activate(nodes) %>%
  dplyr::mutate(color = ifelse(grepl("_", name), 1, 0),
                size = ifelse(grepl("_", name), 4, 2),
                shape = ifelse(grepl("_", name),
                               "square", "circle")) %>%
  manynet::to_undirected() %>%
  manynet::autographr(node_color = "color",
                      node_size = "size",
                      node_shape = "shape")
```

<img src="/blog/how-do-environmental-treaties-connect_files/figure-html/memberships-1.png" width="672" style="display: block; margin: auto;" />

### Extra: dynamic visualisation

For events or panel network data, `{manynet}` allows users to visualise how networks evolve over time as animations.


```r
# Install the development version of manynet, for now
remotes::install_github("snlab-ch/manynet", ref = "develop")
# Subset and plot data
manyenviron::memberships$IEADB_MEM %>%
  dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
  dplyr::filter(year == "1980" | year == "1981") %>%
  dplyr::select(treatyID, stateID, year) %>%
  group_by(treatyID) %>%
  filter( n() > 2) %>% # keep only multilateral treaties
  manynet::as_tidygraph() %>%
  tidygraph::activate(nodes) %>%
  dplyr::mutate(color = ifelse(grepl("_", name), "blue", "red"),
                size = ifelse(grepl("_", name), 4, 2),
                shape = ifelse(grepl("_", name), "square", "circle")) %>%
  manynet::to_waves(attribute = "year") %>% 
  manynet::autographd(delete.vertices = TRUE, node_color = "color",
                      node_size = "size", node_shape = "shape")
```
 
