source("utils.r")
source("pathfinder.r")

Author <- read_csv("data/author.csv")
Keyword <- read_csv("data/keyword.csv")
Paper <- read_csv("data/paper.csv")
AuthorPaper <- read_csv("data/author-paper.csv")
KeywordPaper <- read_csv("data/keyword-paper.csv")


#######################
### Analiza prispevkov avtorjev v reviji in medsebojnega sodelovanja preko avtorstev
### Uporaba metod analize omrežij

# Tabela uteži prispevkov enega avtorja za posamezen članek
author.paper.weight <- AuthorPaper %>%
  group_by(id) %>%
  summarise(weight=1/n()) %>% 
  inner_join(AuthorPaper, by="id") 

# Pregled
# author.paper.weight %>%
#   inner_join(Author, by="aid") %>% 
#   group_by(aid, author) %>%
#   summarise(total.contribution = sum(weight)) %>%
#   arrange(desc(total.contribution)) %>% View


# Izračun povezav med avtorji. Če avtorja sodelujeta na kakem članku, sta povezana.
# Primer: če sta na nekem članku to 2 izmed 5 avtorjev, vsak od avtorjev pridobi utež 1/5.
# Če avtorja sodelujeta (sta soavtorja) na več člankih, se prispevki seštejejo v celotno utež na
# povezavi med avtorjema. 
author.author.links <- author.paper.weight %>% 
  inner_join(author.paper.weight, by="id") %>% # Ustvarjanje povezav preko skupnih člankov 
  mutate(                                      # t.i. "množenje omrežij"
    from=aid.x,
    to=aid.y,
    weight=weight.x
  ) %>%
  select(from, to, weight) %>%     # Izvleček povezav za sodelovanje na posameznem članku
  group_by(from, to) %>%      # združevanje vseh povezav pri sodelovanju
  summarise(n = sum(weight)) %>%   
  filter(from < to)   # ohranimo samo povezave v eno smer (tudi brez zank). 
     # zanke bi določale celoten prispevek avtorjev kot zgoraj

# Seznam avtorjev z njihovimi prispevki
author.all <- author.paper.weight %>%
  inner_join(Author, by="aid") %>% 
  group_by(aid, author) %>%
  summarise(total.contribution = sum(weight)) %>%
  arrange(desc(total.contribution)) 

# Dobimo omrežje
# VOZLIŠĆA: author.all, identifikator avtorja je "aid"
# POVEZAVE: author.author.links, imajo tudi utež "n", kot opisano

# Če je veliko točk in povezav so vizualizacije lahko nepregledne in/ali počasne
# Poglejmo si podomrežje na 100 najproduktivnejših avtorjih (glede na revijo)
author.top100 <- author.all %>% 
  head(100)

# "aid"-ji teh avtorjev
author.top100.ids <- author.top100 %>% pull(aid)

# Filtriranje povezav, ki so v podomrežju
author.author.links.top100 <- author.author.links %>%
  filter(from %in% author.top100.ids & to %in% author.top100.ids)

# Primer uporabe vizualizacije s knjižnico visNetwork
# nodes <- data.frame(id = 1:3)
# edges <- data.frame(from = c(1,2), to = c(1,3))
# visNetwork(nodes, edges, width = "100%")

# author.author.links.top100 %>% View

# Vizualizacija omrežja sodelovanj na 100 najproduktivnejših avtorjih
visNetwork(
  author.top100 %>% mutate(
    id=aid,   # identifikator mora biti v stolpcu "id"
    label=sprintf("%s (%.2f)", author, total.contribution),  # labele vozlišč
    title=sprintf("%s (%.2f)", author, total.contribution),  # labele "na klik"
    value=total.contribution  # Velikost vozlišča
  ),
  author.author.links.top100 %>% mutate(
    width=n*10,   # Debelina povezave - pomnožimo jo glede na "občutek", da lepše izgleda
    # label=sprintf("%.2f", n), # Napis na povezavi (ga ne želimo)
    title=sprintf("%.2f", n)  # Napis na povezavi (ob kliku/oz. potovanju miške nad njo)
  ),
  width="100%",   # Velikost na spletni strani
  height="800px",
  main="Author collaboration (top 100 authors by contribution)"
) %>% 
  visIgraphLayout(physics=TRUE) %>%
  visPhysics(stabilization = FALSE, solver="barnesHut", barnesHut=list(
    gravitationalConstant=-2000,
    centralGravity=0.3,
    springLength=150,
    springConstant=0.03,
    damping=0.9,
    avoidOverlap=1
  )) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("authors-100.html") # Shranjevanje v HTML datoteko (stran), ki jo lahko odpremo v brskalniku

###################################################
## Vizualizacija sodelovanj vseh avtorjev
###################################################

visNetwork(
  author.all %>% mutate(
    id=aid,
    label=sprintf("%s (%.2f)", author, total.contribution),
    title=sprintf("%s (%.2f)", author, total.contribution),
    value=total.contribution
  ),
  author.author.links %>% mutate(
    width=n*10,
    # label=sprintf("%.2f", n),
    title=sprintf("%.2f", n)
  ),
  width="100%",
  height="800px",
  main="Author collaboration"
) %>% 
  visIgraphLayout(physics=TRUE) %>%
  visPhysics(stabilization = FALSE, solver="barnesHut", barnesHut=list(
    gravitationalConstant=-2000,
    centralGravity=0.3,
    springLength=150,
    springConstant=0.03,
    damping=0.9,
    avoidOverlap=1
  )) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("authors-all.html")

###################################################
## Glavna komponenta sodelovanj
###################################################



author.all.components <- calculate.components(author.author.links, author.all) 

min.component.size <- 6
author.all.main.components <- author.all.components %>% 
  filter(cnt > min.component.size)

# author.component.cnt <- author.all.components %>%
#   group_by(component) %>%
#   summarise(n=n()) %>%
#   arrange(desc(n)) 

# author.all.main.components %>% View
# author.author.links %>% 
#   induced.links(author.all.main.components, nodeId="aid") %>% nrow

visNetwork(
  author.all.main.components %>% mutate(
    id=aid,
    label=sprintf("%s (%.2f)", author, total.contribution),
    title=sprintf("%s (%.2f)", author, total.contribution),
    value=total.contribution,
    group=component
  ),
  author.author.links %>% 
    induced.links(author.all.main.components, nodeId="aid") %>%
    mutate(
      width=n*10,
      # label=sprintf("%.2f", n),
      title=sprintf("%.2f", n)
    ),
  width="100%",
  height="800px",
  main=sprintf("Author collaboration (main components, cutoff=%d)", min.component.size)
) %>% 
  visIgraphLayout(physics=TRUE) %>%
  visPhysics(stabilization = FALSE, solver="barnesHut", barnesHut=list(
    gravitationalConstant=-2000,
    centralGravity=0.3,
    springLength=200,
    springConstant=0.03,
    damping=0.9,
    avoidOverlap=1
  )) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("authors-main-components.html")

#################################################
# Čiščenje ključnih besed
#################################################

# Frekvence ključnih besed
Paper %>%
  inner_join(KeywordPaper, by="id") %>%
  inner_join(Keyword, by="kid") %>% 
  group_by(keyword) %>%
  summarise(cnt =n()) %>% 
  arrange(desc(cnt)) %>% 
  write.xlsx("data/freq-keyword.xlsx")

replacements <- c(
  "(^.*configuration)(s)$"="\\1",
  "(^.*graph)(s)$"="\\1",
  "(^.*factorisation)(s)$"="\\1",
  "(^.*)(factorization)(s)$"="\\1factorisation",
  "(^.*group)(s)$"="\\1",
  "(^.*topolog)(ies)$"="\\1y",
  "(^.*polytop)(es)$"="\\1e",
  "(^.*space)(s)$"="\\1",
  "(^.*curve)(s)$"="\\1",
  "(^.*aggregate)(s)$"="\\1",
  "(^.*famil)(ies)$"="\\1y",
  "(^.*patch)(es)$"="\\1",
  "(^.*line)(s)$"="\\1",
  "(^.*scheme)(s)$"="\\1",
  "  "=" ",
  "(^.*quotient)(s)$"="\\1",
  "(^.*plane)(s)$"="\\1",
  "(^.*lattice)(s)$"="\\1",
  "(^.*function)(s)$"="\\1",
  "(^.*automorphism)(s)$"="\\1",
  "(^.*grap)(s)$"="\\1h",
  "(^.*network)(s)$"="\\1",
  "(^.*measure)(s)$"="\\1",
  "(^.*cycle)(s)$"="\\1",
  "(^.*design)(s)$"="\\1",
  "(^.*surface)(s)$"="\\1",
  "(^.*tree)(s)$"="\\1",
  "(^.*system)(s)$"="\\1",
  "(^.*knot)(s)$"="\\1",
  "(^.*coloring)(s)$"="\\1",
  "(^.*colouring)(s)$"="\\1",
  "(^.*dessin)(s)$"="\\1",
  "(^.*permutation)(s)$"="\\1",
  "(^.*number)(s)$"="\\1",
  "(^.*problem)(s)$"="\\1",
  "(^.*class)(es)$"="\\1",
  "(^.*bound)(s)$"="\\1",
  "(^.*product)(s)$"="\\1",
  "(^.*cover)(s)$"="\\1",
  "(^.*voltage)(s)$"="\\1",
  "(^.*vertice)(s)$"="\\1",
  "(^.*code)(s)$"="\\1",
  "(^.*ind)(ices)$"="\\1ex",
  "(^.*cage)(s)$"="\\1",
  "(^.*triangulation)(s)$"="\\1",
  "(^.*transversal)(s)$"="\\1",
  "(^.*strateg)(ies)$"="\\1y",
  "(^.*isomorphism)(s)$"="\\1",
  "(^.*forest)(s)$"="\\1",
  "(^.*leave)(s)$"="\\1",
  "(^.*variet)(ies)$"="\\1y",
  "(^.*triple)(s)$"="\\1",
  "(^.*matroid)(s)$"="\\1",
  "(^.*base)(s)$"="\\1",
  "(^.*cycle)(s)$"="\\1",
  "(^.*bas)(is)$"="\\1e",
  "(^.*map)(s)$"="\\1",
  "(^.*field)(s)$"="\\1",
  "(^.*algorithm)(s)$"="\\1",
  "(^.*identit)(ies)$"="\\1y",
  "(^.*propert)(ies)$"="\\1y",
  "(^.*commutator)(s)$"="\\1",
  "(^.*geometr)(ies)$"="\\1y",
  "(^.*homomorphism)(s)$"="\\1",
  "(^.*determinant)(s)$"="\\1",
  "(^.*symmetr)(ies)$"="\\1y",
  "(^.*polynomial)(s)$"="\\1",
  "(^.*determinant)(s)$"="\\1",
  "(^.*equalit)(ies)$"="\\1y",
  "(^.*snark)(s)$"="\\1",
  "(^.*angle)(s)$"="\\1",
  "(^.*surger)(ies)$"="\\1y",
  "(^.*tiling)(s)$"="\\1",
  "(^.*tiling)(s)$"="\\1",
  "(^.*assignment)(s)$"="\\1",
  "(^.*convexit)(ies)$"="\\1y",
  "(^.*eigenvalue)(s)$"="\\1",
  "(^.*eigenvector)(s)$"="\\1",
  "(^.*integer)(s)$"="\\1",
  "(^.*tour)(s)$"="\\1",
  "(^.*edge)(s)$"="\\1",
  "(^.*polygon)(s)$"="\\1",
  "(^.*decomposition)(s)$"="\\1",
  "(^.*embedding)(s)$"="\\1",
  "(^.*path)(s)$"="\\1",
  "(^.*algebra)(s)$"="\\1",
  "(^.*trilateral)(s)$"="\\1",
  "(^.*tensegrit)(ies)$"="\\1y",
  "(^.*spread)(s)$"="\\1",
  "(^.*game)(s)$"="\\1",
  "(^.*match)(s)$"="\\1",
  "(^.*solid)(s)$"="\\1",
  "(^.*degree)(s)$"="\\1",
  "(^.*crossing)(s)$"="\\1",
  "(^.*enfant)(s)$"="\\1",
  "(^.*cut)(s)$"="\\1",
  "(^.*set)(s)$"="\\1",
  "(^.*covering)(s)$"="\\1",
  "(^.*arrangement)(s)$"="\\1",
  "(^.*value)(s)$"="\\1",
  "(^.*cube)(s)$"="\\1",
  "(^.*word)(s)$"="\\1",
  "(^.*fullerene)(s)$"="\\1",
  "(^.*generator)(s)$"="\\1",
  "(^.*product)(s)$"="\\1",
  "(^.*array)(s)$"="\\1",
  "(^.*hierarch)(ies)$"="\\1y",
  "(^.*packing)(s)$"="\\1",
  "(^.*matri)(ces)$"="\\1x",
  "(^.*theorem)(s)$"="\\1",
  "(^.*involute)(s)$"="\\1",
  "(^.*representation)(s)$"="\\1",
  "(^.*ring)(s)$"="\\1",
  "(^.*relation)(s)$"="\\1",
  "(^.*unital)(s)$"="\\1",
  "(^.*hexagon)(s)$"="\\1",
  "(^.*otope)(s)$"="\\1",
  "(^.*parameterization)(s)$"="\\1",
  "(^.*order)(s)$"="\\1",
  "(^.*partition)(s)$"="\\1",
  "(^.*permanent)(s)$"="\\1",
  "(^.*recurrence)(s)$"="\\1",
  "(^.*coefficient)(s)$"="\\1",
  "(^.*point)(s)$"="\\1",
  "(^.*contribution)(s)$"="\\1",
  "(^.*equidistant)(s)$"="\\1",
  "(^.*produc)(s)$"="\\1t",
  "(^.*distance)(s)$"="\\1",
  "(^.*operation)(s)$"="\\1",
  "(^.*extension)(s)$"="\\1",
  "(^.*disc)(s)$"="\\1",
  "(^.*hypertope)(s)$"="\\1",
  "(^.*nomial)(s)$"="\\1",
  "(^.*multiplicit)(ies)$"="\\1y",
  "(^.*operator)(s)$"="\\1",
  "(^.*model)(s)$"="\\1",
  "(^.*conguration)(s)$"="\\1", 
  "(^.*cone)(s)$"="\\1",
  "(^.*evolute)(s)$"="\\1",
  "(^.*latin square)(s)$"="\\1",
  "(^.*level)(s)$"="\\1",
  "(^.*discriminant)(s)$"="\\1",
  "(^.*chain)(s)$"="\\1",
  "(^.*matching)(s)$"="\\1",
  "(^.*monoid)(s)$"="\\1",
  "- "="-"
)
  

keyword.clean <- Keyword %>% 
  mutate(
    lower=tolower(keyword)
  ) %>%
  arrange(lower) %>%
  (function(data) {
    keyword <- data$lower
    from <- replacements %>% names()
    to <- replacements
    for(i in 1:length(replacements)) {
      keyword <- str_replace(keyword, from[i], to[i])  
    }
    data$lower2 <- keyword
    data
  }) %>% (function(data){
    enum2 <- data %>% 
      arrange(lower2) %>%
      pull(lower2) %>% 
      unique();
    print(enum2)
    data %>% 
      left_join(
        tibble(lower2=enum2) %>% mutate(kid2=row_number()), 
        by="lower2"
      )
  }) 

Keyword2 <- Keyword %>% 
  left_join(keyword.clean, by=c("keyword"="keyword", "kid"="kid")) %>%
  select(kid, kid2, keyword, normalized=lower2) %>%
  mutate(
    normalized=ifelse(is.na(normalized), "NO KEYWORDS", normalized)
  )


keyword.paper.weight <- KeywordPaper %>%
  group_by(id) %>%
  summarise(weight=1/n()) %>%
  # summarise(weight=1) %>% 
  inner_join(KeywordPaper, by="id")  %>% 
  inner_join(Keyword2, by="kid")

# Pregled
# keyword.paper.weight %>%
#   group_by(kid2, normalized) %>%
#   summarise(total.contribution = sum(weight)) %>%
#   arrange(desc(total.contribution)) %>% View


# Izračun povezav med keywordi. Če avtorja sodelujeta na kakem članku, sta povezana.
keyword.keyword.links <- keyword.paper.weight %>% 
  inner_join(keyword.paper.weight, by="id") %>% # Ustvarjanje povezav preko skupnih člankov 
  mutate(                                      # t.i. "množenje omrežij"
    from=kid2.x,
    to=kid2.y,
    weight=weight.x
  ) %>%
  select(from, to, weight) %>%     # Izvleček povezav za sodelovanje na posameznem članku
  group_by(from, to) %>%      # združevanje vseh povezav pri sodelovanju
  summarise(
    n = sum(weight),
    ndeg = n()
  ) %>%   
  filter(from < to)   # ohranimo samo povezave v eno smer (tudi brez zank). 
# zanke bi določale celoten prispevek avtorjev kot zgoraj

# Seznam avtorjev z njihovimi prispevki
keyword.all <- keyword.paper.weight %>%
  group_by(kid2, normalized) %>%
  summarise(
    total.contribution = sum(weight),
    total.degree=n()
  ) %>%
  arrange(desc(total.contribution))

keywords.connected.kid2 <- c(
  keyword.keyword.links %>% pull(from) %>% unique,
  keyword.keyword.links %>% pull(to) %>% unique
) %>% unique


nodes <- keyword.all
links <- keyword.keyword.links 
nodes <- calculate.components(links, nodes, nodeId="kid2") 
visNetwork(
  nodes %>% 
    # filter(kid2 %in% keywords.connected.kid2) %>%
    mutate(
      id=kid2,
      label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      value=total.contribution,
      group=component
    ),
  links %>% mutate(
    width=n*10,
    # label=sprintf("%.2f/%d", n, ndeg),
    title=sprintf("%.2f/%d", n, ndeg)
  ),
  width="100%",
  height="800px",
  main="Keyword co-existence (fractional)"
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
    ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("keywords-all.html")

keyword.important <- keyword.all %>% 
  arrange(desc(total.degree)) %>%
  filter(total.degree > 1)


nodes <- keyword.important
links <- keyword.keyword.links %>% 
  induced.links(keyword.important, nodeId="kid2")
nodes <- calculate.components(links, nodes, nodeId="kid2") 
# keyword.all %>% View
visNetwork(
  nodes %>% 
    mutate(
      id=kid2,
      label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      value=total.contribution,
      group=component
    ),
  links %>%
  mutate(
    width=n*10,
    # label=sprintf("%.2f/%d", n, ndeg),
    title=sprintf("%.2f/%d", n, ndeg)
  ),
  width="100%",
  height="800px",
  main="Keyword co-existence (fractional, more than 1 paper)"
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
  ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("keywords-important.html")


nn <- nodes %>% nrow
l1 <- links %>% nrow
links %>% View
links <- pathfinder_sparse(nodes, links, Inf, nodes %>% length, weight="n")
l2 <- links %>% nrow
warning(sprintf("PF: %d | %d -> %d", nn, l1, l2))
visNetwork(
  nodes %>% 
    mutate(
      id=kid2,
      label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      value=total.contribution,
      group=component
    ),
  links %>%
    mutate(
      width=n*10,
      # label=sprintf("%.2f/%d", n, ndeg),
      title=sprintf("%.2f/%d", n, ndeg)
    ),
  width="100%",
  height="800px",
  main="Keyword co-existence (fractional, more than 1 paper, pathfinder)"
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
  ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("keywords-important-pathfinder.html")




cutoff.degree <- 30
keyword.important.but.top3 <- keyword.important %>%
  filter(total.degree < cutoff.degree)

visNetwork(
  keyword.important.but.top3 %>% 
    mutate(
      id=kid2,
      label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      value=total.contribution
    ),
  keyword.keyword.links %>% 
    induced.links(keyword.important.but.top3, nodeId="kid2") %>%
    mutate(
      width=n*10,
      # label=sprintf("%.2f/%d", n, ndeg),
      title=sprintf("%.2f/%d", n, ndeg)
    ),
  width="100%",
  height="800px",
  main="Keyword co-existence (fractional, more than 2 papers, without top 3)"
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
  ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("keywords-important-but-top-3.html")


