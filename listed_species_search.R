### Kissimmee Prairie Preserve Vouchering Plant List

#Need to compare list of target plants with the endangered/rare/threatened list from Archbold Station

KISS_plants = read.csv('target_species-v2.csv', header = TRUE)
listed = read.csv('Listed_plants.csv', header = TRUE)

target_species = KISS_plants$Scientific.Name

list_cross = sapply(target_species, function(x){
  grep(x, listed$Plant.Species)
})

cross_read = as.vector(lapply(list_cross, length))

cross_ref = list_cross[cross_read > 0]

#Look in synonyms

syno_cross = sapply(target_species, function(x){
  grep(x, listed$Recent.Synonyms)
})

syno_read = as.vector(lapply(syno_cross, length))

syno_ref = syno_cross[syno_read > 0]

#Can we make a more comprehensive list and organize it a bit?

#Pulled native plants that occur on landward counties bordering Okeechobee.

AFP_natives <- read.csv("KPPSP_natives.csv", header = TRUE)
AFP_natrlzd <- read.csv("KPPSP_introduced.csv", header = TRUE)
ALL_species <- rbind(AFP_natives, AFP_natrlzd)

#Crosswalk function

crosswalk = function(x, y){
  hits = sapply(x, function(z){
    grep(z, y)
  })
  hit_read = as.vector(lapply(hits, length))
  hit_ref = hits[hit_read > 0]
  miss_ref = hits[hit_read == 0]
  pull = unlist(hit_ref)
  result = array(list(hit_ref, miss_ref, y[pull], y[-pull]), dimnames = list(c("hits", "misses", "matches", "missing")))
}

cross_allspec = crosswalk(ALL_species$Scientific.Name, target_species)
cross_natives = crosswalk(AFP_natives$Scientific.Name, target_species)
cross_natrlzd = crosswalk(AFP_natrlzd$Scientific.Name, target_species)

#We want to pull the number of genera and get a list of species without photos to target for the event.

AFP_cross = sapply(target_species, function(x){
  grep(x, AFP_natives$Scientific.Name)
})

pull = vector("numeric", length(AFP_cross))

for(i in 1:length(pull)){
  if(length(AFP_cross[[i]])>0){
    pull[i] = AFP_cross[[i]]
  }
}

AFP_pull = AFP_natives[pull[pull > 0],]

genera = AFP_pull$Genus

write.csv(AFP_pull, "AFP_plant-list.csv")

