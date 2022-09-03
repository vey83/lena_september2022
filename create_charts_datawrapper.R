#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena_september2022")

###Config: Bibliotheken laden, Pfade/Links definieren, bereits vorhandene Daten laden
source("config.R",encoding = "UTF-8")

monate_de <- c("Januar", "Februar", "März", 
  "April", "Mai", "Juni", "July", 
  "August", "September", "Oktober",
  "November", "Dezember")

monate_fr <- c("janvier","février","mars",
               "avril","mai","juin","juillet",
               "août","septembre","octobre",
               "novembre","décembre")

monate_it <- c("gennaio","febbraio","marzo",
               "aprile","maggio","giugno",
               "luglio","agosto","settembre",
               "ottobre","novembre","dicembre")

#Ids von Karten-Vorlagen
vorlagen_uebersicht <- c("O1i9P","Ocame","ot8Mm")
vorlagen_gemeinden <- c("EuC56","JJ03i","CPwql")
vorlagen_kantone <- c("HH2Hs","G7A2k","sobvY")

#Titel aktuelle Vorlagen
vorlagen <- get_vorlagen(json_data,"de")
vorlagen_fr <- get_vorlagen(json_data,"fr")
vorlagen_it <- get_vorlagen(json_data,"it")
vorlagen_all <- rbind(vorlagen,vorlagen_fr)
vorlagen_all <- rbind(vorlagen_all,vorlagen_it)

#Ordnerstruktur erstellen
team_id <- "6Gn1afus"
date_voting <- as.Date(json_data$abstimmtag,format="%Y%m%d")

main_folder <- dw_create_folder(name=paste0("Abstimmung ",day(date_voting),". ",monate[month(date_voting)]," ",year(date_voting)),organization_id = team_id)

folder_eid <- dw_create_folder("Eidgenössische Abstimmungen",parent_id = main_folder$id)
folder_kantonal <- dw_create_folder("Kantonale Abstimmungen",parent_id = main_folder$id)
folder_infografiken <- dw_create_folder("SDA Infografiken",parent_id = main_folder$id)

folder_uebersicht <- dw_create_folder("Übersicht",parent_id = folder_eid$id)
folder_einzugsgebiete <- dw_create_folder("Einzugsgebiete",parent_id = folder_eid$id)
folder_kantone <- dw_create_folder("Kantone",parent_id = folder_eid$id)
folder_schweiz <- dw_create_folder("Schweiz",parent_id = folder_eid$id)

folder_gemeindeebene <- dw_create_folder("Gemeindeebene",parent_id = folder_schweiz$id)
folder_kantonsebene <- dw_create_folder("Kantonsebene",parent_id = folder_schweiz$id)

folder_vorlagen <- dw_create_folder("Vorlagen",parent_id = folder_kantone$id)
folder_vorlagen_de <- dw_create_folder("Deutsch",parent_id = folder_vorlagen$id)
folder_vorlagen_fr <- dw_create_folder("Französisch",parent_id = folder_vorlagen$id)

for (k in kantonal_short) {
dw_create_folder(k,parent_id = folder_kantonal$id)  
}  

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe")

#Uebersicht
titel_de <- paste0("Aktueller Stand der Abstimmungen vom ",day(date_voting),". ",monate_de[month(date_voting)]," ",year(date_voting))
titel_fr <- paste0("Etat actuel des votes au ",day(date_voting)," ",monate_fr[month(date_voting)]," ",year(date_voting))
titel_it <- paste0("Situazione attuale delle votazioni del ",day(date_voting)," ",monate_it[month(date_voting)]," ",year(date_voting))

titel_all <- c(titel_de,titel_fr,titel_it)

for (i in 1:3) {
data_chart <- dw_copy_chart(vorlagen_uebersicht[i])
dw_edit_chart(data_chart$id,
              title=titel_all[i],
              folderId = folder_uebersicht$id)
dw_publish_chart(data_chart$id)
metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

new_entry <- data.frame("Uebersicht",
                        "alle",
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        metadata_chart$id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`)
colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

#Schweizkarten erstellen, Gemeinde und Kantone
for (v in 1:length(vorlagen_short)) {
  title_select <- c(v,v+4,v+8)
  #Alle drei Sprachen
  for (i in 1:3) {
  #Gemeinden  
  data_chart <- dw_copy_chart(vorlagen_gemeinden[i])
  dw_edit_chart(data_chart$id,
                title=vorlagen_all$text[title_select[i]],
                folderId = folder_gemeindeebene$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 tolower(monate_de[month(date_voting)]),year(date_voting),
                                                 "/master/Output/",vorlagen_short[v],"_dw.csv")))
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
  new_entry <- data.frame("Schweizer Gemeinden",
                          vorlagen_short[v],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  
  #Kantone
  data_chart <- dw_copy_chart(vorlagen_kantone[i])
  dw_edit_chart(data_chart$id,
                title=vorlagen_all$text[title_select[i]],
                folderId = folder_kantonsebene$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 tolower(monate_de[month(date_voting)]),year(date_voting),
                                                 "/master/Output/",vorlagen_short[v],"_dw_kantone.csv")))
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
  new_entry <- data.frame("Schweizer Kantone",
                          vorlagen_short[v],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  
  
  
  
  }
  
}  

#Vorlagen für Erstellung der Kantone
for (v in 1:length(vorlagen_short)) {

  #Gemeinden  
    data_chart <- dw_copy_chart(vorlagen_gemeinden[1])
    dw_edit_chart(data_chart$id,
                  title=vorlagen_all$text[v],
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = 116284,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   tolower(monate_de[month(date_voting)]),year(date_voting),
                                                   "/master/Output/",vorlagen_short[v],"_dw.csv")))
    dw_publish_chart(data_chart$id)

    data_chart <- dw_copy_chart(vorlagen_gemeinden[2])
    dw_edit_chart(data_chart$id,
                  title=vorlagen_all$text[v+4],
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = 116285,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   tolower(monate_de[month(date_voting)]),year(date_voting),
                                                   "/master/Output/",vorlagen_short[v],"_dw.csv")))
    dw_publish_chart(data_chart$id)
}

#Daten Speichern
library(xlsx)
write.xlsx(grafiken_uebersicht,"./Data/metadaten_grafiken.xlsx",row.names = FALSE)
