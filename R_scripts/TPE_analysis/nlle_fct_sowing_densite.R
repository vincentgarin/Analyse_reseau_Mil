######################################################################
# Ajouter des fonctions pour modifier la date de semis et la densité #
######################################################################

# fonction modèle

mdf_clock <- function(apsim_file, start_date, end_date) {
  
  # Parse the XML
  doc <- read_xml(apsim_file)
  
  # Find the <start_date> and <end_date> nodes
  start_node <- xml_find_first(doc, "//start_date")
  end_node   <- xml_find_first(doc, "//end_date")
  
  # Replace the text content with the new dates
  xml_text(start_node) <- start_date
  xml_text(end_node)   <- end_date
  
  # Overwrite the same file (already named after the met file)
  write_xml(doc, apsim_file)
  
  return(apsim_file)
}

# possibilité d'utiliser une IA (Claude, chatGPT)

# demander une fonction qui prend le fichier apsim en entré plus un nouvelle argument
# ici: date de semi , densité (2e fct)

# 1ère fct
# mdf_sowing_date()

# 2e fct
# mdf_density()

# intégrer la fonction dans le script APSIM_yield_prediction.R
# les arguments sont à récupérer par example dans le d.f
# ex d_trials$year[i] ou dans meta_data

# 1ere fct meta_data$SOWING_DATE

# 2eme fct meta_data$PLANT_DENSITY

# l'information du fichier apsim (xml) à modifier

# <date type="text" description="Enter sowing date (dd-mmm) : ">15-jul</date>
# 15-jul


<manager name="Sow on a fixed date">
  <ui>
  <category type="category" description="Sowing criteria" />
    <date type="text" description="Enter sowing date (dd-mmm) : ">15-jul</date>
      <category type="category" description="Sowing parameters" />
        <crop type="crop" description="Enter name of crop to sow : ">millet</crop>
          <density type="text" description="Enter sowing density  (plants/m2) : ">7.5</density>
            <depth type="text" description="Enter sowing depth  (mm) : ">30</depth>
              <cultivar type="cultivars" description="Enter cultivar : ">wrajpop</cultivar>
                </ui>