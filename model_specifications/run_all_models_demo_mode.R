# This script allows to run the RScripts in all subfolders (i.e., fitting all the models). On a normal machine, this will only work with the demo dataset due to memory limitations.

# IMPORTANT: Before running this script, make sure to set the working directory to the source file location. 

# Get full paths to all .R files recursively
all_scripts <- list.files(
  path = ".", 
  pattern = "\\.R$", 
  recursive = TRUE, 
  full.names = TRUE
)

# Get current working directory (top-level folder)
top_dir <- normalizePath(".")

# Filter out any scripts that are directly in the top-level folder
subfolder_scripts <- Filter(function(path) {
  dirname(normalizePath(path)) != top_dir
}, all_scripts)

# Run each script with working directory set to the script's folder
for (script in subfolder_scripts) {
  script_dir <- dirname(script)
  script_name <- basename(script)
  
  message("\nRunning: ", script_name, " in ", script_dir)
  
  old_wd <- getwd()
  setwd(script_dir)
  
  tryCatch(
    {
      source(script_name, echo = TRUE)
    },
    error = function(e) {
      message("Error in ", script_name, ": ", e$message)
    }
  )
  
  setwd(old_wd)
}
