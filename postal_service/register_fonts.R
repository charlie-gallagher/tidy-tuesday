require(dplyr)

# A solution to the problem of systemfonts font styles. 


#' Generate register font function and execute it
#' 
#' Uses the systemfonts package to register fonts that are not properly
#' parameterized by systemfonts. 
#' 
#' @param ffamily Font family as seen in systemfonts::system_fonts
#' @param fstyle Font style as seen in systemfonts::system_fonts
#' 
#' @author Charlie Gallagher

# This should stay outside the function to avoid getting the fonts over and over
fonts <- systemfonts::system_fonts()


make_register_font <- function(ffamily, fstyle) {
  one_font <- fonts %>% 
    filter(family == ffamily, style == fstyle) %>% 
    mutate(full_family = paste(ffamily, fstyle))
  
  fname <- one_font %>% 
    pull(full_family)
  
  fpath <- one_font %>% 
    pull(path)
  
  fpath <- stringr::str_replace_all(fpath, '\\\\', '/')
  
  
  if (length(fname) != 1) {
    warning('More than one family style combination returned. Only using the first.')
    fname <- fname[1]
  }
  
  # Construct font function
  font_sfunc <- paste0("systemfonts::register_font(name = \"", fname, "\", plain = \"", fpath, '\")')
  
  # print(font_sfunc)
  eval(str2lang(font_sfunc))
}


# Registering a bunch of fonts I use frequently
make_register_font(ffamily = 'Roboto', fstyle = 'Light')
make_register_font(ffamily = 'IBM Plex Sans', fstyle = 'Light')
make_register_font(ffamily = 'IBM Plex Sans Condensed', fstyle = 'Light')
make_register_font('IBM Plex Sans', 'Bold')
make_register_font('Barlow', 'Bold')


# Clean up -------
rm(fonts, make_register_font)
