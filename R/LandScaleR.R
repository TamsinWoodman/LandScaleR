# Show start-up message
.onAttach <- function(libname, 
                      pkgname) {
  
  packageStartupMessage("LandScaleR version 1.1.0\n",
                        "Copyright (C) 2021-2023 Tamsin Woodman\n",
                        "This program comes with ABSOLUTELY NO WARRANTY.\n",
                        "This is free software, and you are welcome to redistribute it and/or modify it under certain conditions;\n",
                        "type 'LandScaleR_license' for details.")
}

#' Print the LandScaleR license
#' @export
LandScaleR_license <- function() {
  
  cat("\nLandScaleR version 1.1.0\n")
  cat("Copyright (C) 2021-2023 Tamsin Woodman\n")
  cat("This program is free software: you can redistribute it and/or modify\n")
  cat("it under the terms of the GNU General Public License as published by\n")
  cat("the Free Software Foundation, either version 3 of the License, or\n")
  cat("(at your option) any later version.\n")
  cat("This program is distributed in the hope that it will be useful,\n")
  cat("but WITHOUT ANY WARRANTY; without even the implied warranty of\n")
  cat("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n")
  cat("GNU General Public License for more details.\n")
  cat("You should have received a copy of the GNU General Public License\n")
  cat("along with this program.  If not, see <https://www.gnu.org/licenses/>.")
  
}
