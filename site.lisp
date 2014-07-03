(in-package :eudoxia.www)

(wax-compile :html :eudoxia.www #p"build/"
  #p"index.xml"
  #p"about.xml"
  #p"posts/relativistic-propulsion.xml")
