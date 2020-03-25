<<<<<<< HEAD
cat $1 | awk -F"," '{print "rm Calib.oms;echo "$2" ; echo \"val (areaind,endyear) = ("$1","$2")\" >> Calib.oms; cat MesoCalibrationYear.oms >> Calib.oms; openmole -p density_2.12-1.0.jar --script Calib.oms --password-file omlpsswd"}' | sh
=======
cat $1 | awk -F"," '{print "rm Calib2.oms;echo "$2" ; echo \"val (areaind,endyear) = ("$1","$2")\" >> Calib2.oms; cat MesoCalibrationYear.oms >> Calib2.oms; openmole -p density_2.12-1.0.jar --script Calib2.oms --password-file omlpsswd"}' | sh
>>>>>>> d4c8e38f51019a65778dff2dec987bb5d3664c2f
