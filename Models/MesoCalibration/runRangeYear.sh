cat $1 | awk -F"," '{print "rm Calib.oms;echo "$2" ; echo \"val (areaind,endyear) = ("$1","$2")\" >> Calib.oms; cat MesoCalibrationYear.oms >> Calib.oms; /home/juste/openmole-9.2/openmole -p density_2.12-1.0.jar --script Calib.oms --password-file omlpsswd"}' | sh
