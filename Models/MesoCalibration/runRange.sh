cat $1 | awk '{print "rm Calib.oms; echo \"val areaind="$1"\" >> Calib.oms; cat MesoCalibration.oms >> Calib.oms; openmole --script Calib.oms --password-file omlpsswd"}'| sh
