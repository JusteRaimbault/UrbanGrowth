cat $1 | awk '{print "rm Calib.oms; echo \"val areaind="$1"\" >> Calib.oms; cat MesoCalibration.oms >> Calib.oms; openmole -p density_2.12-1.0.jar --script Calib.oms --password-file omlpsswd"}'| sh
