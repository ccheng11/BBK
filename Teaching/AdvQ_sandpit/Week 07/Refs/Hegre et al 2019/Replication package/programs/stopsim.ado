program stopsim
	version 12
    syntax, error(string) logfile(string)
	
	display "`error'"
	display "PRIOsim log file shown in viewer."
	capture view "`logfile'.txt"
	assert 1==0
end
