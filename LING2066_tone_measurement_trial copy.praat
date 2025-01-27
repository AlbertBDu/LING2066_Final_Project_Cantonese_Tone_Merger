# LING2066 Final Project
# Measuring intervals that labelled with M and N, for every ten percent into each segment
# Export csv files with speaker, target words, target tone, and pitch at every time point
# Albert Baichen Du 16 April 2021

# This script still has significant shortcomings: It cannot find the target words where our target syllables are in.

# Select speaker
form Settings
    sentence speaker 游蕙祯
	positive target_tier 3
	positive word_tier 1
	#sentence targer_segment_label B
	#sentence target_segment_label_2
	#positive phoneme_tier 2
	sentence gender F

	comment Number of points to measure during tone :
    natural measurement_points 20 (= Every 5%)



endform

if gender$ == "F"

	pitch_floor = 100

elif gender$ == "M"

	pitch_floor = 75

endif
#if gender$ = "M"
    #Pitch settings: 75, 300, "Hertz", "autocorrelation", "automatic"
#else
    #Pitch settings: 100, 500, "Hertz", "autocorrelation", "automatic"
#endif

output_file$ = "result_" + speaker$ + ".csv"

wav = Open long sound file: "CityForum_" + speaker$ + ".wav"
tgfile$ = "CityForum_" + speaker$ + ".TextGrid"

selectObject: wav
#sample_rate = Get sampling frequency

#if fileReadable (tgfile$) = 1
	tg = Read from file: tgfile$
	nInt = Get number of intervals: target_tier
	#nWord = Get number of intervals: word_tier

	#rep = 0

	for i to nInt
	     selectObject: tg
	     target_syllable$ = Get label of interval: target_tier, i


	     #target_word$ = Get label of interval: word_tier, i
	     


	     if target_syllable$ = "B"

	         

	         syllable_start = Get start time of interval: target_tier, i
	         syllable_end = Get end time of interval: target_tier, i

	         duration = syllable_end - syllable_start
	         mid_time = syllable_start + 0.5 * duration

	         target_word_number = Get interval at time: word_tier, mid_time
	         target_word$ = Get label of interval: word_tier, target_word_number
	         appendInfoLine: target_word$


	         tg_chunk = Extract part: syllable_start, syllable_end, "yes"


	         selectObject: wav

	         wav_chunck = Extract part: syllable_start, syllable_end, "yes"

	         selectObject: wav_chunck


	         

	         pitch_chunk = To Pitch: 0, pitch_floor, 600
	         selectObject: pitch_chunk

	         pitches$ = ""

	             for t from 0 to measurement_points



	                 percent = t * (1/measurement_points)
	                 time_percent = syllable_start + (percent * duration)
	                 #nCell = 0.5 * i

	                 pitch_syllable = Get value at time: time_percent, "Hertz", "Linear"

	                   #if pitch_syllable$ == "--undefined--"




	                 pitches$ = pitches$ + string$(pitch_syllable) + ","


	                 #pitch$ = pitch$ + string$(pitch_syllable) + ","

	                 #target_word$ = Get label of interval: word_tier, i




	                 
	             endfor


	             appendFile: output_file$, speaker$, ",", target_syllable$, ",", target_word$, ","
	             appendFile: output_file$, pitches$
	             appendFile: output_file$, newline$
	                 
	           removeObject: wav_chunck
	           removeObject: tg_chunk
	           removeObject: pitch_chunk

	         



	         

	         
	            


	         endif


	         
	       

	     else
	        appendInfoLine: ""



	           #target_word$ = Get label of interval: 1, i
	           #removeObject: wav_chunck
	           #removeObject: tg_chunk
	           #removeObject: pitch_chunk


	           

	     endif


	endfor

appendInfoLine: "Finished! Press Command+W to quit."
#endif

















