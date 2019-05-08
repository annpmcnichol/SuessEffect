-- !preview conn=con

SELECT logged_sample.date_rec,   
		woce_rec_num.rec_num,   
		woce_loc.collection_date,   
		woce_rec_num.expocode,   
		woce_rec_num.whpid,   
		woce_loc.latitude,   
		woce_loc.longitude,   
		woce_rec_num.station,   
		woce_rec_num.depth_corr,   
		water_strip.ws_delta_c13,   
		graphite.gf_dc13,   
		os.f_modern,   
		os.f_int_error,   
		os.f_ext_error,   
		os.seawater_dc14,   
		wheel_pos.wheel_id,   
		water_strip.ws_num,   
		graphite.osg_num,   
		target.tp_num,   
		water_strip.ws_comments,   
		water_strip.ws_strip_date,   
		graphite.gf_date,   
		target.tp_date_pressed,   
		woce_rec_num.niskin  
	FROM os   
		JOIN target ON os.tp_num = target.tp_num
		JOIN graphite ON graphite.osg_num = target.osg_num 
		JOIN water_strip ON water_strip.ws_num = graphite.ws_num  
		JOIN wheel_pos ON target.tp_num = wheel_pos.tp_num  
		JOIN woce_rec_num ON water_strip.rec_num = woce_rec_num.rec_num 
		JOIN logged_sample ON woce_rec_num.rec_num = logged_sample.rec_num  
		JOIN woce_loc ON woce_rec_num.expocode = woce_loc.expocode
			AND woce_rec_num.station = woce_loc.station
			AND woce_rec_num.cast = woce_loc.cast
			AND woce_rec_num.whpid = woce_loc.whpid
	WHERE woce_rec_num.whpid like "%P15S%" 
		AND woce_rec_num.rec_num >= 1
		AND target.graphite_lab = 1 
		AND target.tp_date_pressed >= "1/1/2000"
	ORDER BY woce_rec_num.rec_num ASC   

