-- !preview conn=con

SELECT dbo.logged_sample.date_rec,   
         dbo.woce_rec_num.rec_num,   
         dbo.woce_loc.collection_date,   
         dbo.woce_rec_num.expocode,   
         dbo.woce_rec_num.whpid,   
         dbo.woce_loc.latitude,   
         dbo.woce_loc.longitude,   
         dbo.woce_rec_num.station,   
         dbo.woce_rec_num.depth_corr,   
         dbo.water_strip.ws_delta_c13,   
         dbo.graphite.gf_dc13,   
         dbo.os.f_modern,   
         dbo.os.f_int_error,   
         dbo.os.f_ext_error,   
         dbo.os.seawater_dc14,   
         dbo.wheel_pos.wheel_id,   
         dbo.water_strip.ws_num,   
         dbo.graphite.osg_num,   
         dbo.target.tp_num,   
         dbo.water_strip.ws_comments,   
         dbo.water_strip.ws_strip_date,   
         dbo.graphite.gf_date,   
         dbo.target.tp_date_pressed,   
         dbo.woce_rec_num.niskin  
    FROM dbo.os,   
         dbo.target,   
         dbo.water_strip,   
         dbo.wheel_pos,   
         dbo.woce_rec_num,   
         dbo.graphite,   
         dbo.logged_sample,   
         dbo.woce_loc  
   WHERE ( dbo.os.tp_num = dbo.target.tp_num ) and  
         ( dbo.target.tp_num = dbo.wheel_pos.tp_num ) and  
         ( dbo.water_strip.rec_num = dbo.woce_rec_num.rec_num ) and  
         ( dbo.water_strip.ws_num = dbo.graphite.ws_num ) and  
         ( dbo.graphite.osg_num = dbo.target.osg_num ) and  
         ( dbo.woce_rec_num.rec_num = dbo.logged_sample.rec_num ) and  
         ( dbo.woce_rec_num.expocode = dbo.woce_loc.expocode ) and  
         ( dbo.woce_rec_num.station = dbo.woce_loc.station ) and  
         ( dbo.woce_rec_num.cast = dbo.woce_loc.cast ) and  
         ( dbo.woce_rec_num.whpid = dbo.woce_loc.whpid ) and  
         ( ( dbo.woce_rec_num.whpid like "%P15S%" ) AND  
         ( dbo.woce_rec_num.rec_num >= 1)  AND  
         ( dbo.target.graphite_lab = 1 )  AND  
         ( dbo.target.tp_date_pressed >= "1/1/2000" ) )   
ORDER BY dbo.woce_rec_num.rec_num ASC   

