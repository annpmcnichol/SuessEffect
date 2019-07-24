
library(amstools)
library(odbc)
library(here)
query <- "
SELECT client.client_lname,   
        logged_sample.rec_num,   
        logged_sample.cl_id,   
        logged_sample.cl_type,   
        water_desc.description,   
        water_strip.ws_num,   
        water_strip.ws_wt_bottle_full,   
        water_strip.ws_wt_bottle_empty,   
        water_strip.ws_co2_yield,   
        water_strip.ws_delta_c13,   
        graphite.osg_num,   
        graphite.gf_co2_qty,   
        target.tp_num,   
        dc13.dc13,   
        dc13.dc13_sample,   
        no_os.f_modern,   
        no_os.f_int_error,   
        no_os.f_ext_error,   
        no_os.seawater_dc14,   
        wheel_pos.wheel_id,   
        water_strip.ws_strip_date,   
        water_strip.ws_comments,   
        no_os.q_flag,   
        water_strip.ws_line_num  
   FROM client,   
        dc13,   
        graphite,   
        logged_sample,   
        water_desc,   
        water_strip,   
        wheel_pos,   
        target,   
        no_os   
  WHERE ( graphite.ws_num = water_strip.ws_num ) and  
        ( logged_sample.rec_num = water_strip.rec_num ) and  
        ( graphite.osg_num = target.osg_num ) and  
        ( water_desc.ws_num = water_strip.ws_num ) and  
        ( client.client_id = logged_sample.client_id ) and  
        ( dc13.tp_num = target.tp_num ) and  
        ( wheel_pos.tp_num = target.tp_num ) and  
        ( target.tp_num = no_os.tp_num ) and  
        ( target.graphite_lab = 1  )   
ORDER BY logged_sample.rec_num ASC   "

con <- conNOSAMS()
data <- dbGetQuery(con, query)
write.csv(data, here("data/ann_jul_17_nosams_clivar.csv"))