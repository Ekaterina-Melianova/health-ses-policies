   
    if (hlm){ # if hlm -> to long format
      
      dt1 = ToLong(dt1)
      
      if (aggregate == 'X'){
        dt1[, (grep("^S", names(dt1))) := lapply(.SD, mean), 
            by = .(Group, .id, sample_index, time), .SDcols = grep("^S", names(dt1))]
        
      } else if (aggregate == 'XY'){
        dt1[, (grep("^S|^H", names(dt1))) := lapply(.SD, mean), 
            by = .(Group, .id, sample_index, time), .SDcols = grep("^S|^H", names(dt1))]
        
        #dt1 = dt1[, .SD[1], by = .(Group, .id, sample_index, time)]
      }
    } else{
      
      if (aggregate == 'X'){
        dt1[, (grep("^S", names(dt1))) := lapply(.SD, mean), 
            by = .(Group, .id, sample_index), .SDcols = grep("^S", names(dt1))]
        
      } else if (aggregate == 'XY'){
        dt1[, (grep("^S|^H", names(dt1))) := lapply(.SD, mean), 
            by = .(Group, .id, sample_index), .SDcols = grep("^S|^H", names(dt1))]
        
        #dt1 = dt1[, .SD[1], by = .(Group, .id, sample_index)]
      }
    }