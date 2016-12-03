

simplify_breeds = function(breed_list){
  
  output = breed_list
  
  pit_bull_types = c('pit bull', 
                     'staffordshire bull terrier',
                     'american pit bull terrier',
                     'american bulldog',
                     'american staffordshire terrier',
                     'staffordshire')
  
  chihuahua_types = c('chihuahua shorthair', 
                      'chihuahua longhair',
                      'chihuahua long coat',
                      'chihuahua smooth coat')
  
  dachshund_types = c('dachshund', 
                      'dachshund longhair',
                      'dachshund wirehair',
                      'dachshund smooth haired',
                      'dachshund long haired',
                      'dachshund wire haired',
                      'dachshund miniature smooth haired',
                      'dachshund miniature long haired',
                      'dachshund miniature wire haired')
  
  bulldog_types = c('bulldog', 
                    'english bulldog',
                    'old english bulldog')
  
  output = gsub(paste(pit_bull_types, collapse='|'), 'pit bull', output)
  output = gsub(paste(chihuahua_types, collapse='|'), 'chihuahua', output)
  output = gsub(paste(dachshund_types, collapse='|'), 'dachshund', output)
  output = gsub(paste(bulldog_types, collapse='|'), 'bulldog', output)
  
  return(output)  
}
