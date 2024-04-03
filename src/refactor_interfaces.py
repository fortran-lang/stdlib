
import re
import copy
from platform import os

def refactor_interfaces(file_name,interface_module):
    
    # Parse whole file
    file_body = []
    comment_block = False
    comment_body = []
    is_sub = False
    is_fun = False
    is_interface = False

    # FiLoad whole file; split by lines; join concatenation lines
    with open(os.path.join(file_name), 'r') as file:
        # Create an empty list to store the lines

        # Iterate over the lines of the file
        for line in file:
            
            lsl = line.strip()
            
            is_comment   = lsl.startswith('!>')            
            if not interface_module:
                is_sub = bool(re.match(r'(?:.)*subroutine\s+stdlib_(\w+)',line))
                is_fun = bool(re.match(r'(?:.)*function stdlib_(\w+)',line))
                      

                         
            else:
                is_interface = lsl.startswith('interface') 
            
            if is_comment:
                # Start saving this new comment block
                if not comment_block: comment_body = []
                
                
                # At the beginnging of a comment block, do not include empty comments
                if lsl=='!> !' or lsl=='!>':
                    comment_block = False
                    line = ''                                
                else:
                    comment_block = True                
                    comment_body.append(line)
                
            elif is_interface or is_sub or is_fun:
                # Comment is over and we're now at an interface: append interface line, follow
                # documentaion
                file_body.append(line)
                
                if is_interface:
                    interface_name = re.search(r'interface (\w+)',line).group(1)
                elif is_sub:
                    print(line)
                    interface_name = re.search(r'(?:.)*subroutine\s+stdlib_(\w+)',line).group(1)
                elif is_fun:
                    print(line)
                    
                    interface_name = re.search(r'(?:.)*function stdlib_(\w+)',line).group(1)
                    
                axpy = interface_name.strip().upper()
                search_label = r'!> '+axpy+r':\s*'
                           
                if not comment_body is None:
                    for k in range(len(comment_body)):
                        
                        nointerf = re.sub(search_label,r'!> '+axpy+' ',comment_body[k])                        
                        nointerf = re.sub(r'!> ',r'!! ',nointerf)
                        file_body.append(nointerf)
                        
                comment_body = []                                        
                
            else:
                # Regular body: just append line
                file_body.append(line)
                
               
                   
     # print file out       
    fid = open(file_name,"w")

    # Header
    fid.write(''.join(file_body))
    fid.close()
            
            
            
# Run refactor            
refactor_interfaces('stdlib_linalg_blas.fypp',True)
refactor_interfaces('stdlib_linalg_blas_aux.fypp',False)
refactor_interfaces('stdlib_linalg_blas_s.fypp',False)
refactor_interfaces('stdlib_linalg_blas_d.fypp',False)
refactor_interfaces('stdlib_linalg_blas_q.fypp',False)
refactor_interfaces('stdlib_linalg_blas_c.fypp',False)
refactor_interfaces('stdlib_linalg_blas_z.fypp',False)
refactor_interfaces('stdlib_linalg_blas_w.fypp',False)
refactor_interfaces('stdlib_linalg_lapack.fypp',True)
refactor_interfaces('stdlib_linalg_lapack_aux.fypp',False)
refactor_interfaces('stdlib_linalg_lapack_s.fypp',False)
refactor_interfaces('stdlib_linalg_lapack_d.fypp',False)
refactor_interfaces('stdlib_linalg_lapack_q.fypp',False)
refactor_interfaces('stdlib_linalg_lapack_c.fypp',False)
refactor_interfaces('stdlib_linalg_lapack_z.fypp',False)
refactor_interfaces('stdlib_linalg_lapack_w.fypp',False)            
            
            
    
    
    



    
