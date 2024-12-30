import os
import re
from collections import defaultdict

# Directory containing your Fortran files
input_dir = os.path.join(".","legacy") 
output_dir_blas = os.path.join(".","src","blas") 
output_dir_lapack = os.path.join(".","src","lapack") 
# Ensure the output directory exists
os.makedirs(output_dir_blas, exist_ok=True)
os.makedirs(output_dir_lapack, exist_ok=True)

blas_groups = {
    "level1": [ "asum", "casum",  "zasum","axpy", "copy", "dot", "sdot", "dotc", "dotu", 
               "nrm2", "cnrm2", "znrm2", "rot", "drot", "rotg", "rotm", "rotmg", "srot", "scal", "sscal", "dscal", "swap" ],

    "level2_gen": [ "gemv", "ger", "gerc", "geru", "her", "her2", "hemv" ],
    "level2_ban": [ "gbmv", "hbmv" ],
    "level2_sym": [ "symv", "syr", "syr2" ],
    "level2_pac": [ "spmv", "sbmv", "spr", "spr2", "hpmv", "hpr", "hpr2" ],
    "level2_tri": [ "trmv", "tbmv", "tpmv", "trsv", "tbsv", "tpsv" ],

    "level3_gen": [ "gemm", "hemm", "herk", "her2k" ],
    "level3_sym": [ "syrk", "syr2k", "symm" ],
    "level3_tri": [ "trmm", "trsm" ]
}

# Define the LAPACK routine groups
lapack_groups = {
    "auxiliary_parameters": [
        "lamch", "lamc1", "lamc2", "lamc3", "lamc4", "lamc5", "labad", "sum1", "csum1", "zsum1",
        "laqsb"
    ],
    "auxiliary_others": [
        "lsame","lsamen","roundup_lwork","scond","ladiv1","ladiv2", "rot"
    ],

    "blas_like_base": [
        "laset","larnv","laruv","lacpy","lacp2","tfttp","tfttr","tpttf",
        "tpttr","trttf","trttp","lat2","lag2s","lat2s","lag2d","lat2d"
    ],
    "blas_like_mnorm": [
        "lange","langb","langt","lanhs","lanhf","lansf","lanhp","lansp",
        "lanhb","lansb","lanht","lanst","lantr","lantp","lantb","lansy",
        "lanhe",
    ],
    "blas_like_scalar": [
        "isnan","laisnan","ladiv","lapy2","lapy3","larmm"
    ],
    "blas_like_l1": [
        "lacgv","lasrt","lassq","rscl","srscl","drscl"
    ],
    "blas_like_l2": [
        "lascl","la_geamv","la_gbamv","la_heamv","lascl2","larscl2","la_wwaddw",
        "spmv", "spr", "symv", "syr", 
    ],
    "blas_like_l3": [
        "lagtm","lacrm","larcm","hfrk","tfsm", "sfrk"
    ],

    "householder_reflectors": [
        "larf", "larfx", "larfy", "larfb", "larfg", "larfgp", "larft"
    ],
    "givens_jacobi_rot": [
        "lartg","lartgp","lasr","largv","lartv","lar2v","lacrt"
    ],

    "solve_aux": [
        "lacn2", "lacon", "la_lin_berr"
    ],
    "solve_lu": [
        "gesv", "gesvx", "gesvxx",  
        "gbsv", "gbsvx", "gbsvxx",
        "gtsv", "gtsvx"
    ],
    "solve_lu_comp": [
        "gecon", "getrf", "getrf2", "getf2", "getrs", "getri", "gerfs", "gerfsx", "geequ", "geequb", "laqge", "laswp", "getc2", "gesc2", "latdf",  "la_gercond", "la_gerfsx_extended",
        "gbcon", "gbtrf", "gbtf2", "gbtrs", "gbrfs", "gbrfsx", "gbequ", "gbequb", "laqgb", "la_gbrcond", "la_gbrpvgrw", "la_gbrfsx_extended",
        "gtcon", "gttrf", "gttrs", "gtts2", "gtrfs"
    ],
    "solve_chol": [
        "posv", "posvx", "posvxx", 
        "ppsv", "ppsvx", "pfsv",
        "pbsv", "pbsvx", "ptsv", "ptsvx"
    ],
    "solve_chol_comp": [
        "pocon","potrf", "potrf2", "potf2", "pstrf", "pstf2", "potrs", "potri", "porfs", "porfsx", "poequ", "poequb", "laqhe", "la_porcond", "la_porpvgrw", "la_porfsx_extended",
        "ppcon", "pptrf", "pptrs", "pptri", "pprfs", "ppequ", "laqhp",
        "pftrf", "pftrs", "pftri",
        "pbcon", "pbtrf", "pbtf2", "pbtrs", "pbrfs", "pbequ", "laqhb",
        "ptcon", "pttrf", "pttrs", "ptts2", "ptrfs",
        "laqsp"
    ],
    "solve_ldl": [
        "sysv", "sysvx", "sysv_rk", "sysv_rook", "sysvxx", 
        "hesv", "hesvx", "hesv_rk", "hesv_rook", "hesvxx", 
        "spsv", "spsvx", "hpsv", "hpsvx", 
        "sysv_aa", "sysv_aa_2stage",
        "hesv_aa", "hesv_aa_2stage"
    ],
    "solve_ldl_comp": [
        "sycon","sytrf","lasyf","sytf2","sytrs","sytri","syrfs","syrfsx","syequb",
        "syconv","sycon_3","sytri2","sytri2x","sytri_3","sytri_3x","sytrs2","sytrs_3",
        "syswapr","la_hercond","la_herfsx_extended","la_herpvgrw","spcon","sptrf",
    ],
    "solve_ldl_comp2": [
        "sptrs","sptri","sprfs","sycon_rook","sytrf_rook","lasyf_rook","sytf2_rook",
        "sytrs_rook","sytri_rook","sytrf_rk","lasyf_rk","sytf2_rk","syconvf","syconvf_rook",
        "sytrf_aa","lasyf_aa","sytrs_aa","sytrf_aa_2stage","sytrs_aa_2stage",
    ],
    "solve_ldl_comp3": [
        "hecon","hetrf","lahef","hetf2","hetrs","hetri","herfs","herfsx","heequb",
        "hecon_3","hetri2","hetri2x","hetri_3","hetri_3x","hetrs2","hetrs_3","heswapr",
        "hpcon","hptrf","hptrs","hptri",
    ],
    "solve_ldl_comp4": [
        "hprfs","hecon_rook","hetrf_rook","lahef_rook","hetf2_rook","hetrs_rook",
        "hetri_rook","hetrf_rk","lahef_rk","hetf2_rk","hetrf_aa",
        "lahef_aa","hetrs_aa","hetrf_aa_2stage","hetrs_aa_2stage",
        "laqsy"
    ],
    
    "solve_tri_comp": [
        "trcon", "trtrs", 
        "latrs", "latrs3",
        "trtri","trti2","trrfs",
        "lauum","lauu2",
        "tpcon", "tptrs", 
        "latps","tptri","tprfs",
        "tftri",
        "tbcon", "tbtrs",
        "latbs","tbrfs"  
    ],

    "orthogonal_factors_qr": [
        "geqr","gemqr","geqrf","geqr2","ungqr","ung2r","unmqr","unm2r","orgqr","org2r","ormqr",
        "orm2r","geqrt","geqrt2","geqrt3","gemqrt","geqrfp","geqr2p","geqp3","laqp2","laqps","latsqr",
        "ungtsqr","ungtsqr_row","orgtsqr","orgtsqr_row","larfb_gett","lamtsqr","getsqrhrt","unhr_col",
        "orhr_col","launhr_col_getrfnp","laorhr_col_getrfnp","launhr_col_getrfnp2","laorhr_col_getrfnp2",
        "tpqrt","tpqrt2","tpmqrt","tprfb","ggqrf",
        "gerqf","gerq2","ungrq","unmrq","unmr2","ungr2","orgrq","ormrq","ormr2","orgr2","ggrqf"
    ],
    "orthogonal_factors_ql": [
        "gelq","gemlq","gelqf","gelq2","unglq","ungl2","unmlq","unml2","orglq","orgl2","ormlq","orml2",
        "gelqt","gelqt3","gemlqt","laswlq","lamswlq","tplqt","tplqt2","tpmlqt",
        "geqlf","geql2","ungql","unmql","ung2l","unm2l","orgql","ormql","org2l","orm2l","unm22"
    ],
    "orthogonal_factors_rz": [
        "tzrzf", "unmrz", "ormrz", "unmr3", "ormr3", "larz", "larzb", "larzt", "latrz"
    ],
    
    "lsq": [
        "gelss", "gelsy", "gels", "gelst", "gelsd", "getsls"
    ],
    "lsq_constrained": [
        "gglse", "ggglm"
    ],
    "lsq_aux": [
        "laic1", "lals0", "lalsa", "lalsd"
    ],
    
    "cosine_sine": [
        "bbcsd","uncsd","uncsd2by1","unbdb","unbdb1","unbdb2","unbdb3","unbdb4",
        "unbdb5","unbdb6",
    ],
    "cosine_sine2": [
        "orcsd","orcsd2by1","orbdb","orbdb1","orbdb2","orbdb3",
        "orbdb4","orbdb5","orbdb6","lapmr","lapmt"
    ],
    
    "eigv_gen": [
        "geev","geevx","gees","geesx","ggev3","ggev","ggevx","gges3","gges",
        "ggesx","cgedmd","cgedmdq","dgedmd","dgedmdq","sgedmd","sgedmdq","zgedmd",
        "zgedmdq","gebal","gehrd","gehd2","gebak","lahr2","unghr","unmhr","orghr","ormhr",
    ],
    "eigv_gen2": [
        "hseqr","hsein","trevc","trevc3","laln2","trsyl","trsyl3","lasy2",
        "trsna","trexc","trsen","laexc","lanv2","laein",
    ],
    "eigv_gen3": [
        "laqtr","lahqr","laqr0","laqr1","laqr2","laqr3","laqr4","laqr5",
        "laqz0","laqz1","laqz2","laqz3","laqz4","iparmq",
    ],
    "eigv_comp": [
        "ggbal","gghrd","gghd3","hgeqz","ggbak",
    ],
    "eigv_comp2": [
        "tgsen","tgsna","tgsyl","tgsy2",
        "lagv2","tgevc","tgexc","tgex2",
    ],
    "eigv_sym_comp": [
        "sygst","sygs2","spgst","sbgst","hegst","hegs2","hpgst","hbgst","pbstf","lag2", "orm22", 
        "disna","latrd","lae2","laesy","laev2","lagtf","lagts","sptrd","opgtr","opmtr","sbtrd","hptrd",
        "upgtr","upmtr","hbtrd"
    ],
    "eigv_sym": [
        "sygv","sygv_2stage","sygvd","sygvx","spgv","spgvd","spgvx","sbgv",
        "sbgvd","sbgvx","sytrd","sytd2","orgtr","ormtr","sytrd_2stage","sytrd_he2hb",
        "sytrd_hb2st","sb2st_kernels","hegv","hegv_2stage","hegvd","hegvx","hpgv",
        "hpgvd","hpgvx","hbgv","hbgvd","hbgvx","hetrd","hetd2","ungtr","unmtr",
        "hetrd_2stage","hetrd_he2hb","hetrd_hb2st","hb2st_kernels",
        "sytrd_sb2st", "sytrd_sy2sb",
    ],
    "eigv_tridiag": [
        "laebz","laneg","laed0","laed1","laed2","laed3","laed4",
        "laed5","laed6","laed7","laed8","laed9",
    ],
    "eigv_tridiag2": [
        "lamrg","laeda","larra","larrb","larrc","larrd","larre",
        "larrf","larrj","larrk","larrr","larrv","lar1v",
    ],
    "eigv_tridiag3": [
        "stev","stevd","stevr","stevx","pteqr","stebz","sterf",
        "stedc","stegr","stein","stemr","steqr",
    ],
    "eigv_svd_bidiag_dc": [
        "lasd0","lasdt","lasd1","lasd2","lasd3","lasd4","lasd5","lasdq",
        "lasda","lasd6","lasd7","lasd8",
    ],
    "eigv_svd_drivers": [
        "gesvd", "gesvdq", "ggsvd3", 
    ],
    "eigv_svd_drivers2": [
        "gesdd", "gesvdx", "gejsv", "gesvj",
    ],
    "eigv_svd_drivers3": [
        "bdsqr", "bdsdc", "bdsvdx", 
    ],
    "eigv_std_driver": [
        "syev","syevd","syevr","syevx","syev_2stage","syevd_2stage","syevr_2stage",
        "syevx_2stage","spev","spevd","spevx","sbev","sbevd","sbevx","sbev_2stage",
        "sbevd_2stage","sbevx_2stage","heev","heevd","heevr","heevx","heev_2stage",
        "heevd_2stage","heevr_2stage","heevx_2stage","hpev","hpevd","hpevx","hbev",
        "hbevd","hbevx","hbev_2stage","hbevd_2stage","hbevx_2stage",
    ],
    "svd_comp": [ 
        "gebrd", "gebd2", "gbbrd", "gsvj0", "gsvj1","ggsvp3","tgsja", 
        "ungbr","orgbr","unmbr","ormbr",
    ],
    "svd_comp2": [ 
        "labrd", "las2", "lasv2", "lartgs","lags2", "lapll",
    ],
    "svd_bidiag_qr": [ 
        "lasq1","lasq2","lasq3","lasq4","lasq5","lasq6"
    ],

    "others" : [
        "la_syrpvgrw", "la_gerpvgrw", "la_gbrcond_c", "la_gercond_c", "la_hercond_c", 
        "la_syamv", "la_syrcond", "la_syrcond_c", "la_porcond_c"
    ],
}

lapack_subgroups = {
    "base" : {
        "dependencies" : [],
        "members" : ["auxiliary_parameters","auxiliary_others","blas_like_base",
              "blas_like_scalar","blas_like_l1","blas_like_l2","blas_like_l3","blas_like_mnorm",
              "givens_jacobi_rot","householder_reflectors"],
    },
    
    "solve" : {
        "dependencies" : ["base"],
        "members" : ["solve_aux","solve_tri_comp" ,"solve_lu_comp","solve_lu","solve_chol_comp","solve_ldl_comp","solve_ldl_comp2","solve_ldl_comp3","solve_ldl_comp4","solve_chol","solve_ldl"],
    },

    "other" : {
        "dependencies" : ["solve"],
        "members" : ["others"],
    },

    "orthogonal_factors" : {
        "dependencies" : ["base"],
        "members" : ["orthogonal_factors_rz","orthogonal_factors_qr","orthogonal_factors_ql"],
    },

    "eig_svd_lsq" : {
        "dependencies" : ["base","orthogonal_factors","solve"],
        "members" : ["svd_comp","svd_comp2","eigv_sym_comp","eigv_gen","eigv_gen2","eigv_gen3",
                     "eigv_comp","eigv_comp2","eigv_tridiag","eigv_tridiag2","eigv_tridiag3",
                     "eigv_svd_bidiag_dc","eigv_svd_drivers","eigv_svd_drivers2","eigv_svd_drivers3",
                     "eigv_std_driver","svd_bidiag_qr","cosine_sine","cosine_sine2","eigv_sym",
                     "lsq_aux","lsq","lsq_constrained"],
    },
}

only = { 
    "lamch": ", only: zero, one, eps" ,
    "lassq": ", only: zero, half, one, two, tbig, tsml, ssml, sbig" ,
    "lartg": ", only: zero, half, one, czero, safmax, safmin, rtmin, rtmax" ,
    "lascl": ", only: zero, half, one" ,
    "bbcsd": ", only: negone, zero, one, ten, cnegone" ,
}
# Regular expression to match the subroutine definition with precision
# Matches lines like "pure subroutine stdlib_sname" or "pure subroutine stdlib_dname"
subroutine_pattern = re.compile(
    r"^\s*(?:pure\s*)?(?:recursive\s*)?(?:subroutine|(?:integer|real|complex|logical)\(?\w*\)?\s+function|(?:integer|real|complex|logical)\s*\(\s*\$\{\w+\}\$\s*\)\s*function|function)\s+(stdlib_|stdlib\${ii}\$_)([sdcz]|\${ri}\$|\${ci}\$)(\w+)"
)

# Dictionary to store subroutine definitions by name
subroutines = defaultdict(list)

# Step 1: Parse each Fortran file
for filename in ['stdlib_linalg_blas_s.fypp','stdlib_linalg_blas_d.fypp','stdlib_linalg_blas_q.fypp',
                 'stdlib_linalg_blas_c.fypp','stdlib_linalg_blas_z.fypp','stdlib_linalg_blas_w.fypp']:
    with open(os.path.join(input_dir, filename), 'r') as file:
        lines = file.readlines()
        subroutine_buffer = []
        inside_subroutine = False
        current_name = None

        for line in lines:
            # Check if the line starts a subroutine definition
            match = subroutine_pattern.match(line)

            if match:
                line = re.sub(r'\b(function|subroutine)\b', r'module \1', line)
                # Save any previous subroutine
                if subroutine_buffer:
                    subroutines[current_name].extend(subroutine_buffer)
                # Identify the base name (precision + name) without the prefix "stdlib_"
                precision = match.group(match.lastindex-1)
                base_name = match.group(match.lastindex)
                current_name = base_name  # Use base name for grouping

                aaa = ''
                if filename == 'stdlib_linalg_blas_w.fypp':
                    aaa = '#:for ck,ct,ci in CMPLX_KINDS_TYPES\n#:if not ck in ["sp","dp"]\n'
                elif filename == 'stdlib_linalg_blas_q.fypp':
                    aaa = '#:for rk,rt,ri in REAL_KINDS_TYPES\n#:if not rk in ["sp","dp"]\n'
                subroutine_buffer = [aaa,line]  # Start collecting a new subroutine

                
                if filename in ['stdlib_linalg_blas_s.fypp','stdlib_linalg_blas_c.fypp']:
                    subroutine_buffer.append("     use stdlib_blas_constants_sp\n")
                elif filename in ['stdlib_linalg_blas_d.fypp','stdlib_linalg_blas_z.fypp']:
                    subroutine_buffer.append("     use stdlib_blas_constants_dp\n")
                elif filename == 'stdlib_linalg_blas_q.fypp':
                    subroutine_buffer.append("     use stdlib_blas_constants_${rk}$\n")
                elif filename == 'stdlib_linalg_blas_w.fypp':
                    subroutine_buffer.append("     use stdlib_blas_constants_${ck}$\n")
                inside_subroutine = True
                
            elif inside_subroutine:
                subroutine_buffer.append(line)
                if line.strip().lower().startswith("end subroutine stdlib_") or line.strip().lower().startswith("end function stdlib_") or line.strip().lower().startswith(r"end subroutine stdlib${ii}$_") or line.strip().lower().startswith(r"end function stdlib${ii}$_"):
                    # End of the current subroutine
                    if filename in ['stdlib_linalg_blas_q.fypp','stdlib_linalg_blas_w.fypp'] :
                        subroutine_buffer.append('\n#:endif\n#:endfor\n')
                    subroutine_buffer.append('\n')
                    subroutines[current_name].extend(subroutine_buffer)
                    subroutine_buffer = []
                    inside_subroutine = False
                    current_name = None

##########################################################################
### BLAS
##########################################################################

# Step 2: write header module
blas_include = os.path.join(output_dir_blas, f"stdlib_blas.fypp")

with open(blas_include, 'w') as file:
    file.write("#:include \"common.fypp\" \n")
    file.write(f"module stdlib_blas\n")
    file.write("  use stdlib_linalg_constants\n")
    file.write("  use stdlib_linalg_blas_aux\n")
    file.write("  implicit none(type,external)\n")
    for group, group_list in blas_groups.items():
        for name in group_list:
            procedures = subroutines[name]
            file.write("\n")
            file.write("interface \n")
            file.write("#:for ik,it,ii in LINALG_INT_KINDS_TYPES\n")
            wr = True
            for line in procedures:
                # Switch off writting
                if '! =====================================================================' in line:
                    wr = False
                # Switch on again
                if line.strip().lower().startswith("end subroutine stdlib_") or line.strip().lower().startswith("end function stdlib_") or line.strip().lower().startswith(r"end subroutine stdlib${ii}$_") or line.strip().lower().startswith(r"end function stdlib${ii}$_"):
                    wr = True
                if wr: 
                    if not line.strip().startswith("!"):
                        file.write(line)
            file.write(f"#:endfor\n")
            file.write("end interface \n")
            file.write("\n")
    file.write(f"end module stdlib_blas\n")

# Step 3: Write grouped subroutines to a new submodule file
for group, group_list in blas_groups.items():
    output_file = os.path.join(output_dir_blas, f"stdlib_blas_{group}.fypp")
    with open(output_file, 'w') as file:
        # Write module header
        file.write("#:include \"common.fypp\" \n")
        file.write(f"submodule(stdlib_blas) stdlib_blas_{group}\n")
        file.write("  implicit none(type,external)\n")
        file.write("\n\n")
        file.write("  contains\n")
        file.write("#:for ik,it,ii in LINALG_INT_KINDS_TYPES\n")
        for name in group_list:
            procedures = subroutines[name]
            file.write("\n")
            file.writelines(procedures)
            file.write("\n")
        file.write(f"#:endfor\n")
        # Write module footer
        file.write(f"end submodule stdlib_blas_{group}\n")

##########################################################################
### LAPACK
##########################################################################

subroutines = defaultdict(list)
# Step 3: Parse each Fortran file
for filename in ['stdlib_linalg_lapack_s.fypp','stdlib_linalg_lapack_d.fypp','stdlib_linalg_lapack_q.fypp',
                 'stdlib_linalg_lapack_c.fypp','stdlib_linalg_lapack_z.fypp','stdlib_linalg_lapack_w.fypp']:
    with open(os.path.join(input_dir, filename), 'r') as file:
        # lines = file.readlines()
        subroutine_buffer = []
        inside_subroutine = False
        current_name = None

        while line := file.readline():
            # Check if the line starts a subroutine definition
            match = subroutine_pattern.match(line)
            
            if match:
                line = re.sub(r'\b(function|subroutine)\b', r'module \1', line)
                # Save any previous subroutine
                if subroutine_buffer:
                    subroutines[current_name].extend(subroutine_buffer)

                # Identify the base name (precision + name) without the prefix "stdlib_"
                precision = match.group(match.lastindex-1)
                base_name = match.group(match.lastindex)
                current_name = base_name  # Use base name for grouping

                aaa = ''
                if filename == 'stdlib_linalg_lapack_w.fypp':
                    aaa = '#:for ck,ct,ci in CMPLX_KINDS_TYPES\n#:if not ck in ["sp","dp"]\n'
                elif filename == 'stdlib_linalg_lapack_q.fypp':
                    aaa = '#:for rk,rt,ri in REAL_KINDS_TYPES\n#:if not rk in ["sp","dp"]\n'
                subroutine_buffer = [aaa,line]  # Start collecting a new subroutine

                inside_subroutine = True
                
            elif inside_subroutine:
                if "intrinsic ::" in line:
                    continue
                if "! Scalar Arguments" in line or "! function arguments" in line or "! arguments" in line: # add use stdlib_blas_contants before variables declaration
                    aaa = ", only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone"
                    if base_name in only:
                        aaa = only[base_name]

                    if filename in ['stdlib_linalg_lapack_s.fypp','stdlib_linalg_lapack_c.fypp']:
                        subroutine_buffer.append("           use stdlib_blas_constants_sp"+aaa+"\n")
                    elif filename in ['stdlib_linalg_lapack_d.fypp','stdlib_linalg_lapack_z.fypp']:
                        subroutine_buffer.append("           use stdlib_blas_constants_dp"+aaa+"\n")
                    elif filename == 'stdlib_linalg_lapack_q.fypp':
                        subroutine_buffer.append("           use stdlib_blas_constants_${rk}$"+aaa+"\n")
                    elif filename == 'stdlib_linalg_lapack_w.fypp':
                        subroutine_buffer.append("           use stdlib_blas_constants_${ck}$"+aaa+"\n")

                subroutine_buffer.append(line)
                if line.strip().lower().startswith("end subroutine stdlib_") or line.strip().lower().startswith("end function stdlib_") or line.strip().lower().startswith(r"end subroutine stdlib${ii}$_") or line.strip().lower().startswith(r"end function stdlib${ii}$_"):
                    # End of the current subroutine
                    if filename in ['stdlib_linalg_lapack_q.fypp','stdlib_linalg_lapack_w.fypp'] :
                        subroutine_buffer.append('\n#:endif\n#:endfor\n')
                    subroutine_buffer.append('\n')
                    subroutines[current_name].extend(subroutine_buffer)
                    subroutine_buffer = []
                    inside_subroutine = False
                    current_name = None
                    
# Step 4: write header modules
for subgroup_name, subgroup_data in lapack_subgroups.items():
    dependencies = subgroup_data["dependencies"]
    members = subgroup_data["members"]

    lapack_group = os.path.join(output_dir_lapack, f"stdlib_lapack_{subgroup_name}.fypp")
    
    with open(lapack_group, 'w') as file:
        file.write("#:include \"common.fypp\" \n")
        file.write(f"module stdlib_lapack_{subgroup_name}\n")
        file.write("  use stdlib_linalg_constants\n")
        file.write("  use stdlib_linalg_lapack_aux\n")
        file.write("  use stdlib_linalg_blas\n")
        if dependencies:
            for depen in dependencies:
                file.write(f"  use stdlib_lapack_{depen}\n")
        file.write("  implicit none(type,external)\n")
        for mbr in members:
            for name in lapack_groups[mbr]:
                procedures = subroutines[name]
                file.write("\n")
                file.write("interface \n")
                file.write("#:for ik,it,ii in LINALG_INT_KINDS_TYPES\n")
                wr = True
                for line in procedures:
                    # Switch off writting
                    if '! ==============' in line:
                        wr = False
                    # Switch on again
                    if line.strip().lower().startswith("end subroutine stdlib_") or line.strip().lower().startswith("end function stdlib_") or line.strip().lower().startswith(r"end subroutine stdlib${ii}$_") or line.strip().lower().startswith(r"end function stdlib${ii}$_"):
                        wr = True
                    if wr: 
                        if not line.strip().startswith("!"):
                            file.write(line)
                file.write(f"#:endfor\n")
                file.write("end interface \n")
                file.write("\n")
        file.write(f"end module stdlib_lapack_{subgroup_name}\n")

# Step 5: Write grouped subroutines to a new submodule file
for subgroup_name, subgroup_data in lapack_subgroups.items():
    for group in subgroup_data["members"]:
        output_file = os.path.join(output_dir_lapack, f"stdlib_lapack_{group}.fypp")
        with open(output_file, 'w') as file:
            # Write module header
            file.write("#:include \"common.fypp\" \n")
            file.write(f"submodule(stdlib_lapack_{subgroup_name}) stdlib_lapack_{group}\n")
            file.write("  implicit none(type,external)\n")
            file.write("\n\n")
            file.write("  contains\n")
            file.write("#:for ik,it,ii in LINALG_INT_KINDS_TYPES\n")
            for name in lapack_groups[group]:
                procedures = subroutines[name]
                file.write("\n")
                file.writelines(procedures)
                file.write("\n")
            file.write(f"#:endfor\n")
            # Write module footer
            file.write(f"end submodule stdlib_lapack_{group}\n")