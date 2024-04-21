import os
import fypp
import argparse
from joblib import Parallel, delayed

C_PREPROCESSED = (
    "stdlib_linalg_constants" ,
    "stdlib_linalg_blas" ,
    "stdlib_linalg_blas_aux",
    "stdlib_linalg_blas_s",
    "stdlib_linalg_blas_d",
    "stdlib_linalg_blas_q",
    "stdlib_linalg_blas_c",
    "stdlib_linalg_blas_z",
    "stdlib_linalg_blas_w",
    "stdlib_linalg_lapack",
    "stdlib_linalg_lapack_aux",
    "stdlib_linalg_lapack_s",
    "stdlib_linalg_lapack_d",
    "stdlib_linalg_lapack_q",
    "stdlib_linalg_lapack_c",
    "stdlib_linalg_lapack_z",
    "stdlib_linalg_lapack_w"
)

def pre_process_fypp(args):
    kwd = []
    kwd.append("-DMAXRANK="+str(args.maxrank))
    kwd.append("-DPROJECT_VERSION_MAJOR="+str(args.vmajor))
    kwd.append("-DPROJECT_VERSION_MINOR="+str(args.vminor))
    kwd.append("-DPROJECT_VERSION_PATCH="+str(args.vpatch))
    if args.with_qp:
        kwd.append("-DWITH_QP=True")
    if args.with_xdp:
        kwd.append("-DWITH_XDP=True")

    optparser = fypp.get_option_parser()
    options, leftover = optparser.parse_args(args=kwd)
    options.includes = ['include']
    # options.line_numbering = True
    tool = fypp.Fypp(options)

    # Check destination folder for preprocessing. if not 'stdlib-fpm', it is assumed to be the root folder.
    if not os.path.exists('src'+os.sep+'temp'):
        os.makedirs('src'+os.sep+'temp')
    if not os.path.exists('test'+os.sep+'temp'):
        os.makedirs('test'+os.sep+'temp')

    # Define the folders to search for *.fypp files
    folders = ['src','test']
    # Process all folders
    fypp_files = [os.path.join(root, file) for folder in folders
                  for root, _, files in os.walk(folder)
                  for file in files if file.endswith(".fypp")]
    
    def process_f(file):
        source_file = file
        root = os.path.dirname(file)
        if not os.path.exists(root+os.sep+'temp'):
            os.makedirs(root+os.sep+'temp')
        basename = os.path.splitext(os.path.basename(source_file))[0]
        sfx = 'f90' if basename not in C_PREPROCESSED else 'F90'
        target_file = root+os.sep+'temp' + os.sep + basename + '.' + sfx
        tool.process_file(source_file, target_file)
    
    Parallel(n_jobs=args.njob)(delayed(process_f)(f) for f in fypp_files)
    
    return

def fpm_build(args,unknown):
    import subprocess
    #==========================================
    # check compilers
    FPM_FC  = os.environ['FPM_FC']  if "FPM_FC"  in os.environ else "gfortran"
    FPM_CC  = os.environ['FPM_CC']  if "FPM_CC"  in os.environ else "gcc"
    FPM_CXX = os.environ['FPM_CXX'] if "FPM_CXX" in os.environ else "gcc"
    #==========================================
    # Filter out the macro definitions.
    macros = [arg for arg in unknown if arg.startswith("-D")]
    # Filter out the include paths with -I prefix.
    include_paths = [arg for arg in unknown if arg.startswith("-I")]
    # Filter out flags
    flags = "-cpp "
    for idx, arg in enumerate(unknown):
        if arg.startswith("--flag"):
            flags= flags + unknown[idx+1]
    #==========================================
    # build with fpm
    subprocess.run(["fpm build"]+
                   [" --compiler "]+[FPM_FC]+
                   [" --c-compiler "]+[FPM_CC]+
                   [" --cxx-compiler "]+[FPM_CXX]+
                   [" --flag "]+[flags], shell=True, check=True)
    return

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Preprocess stdlib source files.')
    # fypp arguments
    parser.add_argument("--vmajor", type=int, default=0, help="Project Version Major")
    parser.add_argument("--vminor", type=int, default=5, help="Project Version Minor")
    parser.add_argument("--vpatch", type=int, default=0, help="Project Version Patch")

    parser.add_argument("--njob", type=int, default=4, help="Number of parallel jobs for preprocessing")
    parser.add_argument("--maxrank",type=int, default=7, help="Set the maximum allowed rank for arrays")
    parser.add_argument("--with_qp",type=bool, default=False, help="Include WITH_QP in the command")
    parser.add_argument("--with_xdp",type=bool, default=False, help="Include WITH_XDP in the command")

    # external libraries arguments
    parser.add_argument("--build",type=bool, default=False, help="Build the project")

    args, unknown = parser.parse_known_args()
    #==========================================
    # read current manifest
    with open('VERSION', 'r') as file:
        version = file.read().split(".")
        vmajor, vminor, vpatch = [int(value) for value in version]
        args.vmajor = max(vmajor,args.vmajor)
        args.vminor = max(vminor,args.vminor)
        args.vpatch = max(vpatch,args.vpatch)
    #==========================================
    # pre process the meta programming fypp files
    pre_process_fypp(args)
    #==========================================
    # build using fpm
    if args.build:
        fpm_build(args,unknown)