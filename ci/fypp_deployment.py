import os
import shutil
import fypp
import argparse
from joblib import Parallel, delayed

def copy_folder_with_filter(source_folder, destination_folder, filter_list=None, filter_suffix=None):
    # Create destination folder if it doesn't exist
    if not os.path.exists(destination_folder):
        os.makedirs(destination_folder)

    # Iterate over the files and folders in the source folder
    for item in os.listdir(source_folder):
        source_item = os.path.join(source_folder, item)
        destination_item = os.path.join(destination_folder, item)

        # If it's a folder, recursively copy it
        if os.path.isdir(source_item):
            copy_folder_with_filter(source_item, destination_item, filter_list, filter_suffix)
        else:
            # If filter_list is provided, check if the filename is in the list to avoid it
            should_copy = False if filter_list and item in filter_list else True
            should_copy = False if filter_suffix and item.endswith(filter_suffix) else should_copy
            if should_copy:
                shutil.copy2(source_item, destination_item)

def pre_process_toml(args):
    """
    Pre-process the fpm.toml
    """
    from tomlkit import table, dumps
    data = table()
    data.add("name", "stdlib")
    data.add("version", str(args.vmajor)+
                    "."+str(args.vminor)+
                    "."+str(args.vpatch) )
    data.add("license", "MIT")
    data.add("author", "stdlib contributors")
    data.add("maintainer", "@fortran-lang/stdlib")
    data.add("copyright", "2019-2021 stdlib contributors")

    if(args.with_blp):
        build = table()
        build.add("link", ["lapack", "blas"] )
        data.add("build", build)

    dev_dependencies = table()
    dev_dependencies.add("test-drive", {"git" : "https://github.com/fortran-lang/test-drive", 
                                        "tag" : "v0.4.0"})
    data.add("dev-dependencies", dev_dependencies)

    preprocess = table()
    preprocess.add("cpp", {} )
    preprocess['cpp'].add("suffixes", [".F90", ".f90"] )
    preprocess['cpp'].add("macros", ["MAXRANK="+str(args.maxrank)] )
    data.add("preprocess", preprocess)

    if args.destdir == 'stdlib-fpm':
        if not os.path.exists('stdlib-fpm'):
            os.makedirs('stdlib-fpm')
        name = 'stdlib-fpm'+os.sep+'fpm.toml'
    else:
        name = "fpm.toml"
    with open(name, "w") as f:
        f.write(dumps(data))
    return

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
    if args.with_xqp:
        kwd.append("-DWITH_XDP=True")

    optparser = fypp.get_option_parser()
    options, leftover = optparser.parse_args(args=kwd)
    options.includes = ['include']
    # options.line_numbering = True
    tool = fypp.Fypp(options)

    # Check destination folder for preprocessing. if not 'stdlib-fpm', it is assumed to be the root folder.
    in_place = False if args.destdir == 'stdlib-fpm' else True
    src     = 'src'     if in_place else 'stdlib-fpm'+os.sep+'src'
    test    = 'test'    if in_place else 'stdlib-fpm'+os.sep+'test'
    example = 'example' if in_place else 'stdlib-fpm'+os.sep+'example'
    if not in_place:
        copy_folder_with_filter('src', src,
                                filter_list=['CMakeLists.txt',"f18estop.f90"])
        copy_folder_with_filter('test', test,
                                filter_list=['CMakeLists.txt',"test_always_fail.f90",
                                            "test_always_skip.f90","test_hash_functions.f90"],
                                filter_suffix='manual')
        copy_folder_with_filter('example',example,
                                filter_list=['CMakeLists.txt'])
        shutil.copy2('ci'+os.sep+'fpm.toml', 'stdlib-fpm'+os.sep+'fpm.toml')
        shutil.copy2('VERSION', 'stdlib-fpm'+os.sep+'VERSION')
        shutil.copy2('LICENSE', 'stdlib-fpm'+os.sep+'LICENSE')

    # Define the folders to search for *.fypp files
    folders = [src,test]
    # Process all folders
    fypp_files = [os.path.join(root, file) for folder in folders
                  for root, _, files in os.walk(folder)
                  for file in files if file.endswith(".fypp")]
    
    def process_f(file):
        source_file = file
        root = os.path.dirname(file)
        basename = os.path.splitext(os.path.basename(source_file))[0]
        sfx = 'f90' if basename not in C_PREPROCESSED else 'F90'
        target_file = root + os.sep + basename + '.' + sfx
        tool.process_file(source_file, target_file)
        # if folder different from root
        if not in_place:
            os.remove(source_file)
    
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
    os.chdir(args.destdir)
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
    parser.add_argument("--with_xqp",type=bool, default=False, help="Include WITH_XDP in the command")

    parser.add_argument('--destdir', action='store', type=str, default='stdlib-fpm', help='destination directory for the fypp preprocessing.')
    # external libraries arguments
    parser.add_argument("--with_blp",type=bool, default=False, help="Link against OpenBLAS")
    parser.add_argument("--build",type=bool, default=False, help="Build the project")

    args, unknown = parser.parse_known_args()
    #==========================================
    # read current manifest
    import tomlkit
    with open('ci'+os.sep+'fpm.toml', 'r') as file:
        manifest = tomlkit.parse(file.read())
    version = manifest['version'].split(".")
    if version != ['VERSION']:
        vmajor, vminor, vpatch = [int(value) for value in version]
    #==========================================
    # pre process the fpm manifest
    #pre_process_toml(args)
    #==========================================
    # pre process the meta programming fypp files
    pre_process_fypp(args)
    #==========================================
    # build using fpm
    if args.build:
        fpm_build(args,unknown)