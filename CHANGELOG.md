# Version 0.7.0

Full release notes available at [v0.7.0] tag.

[v0.7.0]: https://github.com/fortran-lang/stdlib/releases/tag/v0.7.0

- new module `stdlib_constants`
  [#800](https://github.com/fortran-lang/stdlib/pull/800)
  - Many mathematical constants and most common physical ([codata](https://codata.org)) constants

Changes to existing scripts and modules
  - changes in CI
    - Use of `fortran-setup` for GCC, Intel LLVM and Intel Classic
      [#834](https://github.com/fortran-lang/stdlib/pull/834)
  - change in module `stdlib_hashmaps`
    - Support of hash map key generic interfaces
      [#827](https://github.com/fortran-lang/stdlib/pull/827)
  - changes in module `stdlib_io`
    - Addition of a Fortran format specifier in `loadtxt`
      [#805](https://github.com/fortran-lang/stdlib/pull/805)
  - changes in module `stdlib_linalg`
    - Support of extended and quad precision checking 
      [#821](https://github.com/fortran-lang/stdlib/pull/821)
    - Several fixes
      [#815](https://github.com/fortran-lang/stdlib/pull/815)
      [#818](https://github.com/fortran-lang/stdlib/pull/818)
      [#826](https://github.com/fortran-lang/stdlib/pull/826)
      [#830](https://github.com/fortran-lang/stdlib/pull/830)
      [#836](https://github.com/fortran-lang/stdlib/pull/836)
    - New procedures for Eigenvalues and Eigenvectors computation: `eig`, `eigh`, `eigvals`, `eigvalsh`
      [#816](https://github.com/fortran-lang/stdlib/pull/816)
    - New procedures for Singular Value Decomposition: `svd`, `svdvals`
      [#808](https://github.com/fortran-lang/stdlib/pull/808)
  - changes in module `stdlib_sorting`
    - Renamed variable from `int_size` to `int_index`
      [#824](https://github.com/fortran-lang/stdlib/pull/824)
    - Support of `int32` `index` array in `sort_index`
      [#829](https://github.com/fortran-lang/stdlib/pull/829)


# Version 0.6.1

Full release notes available at [v0.6.1] tag.

[v0.6.1]: https://github.com/fortran-lang/stdlib/releases/tag/v0.6.1

Changes to existing scripts and modules
  - changes in module `stdlib_linalg_lapack`
    - Renamed variable for compiler compliance
      [#812](https://github.com/fortran-lang/stdlib/pull/812)
  - change of the format in some example programs
    [#813](https://github.com/fortran-lang/stdlib/pull/813)


# Version 0.6.0

Full release notes available at [v0.6.0] tag.

[v0.6.0]: https://github.com/fortran-lang/stdlib/releases/tag/v0.6.0


- new script `fypp_deployment.py` to support `fpm` in combination with `fypp`
  files
  [#802](https://github.com/fortran-lang/stdlib/pull/802)


Changes to existing scripts and modules
  - change in module `stdlib_hashmap_wrappers`
    - addition of `int32` hashmap key type
      [#778](https://github.com/fortran-lang/stdlib/pull/778)
  - changes in module `stdlib_linalg`
    - addition of the procedure `det` to compute determinants
      [#798](https://github.com/fortran-lang/stdlib/pull/798)
    - addition of the procedures `lstsq` and `lstsq_space`
      [#801](https://github.com/fortran-lang/stdlib/pull/801)
      [#809](https://github.com/fortran-lang/stdlib/pull/809)
    - addition of the procedures `solve` and `solve_lu`
      [#806](https://github.com/fortran-lang/stdlib/pull/806)
  - change in module `stdlib_linalg_blas`
    - addition of the documentation for `rotm` and `rotmg`
      [#795](https://github.com/fortran-lang/stdlib/pull/795)
  - use of macOS 12 in macOS CI
    [#807](https://github.com/fortran-lang/stdlib/pull/807)


Changes to existing documentation
  - Improvement of the documentation `linalg`
    [#797](https://github.com/fortran-lang/stdlib/pull/797)


# Version 0.5.0

Full release notes available at [v0.5.0] tag.

[v0.5.0]: https://github.com/fortran-lang/stdlib/releases/tag/v0.5.0

- new module `stdlib_linalg_state`
  [#774](https://github.com/fortran-lang/stdlib/pull/774)
  - new derived type: `linalg_state_type`
  - new procedure: `linalg_error_handling`

Changes to existing scripts and modules
- addition of `implicit none` to all example programs
  [#780](https://github.com/fortran-lang/stdlib/pull/780)
- change in module `stdlib_hashmaps`
  - fix the procedure `remove_chaining_entry`
    [#788](https://github.com/fortran-lang/stdlib/pull/788)
- change in module `stdlib_linalg`
  - addition of the BLAS/LAPACK backends and interfaces
    [#772](https://github.com/fortran-lang/stdlib/pull/772)
- change in module `stdlib_str2num`
  - fix the procedure `to_${k1}$_from_stream`
    [#789](https://github.com/fortran-lang/stdlib/pull/789)
- upgrade of the Intel-classic compiler in macOS CI 
  [#777](https://github.com/fortran-lang/stdlib/pull/777)

Changes to existing documentation
  - Improvement of the documentation
    [#784](https://github.com/fortran-lang/stdlib/pull/784)
    [#781](https://github.com/fortran-lang/stdlib/pull/781)
    [#786](https://github.com/fortran-lang/stdlib/pull/786)
  - Improvement of the support of `fpm`
    [#787](https://github.com/fortran-lang/stdlib/pull/787)
    [#790](https://github.com/fortran-lang/stdlib/pull/790)

# Version 0.4.0

Full release notes available at [v0.4.0] tag.

[v0.4.0]: https://github.com/fortran-lang/stdlib/releases/tag/v0.4.0

- new module `stdlib_str2num`
  [#743](https://github.com/fortran-lang/stdlib/pull/743)
  - new procedures: `to_num`, `to_num_from_stream`

Changes to existing scripts and modules

- change in .gitignore
  - addition of the file extensions `.dat` and `.stream`
    [#768](https://github.com/fortran-lang/stdlib/pull/768)
  - addition of `.gitignore` to `stdlib-fpm`
    [#769](https://github.com/fortran-lang/stdlib/pull/769)
- change in CI/CD
  - support of GCC 13
    [#737](https://github.com/fortran-lang/stdlib/pull/737)
  - support of Intel compiler `ifx`
    [#752](https://github.com/fortran-lang/stdlib/pull/752)
- change in script `fpm-deployment.sh`
  - changes to facilitate `fypp` preprocessing for the `fpm` deployment
    [#758](https://github.com/fortran-lang/stdlib/pull/758)
- change in module `stdlib_ascii`
  - Improved procedures `to_lower` and `to_upper`
    [#733](https://github.com/fortran-lang/stdlib/pull/733)
- change in module `stdlib_bitsets`
  - initialization in `bitset_type`
    [#753](https://github.com/fortran-lang/stdlib/pull/753)
  - improved procedure `bit_count_large`
    [#756](https://github.com/fortran-lang/stdlib/pull/756)
- change in module `stdlib_hashmaps`
  - new procedure `get_all_keys`
    [#741](https://github.com/fortran-lang/stdlib/pull/741)
  - new file permissions
    [#762](https://github.com/fortran-lang/stdlib/pull/762)
- change in module `stdlib_math`
  - new procedure `meshgrid`
    [#764](https://github.com/fortran-lang/stdlib/pull/764)
- change in module `stdlib_specialfunctions_gamma`
  - fix procedure `gamma`
    [#730](https://github.com/fortran-lang/stdlib/pull/730)
- change in module `stdlib_string_type`
  - fix procedure `move`
    [#736](https://github.com/fortran-lang/stdlib/pull/736)
    [#773](https://github.com/fortran-lang/stdlib/pull/773)
- change in `SpookyV2Test.cpp`
  - Fix undefined use of <cstdint> types
    [#747](https://github.com/fortran-lang/stdlib/pull/747)

Changes to the existing documentation

- change in the specs `stdlib_hashmaps`
  - Correction of an intent of the variable "conflict"
    [#739](https://github.com/fortran-lang/stdlib/pull/739)
- change in README.md
  - instructions to build `stdlib` with `fpm` through the `fpm-deployment.sh` script
    [#757](https://github.com/fortran-lang/stdlib/pull/757)


# Version 0.3.0

Full release notes available at [v0.3.0] tag.

[v0.3.0]: https://github.com/fortran-lang/stdlib/releases/tag/v0.3.0

- new modules `stdlib_hashmap_wrappers` and `stdlib_hashmap`
  [#611](https://github.com/fortran-lang/stdlib/pull/611)
  - new procedures in `stdlib_hashmap_wrappers`: `copy_key`, `copy_other`,
    `fibonacci_hash`, `fnv_1_hasher`, `fnv_1a_hasher`, `free_key`,
    `free_other`, `get`, `hasher_fun`, `operator(==)`, `seeded_nmhash32_hasher`,
    `seeded_nmhash32x_hasher`, `seeded_water_hasher`, `set`, `key_type`,
    `other_type`
  - new procedures in `stdlib_hashmaps`: `chaining_hashmap_type`,
    `hashmap_type`, `open_hashmap_type


Changes to existing scripts and modules

- change in script `doc-deployment.yml`
  - update of the script
    [#681](https://github.com/fortran-lang/stdlib/pull/681)
- change in script `fpm-deployment.sh`
  - fixed a problem with `dat` and `npy` files in example dir not being deployed
    [#713](https://github.com/fortran-lang/stdlib/pull/713)
- change in module `stdlib_bitsets`  
  - remove define assignment for `bitset_64` and `bitset_large`
    [#727](https://github.com/fortran-lang/stdlib/pull/727)
- change in module `stdlib_hashmap_open`
  - fix access violation in a type-bound procedure of `open_hashmap_type`
    [#707](https://github.com/fortran-lang/stdlib/pull/707)
- change in module `stdlib_io_npy_load`
  - fix various bugs
    [#708](https://github.com/fortran-lang/stdlib/pull/708)
    [#711](https://github.com/fortran-lang/stdlib/pull/711)
- change in module `stdlib_linalg`
  - addition of `kronecker_product`
    [#700](https://github.com/fortran-lang/stdlib/pull/700)
- change in module `stdlib_quadrature_gauss`
  - fix erroneous gaussian quadrature points in `gauss_legendre`
    [#660](https://github.com/fortran-lang/stdlib/pull/660)
- change in module `stdlib_sorting`
  - addition of radix sort
    [#712](https://github.com/fortran-lang/stdlib/pull/712)
  - support for sorting arrays of `bitset_64` and of `bitset_large`
    [#723](https://github.com/fortran-lang/stdlib/pull/723)
- change in module `stdlib_stats_distribution_exponential`
  - convert `pdf_exp` and `cdf_exp` to `pure` functions
    [#717](https://github.com/fortran-lang/stdlib/pull/717)
- change in module `stdlib_stats_distribution_normal`
  - convert `rvs_norm` to an `impure elemental` function
    [#665](https://github.com/fortran-lang/stdlib/pull/665)
  - remove unused module `stdlib_error` from module `stdlib_stats_distribution_normal`
    [#716](https://github.com/fortran-lang/stdlib/pull/716)
- remove support for manual make builds
  [#657](https://github.com/fortran-lang/stdlib/pull/657)


Changes to the existing documentation

- change in README.md
    [#656](https://github.com/fortran-lang/stdlib/pull/656)
    [#659](https://github.com/fortran-lang/stdlib/pull/659)
    [#715](https://github.com/fortran-lang/stdlib/pull/715)
    [#725](https://github.com/fortran-lang/stdlib/pull/725)
- change in `stdlib_stats_distribution_normal.md`
  - Improvement of the documentation
    [#718](https://github.com/fortran-lang/stdlib/pull/718)
    [#721](https://github.com/fortran-lang/stdlib/pull/721)
- change in `stdlib_stats_distribution_exponential.md`
  - Improvement of the documentation
    [#721](https://github.com/fortran-lang/stdlib/pull/721)
- change in the structure of the project `stdlib`
  - extraction of the demo programs from the specs in the directory example
    [#662](https://github.com/fortran-lang/stdlib/pull/662)
  - move the directory `src/tests` to `test`
    [#669](https://github.com/fortran-lang/stdlib/pull/669)
- fix various docs
  [#663](https://github.com/fortran-lang/stdlib/pull/663)

# Version 0.2.1

Full release notes available at [v0.2.1] tag.

[v0.2.1]: https://github.com/fortran-lang/stdlib/releases/tag/v0.2.1

- build system related bugfixes


# Version 0.2.0

Full release notes available at [v0.2.0] tag.

[v0.2.0]: https://github.com/fortran-lang/stdlib/releases/tag/v0.2.0

- new module `stdlib_hash_32bit`
  [#573](https://github.com/fortran-lang/stdlib/pull/573)
  - new procedures: `fibonacci_hash`, `fnv_1_hash`, 
    `fnv_1a_hash`, `new_nmhash32_seed`, `new_nmhash32x_seed`, 
    `new_water_hash_seed`, `nmhash32`, `nmhash32x`, `odd_random_integer`,
    `universal_mult_hash`, and `water_hash`
- new module `stdlib_hash_64bit`
  [#573](https://github.com/fortran-lang/stdlib/pull/573)
  - new procedures: `fibonacci_hash`, `fnv_1_hash`, `fnv_1a_hash`,
    `new_pengy_hash_seed`, `new_spooky_hash_seed`,
    `odd_random_integer`, `pengy_hash`, `spooky_hash`, `spookyhash_128`, and
    `universal_mult_hash`
- new module `stdlib_array`
  [#603](https://github.com/fortran-lang/stdlib/pull/603)
  - new procedures `trueloc`, `falseloc`
- new module `stdlib_distribution_uniform`
  [#272](https://github.com/fortran-lang/stdlib/pull/272)
- new module `stdlib_selection`
  [#500](https://github.com/fortran-lang/stdlib/pull/500)
  - new procedures `select`, `arg_select`
- new module `stdlib_version`
  [#579](https://github.com/fortran-lang/stdlib/pull/579)
  - new procedure `get_stdlib_version`
- update module `stdlib_io`
  [597](https://github.com/fortran-lang/stdlib/pull/597)
  - new procedure `getline`
- new module `stdlib_io_npy`
  [#581](https://github.com/fortran-lang/stdlib/pull/581)
  - new procedures `save_npy`, `load_npy`
- update module `stdlib_math`
  - new procedures `is_close` and `all_close`
    [#488](https://github.com/fortran-lang/stdlib/pull/488)
  - new procedures `arg`, `argd` and `argpi`
    [#498](https://github.com/fortran-lang/stdlib/pull/498)
  - new procedure `diff`
    [#605](https://github.com/fortran-lang/stdlib/pull/605)

Changes to existing modules

- change in module `stdlib_math`
  - `linspace` and `logspace` made pure
    [#549](https://github.com/fortran-lang/stdlib/pull/549)
- change in module `stdlib_string_type`
  - `move` procedure made *pure*/*elemental*
    [#562](https://github.com/fortran-lang/stdlib/pull/562)
- support for quadruple precision made optional
  [#565](https://github.com/fortran-lang/stdlib/pull/565)
- change in module `stdlib_io`
  - Modified format constants, and made public
  [#617](https://github.com/fortran-lang/stdlib/pull/617)
- change in module `stdlib_math`
    - Minor update to `stdlib_math` module and document
    [#624](https://github.com/fortran-lang/stdlib/pull/624)


# Version 0.1.0

Full release notes available at [v0.1.0] tag.

[v0.1.0]: https://github.com/fortran-lang/stdlib/releases/tag/v0.1.0

- new module `stdlib_ascii`
  [#32](https://github.com/fortran-lang/stdlib/pull/32)
- new module `stdlib_bitsets`
  [#239](https://github.com/fortran-lang/stdlib/pull/239)
  - new derived types `bitset_64` and `bitset_large`
  - new abstract base class `bitset_type`
- new module `stdlib_error`
  [#53](https://github.com/fortran-lang/stdlib/pull/53)
- new module `stdlib_io`
  - new procedures `loadtxt` and `savetxt`
    [#23](https://github.com/fortran-lang/stdlib/pull/23)
    [#37](https://github.com/fortran-lang/stdlib/pull/37)
  - new procedure `open`
    [#71](https://github.com/fortran-lang/stdlib/pull/71)
    [#77](https://github.com/fortran-lang/stdlib/pull/77)
- new module `stdlib_kinds`
  [#63](https://github.com/fortran-lang/stdlib/pull/63)
- new module `stdlib_linalg`
  - new procedures `diag`, `eye` and `trace`
    [#170](https://github.com/fortran-lang/stdlib/pull/170)
  - new procedure `outer_product`
    [#432](https://github.com/fortran-lang/stdlib/pull/432)
- new module `stdlib_logger`
  - new derived type `logger_type`
    [#228](https://github.com/fortran-lang/stdlib/pull/228)
    [#261](https://github.com/fortran-lang/stdlib/pull/261)
- new module `stdlib_math`
  - new procedure `clip`
    [#355](https://github.com/fortran-lang/stdlib/pull/355)
  - new procedures `linspace` and `logspace`
    [#420](https://github.com/fortran-lang/stdlib/pull/420)
  - new procedure `arange`
    [#480](https://github.com/fortran-lang/stdlib/pull/480)
  - new procedure `gcd`
    [#539](https://github.com/fortran-lang/stdlib/pull/539)
- new module `stdlib_optval`
  [#73](https://github.com/fortran-lang/stdlib/pull/73)
  [#96](https://github.com/fortran-lang/stdlib/pull/96)
  [#139](https://github.com/fortran-lang/stdlib/pull/139)
- new module `stdlib_quadrature`
  - new procedures `trapz`, `trapz_weights`, `simps` and `simps_weights`
    [#146](https://github.com/fortran-lang/stdlib/pull/146)
  - new procedures `gauss_legendre`, `gauss_legendre_lobatto`
    [#313](https://github.com/fortran-lang/stdlib/pull/313)
- new module `stdlib_random`
  [#271](https://github.com/fortran-lang/stdlib/pull/271)
- new module `stdlib_sorting`
  - new procedures `sort`, `ord_sort` and `sort_index`
    [#408](https://github.com/fortran-lang/stdlib/pull/408)
- new module `stdlib_specialfunctions`
  - new procedures `legendre` and `dlegendre`
    [#313](https://github.com/fortran-lang/stdlib/pull/313)
- new module `stdlib_stats`
  - new procedure `mean`
    [#124](https://github.com/fortran-lang/stdlib/pull/124)
    [#130](https://github.com/fortran-lang/stdlib/pull/130)
    [#132](https://github.com/fortran-lang/stdlib/pull/132)
  - new procedure `var`
    [#144](https://github.com/fortran-lang/stdlib/pull/144)
  - new procedure `moment`
    [#153](https://github.com/fortran-lang/stdlib/pull/153)
  - new procedure `corr`
    [#191](https://github.com/fortran-lang/stdlib/pull/191)
  - new procedure `median`
    [#426](https://github.com/fortran-lang/stdlib/pull/426)
- new module `stdlib_string_type`
  - new derived types `string_type`
    [#320](https://github.com/fortran-lang/stdlib/pull/320)
  - new procedure `move`
    [#467](https://github.com/fortran-lang/stdlib/pull/467)
- new module `stdlib_stringlist_type`
  - new derived types `stringlist_type` and `stringlist_index_type`
    [#470](https://github.com/fortran-lang/stdlib/pull/470)
- new module `stdlib_strings`
  - new procedure `to_string`
    [#444](https://github.com/fortran-lang/stdlib/pull/444)
  - new procedures `strip` and `chomp`
    [#343](https://github.com/fortran-lang/stdlib/pull/343)
  - new procedures `starts_with` and `ends_with`
    [#384](https://github.com/fortran-lang/stdlib/pull/384)
  - new procedure `slice`
    [#414](https://github.com/fortran-lang/stdlib/pull/414)
  - new procedure `find`
    [#433](https://github.com/fortran-lang/stdlib/pull/433)
  - new procedure `replace_all`
    [#436](https://github.com/fortran-lang/stdlib/pull/436)
  - new procedures `padl` and `padr`
    [#441](https://github.com/fortran-lang/stdlib/pull/441)
  - new procedure `count`
    [#453](https://github.com/fortran-lang/stdlib/pull/453)
- new module `stdlib_system`
  - new procedure `sleep`
    [#54](https://github.com/fortran-lang/stdlib/pull/54)
