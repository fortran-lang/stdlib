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
