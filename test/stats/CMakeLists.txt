#### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    test_mean.fypp
    test_mean_f03.fypp
    test_median.fypp
    test_distribution_uniform.fypp
    test_distribution_normal.fypp
    test_distribution_exponential.fypp
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(corr)
ADDTEST(cov)
ADDTEST(pca)
ADDTEST(mean)
ADDTEST(median)
ADDTEST(moment)
ADDTEST(rawmoment)
ADDTEST(var)
ADDTEST(varn)
ADDTEST(random)
ADDTEST(distribution_uniform)
ADDTEST(distribution_normal)
ADDTEST(distribution_exponential)

if(DEFINED CMAKE_MAXIMUM_RANK)
    if(${CMAKE_MAXIMUM_RANK} GREATER 7)
        ADDTEST(mean_f03)
    endif()
elseif(f03rank)
    ADDTEST(mean_f03)
endif()
