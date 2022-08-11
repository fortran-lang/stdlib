module test_gauss
    use stdlib_kinds, only: dp
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_quadrature , only: gauss_legendre, gauss_legendre_lobatto

    implicit none


contains

    !> Collect all exported unit tests
    subroutine collect_gauss(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("gauss-analytic", test_gauss_analytic), &
            new_unittest("gauss-5", test_gauss_5), &
            new_unittest("gauss-32", test_gauss_32), &
            new_unittest("gauss-64", test_gauss_64), &
            new_unittest("gauss-lobatto-analytic", test_gauss_lobatto_analytic), &
            new_unittest("gauss-lobatto-5", test_gauss_lobatto_5), &
            new_unittest("gauss-lobatto-32", test_gauss_lobatto_32), &
            new_unittest("gauss-lobatto-64", test_gauss_lobatto_64), &
            new_unittest("gauss-github-issue-619", test_fix_github_issue619) &
            ]
    end subroutine

    subroutine test_gauss_analytic(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        real(dp) :: analytic, numeric

        ! test an analytic derivative versus an actual integration
        ! x**2 from -1 to 1
        analytic = 2.0_dp/3.0_dp
        do i=2,6
            block
                real(dp), dimension(i) :: x,w
                call gauss_legendre(x,w)
                numeric = sum(x**2 * w)
                !print *, i, numeric
                call check(error, abs(numeric-analytic) < 2*epsilon(analytic))
                if (allocated(error)) return
            end block
        end do

    end subroutine

    subroutine test_fix_github_issue619(error)
        !> See github issue https://github.com/fortran-lang/stdlib/issues/619
        type(error_type), allocatable, intent(out) :: error
        integer :: i

        ! test the values of nodes and weights
        i = 5
        block 
            real(dp), dimension(i) :: x1,w1,x2,w2
            call gauss_legendre(x1,w1)
            call gauss_legendre(x2,w2,interval=[-1._dp, 1._dp])

            call check(error, all(abs(x1-x2) < 2*epsilon(x1(1))))
            if (allocated(error)) return
            call check(error, all(abs(w1-w2) < 2*epsilon(w1(1))))
        end block

    end subroutine

    subroutine test_gauss_5(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i

        ! test the values of nodes and weights
        i = 5
        block 
            real(dp), dimension(i) :: x,w,xref,wref
            call gauss_legendre(x,w)
            xref(1)=-0.90617984593866399_dp
            xref(2)=-0.53846931010568309_dp
            xref(3)=0.0_dp
            xref(4)=0.53846931010568309_dp
            xref(5)=0.90617984593866399_dp

            wref(1)=0.23692688505618909_dp
            wref(2)=0.47862867049936647_dp
            wref(3)=0.56888888888888889_dp
            wref(4)=0.47862867049936647_dp
            wref(5)=0.23692688505618909_dp

            call check(error, all(abs(x-xref) < 2*epsilon(x(1))))
            if (allocated(error)) return
            call check(error, all(abs(w-wref) < 2*epsilon(w(1))))
        end block

    end subroutine

    subroutine test_gauss_32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i

        i = 32 
        block 
            real(dp), dimension(i) :: x,w,xref,wref
            call gauss_legendre(x,w)

            xref(1)=-0.99726386184948156_dp
            xref(2)=-0.98561151154526834_dp
            xref(3)=-0.96476225558750643_dp
            xref(4)=-0.93490607593773969_dp
            xref(5)=-0.89632115576605212_dp
            xref(6)=-0.84936761373256997_dp
            xref(7)=-0.79448379596794241_dp
            xref(8)=-0.73218211874028968_dp
            xref(9)=-0.66304426693021520_dp
            xref(10)=-0.58771575724076233_dp
            xref(11)=-0.50689990893222939_dp
            xref(12)=-0.42135127613063535_dp
            xref(13)=-0.33186860228212765_dp
            xref(14)=-0.23928736225213707_dp
            xref(15)=-0.14447196158279649_dp
            xref(16)=-0.048307665687738316_dp
            xref(17)=0.048307665687738316_dp
            xref(18)=0.14447196158279649_dp
            xref(19)=0.23928736225213707_dp
            xref(20)=0.33186860228212765_dp
            xref(21)=0.42135127613063535_dp
            xref(22)=0.50689990893222939_dp
            xref(23)=0.58771575724076233_dp
            xref(24)=0.66304426693021520_dp
            xref(25)=0.73218211874028968_dp
            xref(26)=0.79448379596794241_dp
            xref(27)=0.84936761373256997_dp
            xref(28)=0.89632115576605212_dp
            xref(29)=0.93490607593773969_dp
            xref(30)=0.96476225558750643_dp
            xref(31)=0.98561151154526834_dp
            xref(32)=0.99726386184948156_dp


            wref(1)=0.0070186100094700966_dp
            wref(2)=0.016274394730905671_dp
            wref(3)=0.025392065309262059_dp
            wref(4)=0.034273862913021433_dp
            wref(5)=0.042835898022226681_dp
            wref(6)=0.050998059262376176_dp
            wref(7)=0.058684093478535547_dp
            wref(8)=0.065822222776361847_dp
            wref(9)=0.072345794108848506_dp
            wref(10)=0.078193895787070306_dp
            wref(11)=0.083311924226946755_dp
            wref(12)=0.087652093004403811_dp
            wref(13)=0.091173878695763885_dp
            wref(14)=0.093844399080804566_dp
            wref(15)=0.095638720079274859_dp
            wref(16)=0.096540088514727801_dp
            wref(17)=0.096540088514727801_dp
            wref(18)=0.095638720079274859_dp
            wref(19)=0.093844399080804566_dp
            wref(20)=0.091173878695763885_dp
            wref(21)=0.087652093004403811_dp
            wref(22)=0.083311924226946755_dp
            wref(23)=0.078193895787070306_dp
            wref(24)=0.072345794108848506_dp
            wref(25)=0.065822222776361847_dp
            wref(26)=0.058684093478535547_dp
            wref(27)=0.050998059262376176_dp
            wref(28)=0.042835898022226681_dp
            wref(29)=0.034273862913021433_dp
            wref(30)=0.025392065309262059_dp
            wref(31)=0.016274394730905671_dp
            wref(32)=0.0070186100094700966_dp

            call check(error, all(abs(x-xref) < 2*epsilon(x(1))))
            if (allocated(error)) return
            call check(error, all(abs(w-wref) < 2*epsilon(w(1))))
        end block

    end subroutine

    subroutine test_gauss_64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i


        i = 64
        block 
            real(dp), dimension(i) :: x,w,xref,wref
            call gauss_legendre(x,w)


            xref(1)=-0.99930504173577214_dp
            xref(2)=-0.99634011677195528_dp
            xref(3)=-0.99101337147674432_dp
            xref(4)=-0.98333625388462596_dp
            xref(5)=-0.97332682778991096_dp
            xref(6)=-0.96100879965205372_dp
            xref(7)=-0.94641137485840282_dp
            xref(8)=-0.92956917213193958_dp
            xref(9)=-0.91052213707850281_dp
            xref(10)=-0.88931544599511411_dp
            xref(11)=-0.86599939815409282_dp
            xref(12)=-0.84062929625258036_dp
            xref(13)=-0.81326531512279756_dp
            xref(14)=-0.78397235894334141_dp
            xref(15)=-0.75281990726053190_dp
            xref(16)=-0.71988185017161083_dp
            xref(17)=-0.68523631305423324_dp
            xref(18)=-0.64896547125465734_dp
            xref(19)=-0.61115535517239325_dp
            xref(20)=-0.57189564620263403_dp
            xref(21)=-0.53127946401989455_dp
            xref(22)=-0.48940314570705296_dp
            xref(23)=-0.44636601725346409_dp
            xref(24)=-0.40227015796399160_dp
            xref(25)=-0.35722015833766812_dp
            xref(26)=-0.31132287199021096_dp
            xref(27)=-0.26468716220876742_dp
            xref(28)=-0.21742364374000708_dp
            xref(29)=-0.16964442042399282_dp
            xref(30)=-0.12146281929612055_dp
            xref(31)=-0.072993121787799039_dp
            xref(32)=-0.024350292663424433_dp
            xref(33)=0.024350292663424433_dp
            xref(34)=0.072993121787799039_dp
            xref(35)=0.12146281929612055_dp
            xref(36)=0.16964442042399282_dp
            xref(37)=0.21742364374000708_dp
            xref(38)=0.26468716220876742_dp
            xref(39)=0.31132287199021096_dp
            xref(40)=0.35722015833766812_dp
            xref(41)=0.40227015796399160_dp
            xref(42)=0.44636601725346409_dp
            xref(43)=0.48940314570705296_dp
            xref(44)=0.53127946401989455_dp
            xref(45)=0.57189564620263403_dp
            xref(46)=0.61115535517239325_dp
            xref(47)=0.64896547125465734_dp
            xref(48)=0.68523631305423324_dp
            xref(49)=0.71988185017161083_dp
            xref(50)=0.75281990726053190_dp
            xref(51)=0.78397235894334141_dp
            xref(52)=0.81326531512279756_dp
            xref(53)=0.84062929625258036_dp
            xref(54)=0.86599939815409282_dp
            xref(55)=0.88931544599511411_dp
            xref(56)=0.91052213707850281_dp
            xref(57)=0.92956917213193958_dp
            xref(58)=0.94641137485840282_dp
            xref(59)=0.96100879965205372_dp
            xref(60)=0.97332682778991096_dp
            xref(61)=0.98333625388462596_dp
            xref(62)=0.99101337147674432_dp
            xref(63)=0.99634011677195528_dp
            xref(64)=0.99930504173577214_dp


            wref(1)=0.0017832807216964329_dp
            wref(2)=0.0041470332605624676_dp
            wref(3)=0.0065044579689783629_dp
            wref(4)=0.0088467598263639477_dp
            wref(5)=0.011168139460131129_dp
            wref(6)=0.013463047896718643_dp
            wref(7)=0.015726030476024719_dp
            wref(8)=0.017951715775697343_dp
            wref(9)=0.020134823153530209_dp
            wref(10)=0.022270173808383254_dp
            wref(11)=0.024352702568710873_dp
            wref(12)=0.026377469715054659_dp
            wref(13)=0.028339672614259483_dp
            wref(14)=0.030234657072402479_dp
            wref(15)=0.032057928354851554_dp
            wref(16)=0.033805161837141609_dp
            wref(17)=0.035472213256882384_dp
            wref(18)=0.037055128540240046_dp
            wref(19)=0.038550153178615629_dp
            wref(20)=0.039953741132720341_dp
            wref(21)=0.041262563242623529_dp
            wref(22)=0.042473515123653589_dp
            wref(23)=0.043583724529323453_dp
            wref(24)=0.044590558163756563_dp
            wref(25)=0.045491627927418144_dp
            wref(26)=0.046284796581314417_dp
            wref(27)=0.046968182816210017_dp
            wref(28)=0.047540165714830309_dp
            wref(29)=0.047999388596458308_dp
            wref(30)=0.048344762234802957_dp
            wref(31)=0.048575467441503427_dp
            wref(32)=0.048690957009139720_dp
            wref(33)=0.048690957009139720_dp
            wref(34)=0.048575467441503427_dp
            wref(35)=0.048344762234802957_dp
            wref(36)=0.047999388596458308_dp
            wref(37)=0.047540165714830309_dp
            wref(38)=0.046968182816210017_dp
            wref(39)=0.046284796581314417_dp
            wref(40)=0.045491627927418144_dp
            wref(41)=0.044590558163756563_dp
            wref(42)=0.043583724529323453_dp
            wref(43)=0.042473515123653589_dp
            wref(44)=0.041262563242623529_dp
            wref(45)=0.039953741132720341_dp
            wref(46)=0.038550153178615629_dp
            wref(47)=0.037055128540240046_dp
            wref(48)=0.035472213256882384_dp
            wref(49)=0.033805161837141609_dp
            wref(50)=0.032057928354851554_dp
            wref(51)=0.030234657072402479_dp
            wref(52)=0.028339672614259483_dp
            wref(53)=0.026377469715054659_dp
            wref(54)=0.024352702568710873_dp
            wref(55)=0.022270173808383254_dp
            wref(56)=0.020134823153530209_dp
            wref(57)=0.017951715775697343_dp
            wref(58)=0.015726030476024719_dp
            wref(59)=0.013463047896718643_dp
            wref(60)=0.011168139460131129_dp
            wref(61)=0.0088467598263639477_dp
            wref(62)=0.0065044579689783629_dp
            wref(63)=0.0041470332605624676_dp
            wref(64)=0.0017832807216964329_dp
            
            call check(error, all(abs(x-xref) < 2*epsilon(x(1))))
            if (allocated(error)) return
            call check(error, all(abs(w-wref) < 2*epsilon(w(1))))
        end block



    end subroutine

    subroutine test_gauss_lobatto_analytic(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        real(dp) :: analytic, numeric

        ! test an analytic derivative versus an actual integration
        ! x**2 from -1 to 1
        analytic = 2.0_dp/3.0_dp
        do i=4,6 ! lobatto quadrature is less accurate for low i, so omit checking at i=2,3
            block
                real(dp), dimension(i) :: x,w
                call gauss_legendre_lobatto(x,w)
                numeric = sum(x**2 * w)
                !print *, i, numeric
                call check(error, abs(numeric-analytic) < 2*epsilon(analytic))
                if (allocated(error)) return
            end block
        end do

    end subroutine

    subroutine test_gauss_lobatto_5(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i


        ! test the values of nodes and weights
        i = 5
        block 
            real(dp), dimension(i) :: x,w,xref,wref
            call gauss_legendre_lobatto(x,w)


            xref(1)=-1.0000000000000000_dp
            xref(2)=-0.65465367070797714_dp
            xref(3)=0.0_dp
            xref(4)=0.65465367070797714_dp
            xref(5)=1.0000000000000000_dp

            wref(1)=0.10000000000000000_dp
            wref(2)=0.54444444444444444_dp
            wref(3)=0.71111111111111111_dp
            wref(4)=0.54444444444444444_dp
            wref(5)=0.10000000000000000_dp


            call check(error, all(abs(x-xref) < 2*epsilon(x(1))))
            if (allocated(error)) return
            call check(error, all(abs(w-wref) < 2*epsilon(w(1))))
        end block

    end subroutine

    subroutine test_gauss_lobatto_32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i

        i = 32 
        block 
            real(dp), dimension(i) :: x,w,xref,wref
            call gauss_legendre_lobatto(x,w)

            xref(1)=-1.0000000000000000_dp
            xref(2)=-0.99260893397276136_dp
            xref(3)=-0.97529469048270923_dp
            xref(4)=-0.94828483841723238_dp
            xref(5)=-0.91184993906373190_dp
            xref(6)=-0.86635247601267552_dp
            xref(7)=-0.81224473177744234_dp
            xref(8)=-0.75006449393667480_dp
            xref(9)=-0.68042975561555082_dp
            xref(10)=-0.60403258714842113_dp
            xref(11)=-0.52163226288156529_dp
            xref(12)=-0.43404771720184694_dp
            xref(13)=-0.34214940653888149_dp
            xref(14)=-0.24685065885020530_dp
            xref(15)=-0.14909859681364749_dp
            xref(16)=-0.049864725046593252_dp
            xref(17)=0.049864725046593252_dp
            xref(18)=0.14909859681364749_dp
            xref(19)=0.24685065885020530_dp
            xref(20)=0.34214940653888149_dp
            xref(21)=0.43404771720184694_dp
            xref(22)=0.52163226288156529_dp
            xref(23)=0.60403258714842113_dp
            xref(24)=0.68042975561555082_dp
            xref(25)=0.75006449393667480_dp
            xref(26)=0.81224473177744234_dp
            xref(27)=0.86635247601267552_dp
            xref(28)=0.91184993906373190_dp
            xref(29)=0.94828483841723238_dp
            xref(30)=0.97529469048270923_dp
            xref(31)=0.99260893397276136_dp
            xref(32)=1.0000000000000000_dp

            wref(1)=0.0020161290322580645_dp
            wref(2)=0.012398106501373844_dp
            wref(3)=0.022199552889291965_dp
            wref(4)=0.031775135410915466_dp
            wref(5)=0.041034201586062723_dp
            wref(6)=0.049885271336221207_dp
            wref(7)=0.058240497248055870_dp
            wref(8)=0.066016877257154544_dp
            wref(9)=0.073137139602679033_dp
            wref(10)=0.079530525692106252_dp
            wref(11)=0.085133497949668231_dp
            wref(12)=0.089890372957357833_dp
            wref(13)=0.093753875546813814_dp
            wref(14)=0.096685608948002601_dp
            wref(15)=0.098656436540761777_dp
            wref(16)=0.099646771501276778_dp
            wref(17)=0.099646771501276778_dp
            wref(18)=0.098656436540761777_dp
            wref(19)=0.096685608948002601_dp
            wref(20)=0.093753875546813814_dp
            wref(21)=0.089890372957357833_dp
            wref(22)=0.085133497949668231_dp
            wref(23)=0.079530525692106252_dp
            wref(24)=0.073137139602679033_dp
            wref(25)=0.066016877257154544_dp
            wref(26)=0.058240497248055870_dp
            wref(27)=0.049885271336221207_dp
            wref(28)=0.041034201586062723_dp
            wref(29)=0.031775135410915466_dp
            wref(30)=0.022199552889291965_dp
            wref(31)=0.012398106501373844_dp
            wref(32)=0.0020161290322580645_dp
            
            call check(error, all(abs(x-xref) < 2*epsilon(x(1))))
            if (allocated(error)) return
            call check(error, all(abs(w-wref) < 2*epsilon(w(1))))
        end block

    end subroutine

    subroutine test_gauss_lobatto_64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i


        i = 64
        block 
            real(dp), dimension(i) :: x,w,xref,wref
            call gauss_legendre_lobatto(x,w)


            xref(1)=-1.0000000000000000_dp
            xref(2)=-0.99817987150216322_dp
            xref(3)=-0.99390272670305729_dp
            xref(4)=-0.98719267660274024_dp
            xref(5)=-0.97806666283139607_dp
            xref(6)=-0.96654711036909923_dp
            xref(7)=-0.95266223578866292_dp
            xref(8)=-0.93644602747563416_dp
            xref(9)=-0.91793817351028163_dp
            xref(10)=-0.89718396784585004_dp
            xref(11)=-0.87423420065762749_dp
            xref(12)=-0.84914503454299099_dp
            xref(13)=-0.82197786730751705_dp
            xref(14)=-0.79279918182620814_dp
            xref(15)=-0.76168038340811997_dp
            xref(16)=-0.72869762508883694_dp
            xref(17)=-0.69393162129070484_dp
            xref(18)=-0.65746745031297650_dp
            xref(19)=-0.61939434613843155_dp
            xref(20)=-0.57980548006771842_dp
            xref(21)=-0.53879773271680044_dp
            xref(22)=-0.49647145693605775_dp
            xref(23)=-0.45293023223158118_dp
            xref(24)=-0.40828061128985404_dp
            xref(25)=-0.36263185922626182_dp
            xref(26)=-0.31609568619562581_dp
            xref(27)=-0.26878597401917000_dp
            xref(28)=-0.22081849749695350_dp
            xref(29)=-0.17231064108779297_dp
            xref(30)=-0.12338111165002799_dp
            xref(31)=-0.074149647946115919_dp
            xref(32)=-0.024736727621958728_dp
            xref(33)=0.024736727621958728_dp
            xref(34)=0.074149647946115919_dp
            xref(35)=0.12338111165002799_dp
            xref(36)=0.17231064108779297_dp
            xref(37)=0.22081849749695350_dp
            xref(38)=0.26878597401917000_dp
            xref(39)=0.31609568619562581_dp
            xref(40)=0.36263185922626182_dp
            xref(41)=0.40828061128985404_dp
            xref(42)=0.45293023223158118_dp
            xref(43)=0.49647145693605775_dp
            xref(44)=0.53879773271680044_dp
            xref(45)=0.57980548006771842_dp
            xref(46)=0.61939434613843155_dp
            xref(47)=0.65746745031297650_dp
            xref(48)=0.69393162129070484_dp
            xref(49)=0.72869762508883694_dp
            xref(50)=0.76168038340811997_dp
            xref(51)=0.79279918182620814_dp
            xref(52)=0.82197786730751705_dp
            xref(53)=0.84914503454299099_dp
            xref(54)=0.87423420065762749_dp
            xref(55)=0.89718396784585004_dp
            xref(56)=0.91793817351028163_dp
            xref(57)=0.93644602747563416_dp
            xref(58)=0.95266223578866292_dp
            xref(59)=0.96654711036909923_dp
            xref(60)=0.97806666283139607_dp
            xref(61)=0.98719267660274024_dp
            xref(62)=0.99390272670305729_dp
            xref(63)=0.99817987150216322_dp
            xref(64)=1.0000000000000000_dp

            wref(1)=0.00049603174603174603_dp
            wref(2)=0.0030560082449124904_dp
            wref(3)=0.0054960162038171569_dp
            wref(4)=0.0079212897900466340_dp
            wref(5)=0.010327002366815328_dp
            wref(6)=0.012707399197454735_dp
            wref(7)=0.015056683987961443_dp
            wref(8)=0.017369116384542182_dp
            wref(9)=0.019639040723241718_dp
            wref(10)=0.021860903511518060_dp
            wref(11)=0.024029268144023827_dp
            wref(12)=0.026138828614338438_dp
            wref(13)=0.028184422665848517_dp
            wref(14)=0.030161044499089451_dp
            wref(15)=0.032063857057727025_dp
            wref(16)=0.033888203884125398_dp
            wref(17)=0.035629620524489486_dp
            wref(18)=0.037283845459801173_dp
            wref(19)=0.038846830537807737_dp
            wref(20)=0.040314750881560237_dp
            wref(21)=0.041684014250801952_dp
            wref(22)=0.042951269833601819_dp
            wref(23)=0.044113416446892471_dp
            wref(24)=0.045167610125947702_dp
            wref(25)=0.046111271084289059_dp
            wref(26)=0.046942090027028316_dp
            wref(27)=0.047658033802220637_dp
            wref(28)=0.048257350376414549_dp
            wref(29)=0.048738573122233185_dp
            wref(30)=0.049100524407501308_dp
            wref(31)=0.049342318477139574_dp
            wref(32)=0.049463363620776646_dp
            wref(33)=0.049463363620776646_dp
            wref(34)=0.049342318477139574_dp
            wref(35)=0.049100524407501308_dp
            wref(36)=0.048738573122233185_dp
            wref(37)=0.048257350376414549_dp
            wref(38)=0.047658033802220637_dp
            wref(39)=0.046942090027028316_dp
            wref(40)=0.046111271084289059_dp
            wref(41)=0.045167610125947702_dp
            wref(42)=0.044113416446892471_dp
            wref(43)=0.042951269833601819_dp
            wref(44)=0.041684014250801952_dp
            wref(45)=0.040314750881560237_dp
            wref(46)=0.038846830537807737_dp
            wref(47)=0.037283845459801173_dp
            wref(48)=0.035629620524489486_dp
            wref(49)=0.033888203884125398_dp
            wref(50)=0.032063857057727025_dp
            wref(51)=0.030161044499089451_dp
            wref(52)=0.028184422665848517_dp
            wref(53)=0.026138828614338438_dp
            wref(54)=0.024029268144023827_dp
            wref(55)=0.021860903511518060_dp
            wref(56)=0.019639040723241718_dp
            wref(57)=0.017369116384542182_dp
            wref(58)=0.015056683987961443_dp
            wref(59)=0.012707399197454735_dp
            wref(60)=0.010327002366815328_dp
            wref(61)=0.0079212897900466340_dp
            wref(62)=0.0054960162038171569_dp
            wref(63)=0.0030560082449124904_dp
            wref(64)=0.00049603174603174603_dp
            
            call check(error, all(abs(x-xref) < 2*epsilon(x(1))))
            if (allocated(error)) return
            call check(error, all(abs(w-wref) < 2*epsilon(w(1))))
        end block

    end subroutine

end module


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_gauss, only : collect_gauss
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("gauss", collect_gauss) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
