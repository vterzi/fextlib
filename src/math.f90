module extlib_math
    implicit none

    private
    public :: gcd

contains
    elemental function gcd(i, j) result(n)
        integer, intent(in) :: i, j
        integer :: n

        integer :: d, r

        n = abs(i)
        d = abs(j)
        do while (d /= 0)
            r = mod(n, d)
            n = d
            d = r
        end do
    end function gcd
end module extlib_math
