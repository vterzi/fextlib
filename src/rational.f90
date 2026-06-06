module extlib_rational
    use extlib_math, only: gcd

    implicit none

    private

    type, public :: Rational
        private
        integer :: n = 0, d = 1
    contains
        procedure, private :: canonicalize => canonicalize_Rational
        procedure, private :: reduce => reduce_Rational
        procedure, private :: normalize => normalize_Rational
        procedure :: nom => Rational_nom
        procedure :: denom => Rational_denom
        procedure, private :: Rational_eq_Rational
        generic :: operator(==) => Rational_eq_Rational
        procedure, private :: Rational_ne_Rational
        generic :: operator(/=) => Rational_ne_Rational
        procedure, private :: Rational_gt_Rational
        generic :: operator(>) => Rational_gt_Rational
        procedure, private :: Rational_lt_Rational
        generic :: operator(<) => Rational_lt_Rational
        procedure, private :: Rational_ge_Rational
        generic :: operator(>=) => Rational_ge_Rational
        procedure, private :: Rational_le_Rational
        generic :: operator(<=) => Rational_le_Rational
        procedure, private :: pos_Rational
        generic :: operator(+) => pos_Rational
        procedure, private :: neg_Rational
        generic :: operator(-) => neg_Rational
        procedure, private :: Rational_add_Rational
        generic :: operator(+) => Rational_add_Rational
        procedure, private :: Rational_sub_Rational
        generic :: operator(-) => Rational_sub_Rational
        procedure, private :: Rational_mul_Rational
        generic :: operator(*) => Rational_mul_Rational
        procedure, private :: Rational_div_Rational
        generic :: operator(/) => Rational_div_Rational
        procedure, private :: read_fmt_Rational
        generic :: read(formatted) => read_fmt_Rational
        procedure, private :: write_fmt_Rational
        generic :: write(formatted) => write_fmt_Rational
        procedure, private :: read_unfmt_Rational
        generic :: read(unformatted) => read_unfmt_Rational
        procedure, private :: write_unfmt_Rational
        generic :: write(unformatted) => write_unfmt_Rational
    end type Rational

    interface Rational
        module procedure :: new_Rational
    end interface

    public :: assignment(=)
    interface assignment(=)
        module procedure :: I_assign_Rational
        module procedure :: Rational_assign_I
    end interface

    public :: operator(==)
    interface operator(==)
        module procedure :: I_eq_Rational
        module procedure :: Rational_eq_I
    end interface

    public :: operator(/=)
    interface operator(/=)
        module procedure :: I_ne_Rational
        module procedure :: Rational_ne_I
    end interface

    public :: operator(>)
    interface operator(>)
        module procedure :: I_gt_Rational
        module procedure :: Rational_gt_I
    end interface

    public :: operator(<)
    interface operator(<)
        module procedure :: I_lt_Rational
        module procedure :: Rational_lt_I
    end interface

    public :: operator(>=)
    interface operator(>=)
        module procedure :: I_ge_Rational
        module procedure :: Rational_ge_I
    end interface

    public :: operator(<=)
    interface operator(<=)
        module procedure :: I_le_Rational
        module procedure :: Rational_le_I
    end interface

    public :: operator(+)
    interface operator(+)
        module procedure :: I_add_Rational
        module procedure :: Rational_add_I
    end interface

    public :: operator(-)
    interface operator(-)
        module procedure :: I_sub_Rational
        module procedure :: Rational_sub_I
    end interface

    public :: operator(*)
    interface operator(*)
        module procedure :: I_mul_Rational
        module procedure :: Rational_mul_I
    end interface

    public :: operator(/)
    interface operator(/)
        module procedure :: I_div_Rational
        module procedure :: Rational_div_I
    end interface

    public :: operator(**)
    interface operator(**)
        module procedure :: Rational_pow_I
    end interface

    public :: operator(//)
    interface operator(//)
        module procedure :: I_cat_I
    end interface

    public :: abs
    interface abs
        module procedure :: abs_Rational
    end interface

contains
    elemental function ordered_rationals(xn, xd, yn, yd) result(r)
        integer, intent(in) :: xn, xd, yn, yd
        logical :: r

        logical :: l1, l2
        integer :: n1, n2, d1, d2, i1, i2

        l1 = xn >= 0
        l2 = yn >= 0
        if (l1 .neqv. l2) then
            r = l2
            return
        else if (l1) then
            n1 = xn
            d1 = xd
            n2 = yn
            d2 = yd
        else
            n1 = -yn
            d1 = yd
            n2 = -xn
            d2 = xd
        end if
        do
            i1 = n1 / d1
            i2 = n2 / d2
            if (i1 /= i2) then
                r = i1 < i2
                return
            end if
            i1 = mod(n1, d1)
            i2 = mod(n2, d2)
            l1 = i1 == 0
            l2 = i2 == 0
            if (l1 .or. l2) then
                r = l1 .and. .not. l2
                return
            end if
            n1 = d2
            n2 = d1
            d1 = i2
            d2 = i1
        end do
    end function ordered_rationals


    elemental subroutine canonicalize_Rational(x)
        class(Rational), intent(inout) :: x

        if (x%d < 0) then
            x%n = -x%n
            x%d = -x%d
        end if
    end subroutine canonicalize_Rational


    elemental subroutine reduce_Rational(x)
        class(Rational), intent(inout) :: x

        integer :: f

        f = gcd(x%n, x%d)
        x%n = x%n / f
        x%d = x%d / f
    end subroutine reduce_Rational


    elemental subroutine normalize_Rational(x)
        class(Rational), intent(inout) :: x

        call x%canonicalize()
        call x%reduce()
    end subroutine normalize_Rational


    elemental function new_Rational(n, d) result(r)
        integer, intent(in) :: n
        integer, intent(in), optional :: d
        type(Rational) :: r

        r%n = n
        if (present(d)) r%d = d
        call r%normalize()
    end function new_Rational


    elemental function Rational_nom(x) result(n)
        class(Rational), intent(in) :: x
        integer :: n

        n = x%n
    end function Rational_nom


    elemental function Rational_denom(x) result(d)
        class(Rational), intent(in) :: x
        integer :: d

        d = x%d
    end function Rational_denom


    elemental subroutine I_assign_Rational(i, x)
        integer, intent(out) :: i
        class(Rational), intent(in) :: x

        i = x%n / x%d
    end subroutine I_assign_Rational


    elemental subroutine Rational_assign_I(x, i)
        type(Rational), intent(out) :: x
        integer, intent(in) :: i

        x%n = i
        x%d = 1
    end subroutine Rational_assign_I


    elemental function Rational_eq_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        logical :: r

        r = x%n == y%n .and. x%d == y%d
    end function Rational_eq_Rational


    elemental function Rational_ne_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        logical :: r

        r = x%n /= y%n .or. x%d /= y%d
    end function Rational_ne_Rational


    elemental function Rational_gt_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        logical :: r

        ! r = x%n * y%d > x%d * y%n
        r = ordered_rationals(y%n, y%d, x%n, x%d)
    end function Rational_gt_Rational


    elemental function Rational_lt_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        logical :: r

        ! r = x%n * y%d < x%d * y%n
        r = ordered_rationals(x%n, x%d, y%n, y%d)
    end function Rational_lt_Rational


    elemental function Rational_ge_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        logical :: r

        r = Rational_eq_Rational(x, y) .or. Rational_gt_Rational(x, y)
    end function Rational_ge_Rational


    elemental function Rational_le_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        logical :: r

        r = Rational_eq_Rational(x, y) .or. Rational_lt_Rational(x, y)
    end function Rational_le_Rational


    elemental function pos_Rational(x) result(r)
        class(Rational), intent(in) :: x
        type(Rational) :: r

        r = x
    end function pos_Rational


    elemental function neg_Rational(x) result(r)
        class(Rational), intent(in) :: x
        type(Rational) :: r

        r%n = -x%n
        r%d = x%d
    end function neg_Rational


    elemental function Rational_add_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        type(Rational) :: r

        r%n = x%n * y%d + x%d * y%n
        r%d = x%d * y%d
        call r%reduce()
    end function Rational_add_Rational


    elemental function Rational_sub_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        type(Rational) :: r

        r%n = x%n * y%d - x%d * y%n
        r%d = x%d * y%d
        call r%reduce()
    end function Rational_sub_Rational


    elemental function Rational_mul_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        type(Rational) :: r

        r%n = x%n * y%n
        r%d = x%d * y%d
        call r%reduce()
    end function Rational_mul_Rational


    elemental function Rational_div_Rational(x, y) result(r)
        class(Rational), intent(in) :: x, y
        type(Rational) :: r

        r%n = x%n * y%d
        r%d = x%d * y%n
        call r%normalize()
    end function Rational_div_Rational


    subroutine read_fmt_Rational(dtv, unit, iotype, v_list, iostat, iomsg)
        class(Rational), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        complex(kind(1.0d0)) :: num

        read(unit, *, iostat=iostat, iomsg=iomsg) num
        dtv%n = nint(real(num))
        dtv%d = nint(aimag(num))
    end subroutine read_fmt_Rational


    subroutine write_fmt_Rational(dtv, unit, iotype, v_list, iostat, iomsg)
        class(Rational), intent(in) :: dtv
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        write( &
            unit, '("(",i0,",",i0,")")', iostat=iostat, iomsg=iomsg &
        ) dtv%n, dtv%d
    end subroutine write_fmt_Rational


    subroutine read_unfmt_Rational(dtv, unit, iostat, iomsg)
        class(Rational), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        read(unit, iostat=iostat, iomsg=iomsg) dtv%n, dtv%d
    end subroutine


    subroutine write_unfmt_Rational(dtv, unit, iostat, iomsg)
        class(Rational), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        write(unit, iostat=iostat, iomsg=iomsg) dtv%n, dtv%d
    end subroutine


    elemental function I_eq_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        logical :: r

        r = 1 == x%d .and. i == x%n
    end function I_eq_Rational


    elemental function Rational_eq_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        logical :: r

        r = x%d == 1 .and. x%n == i
    end function Rational_eq_I


    elemental function I_ne_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        logical :: r

        r = 1 /= x%d .or. i /= x%n
    end function I_ne_Rational


    elemental function Rational_ne_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        logical :: r

        r = x%d /= 1 .or. x%n /= i
    end function Rational_ne_I


    elemental function I_gt_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        logical :: r

        r = i * x%d > x%n
    end function I_gt_Rational


    elemental function Rational_gt_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        logical :: r

        r = x%n > i * x%d
    end function Rational_gt_I


    elemental function I_lt_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        logical :: r

        r = i * x%d < x%n
    end function I_lt_Rational


    elemental function Rational_lt_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        logical :: r

        r = x%n < i * x%d
    end function Rational_lt_I


    elemental function I_ge_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        logical :: r

        r = i * x%d >= x%n
    end function I_ge_Rational


    elemental function Rational_ge_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        logical :: r

        r = x%n >= i * x%d
    end function Rational_ge_I


    elemental function I_le_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        logical :: r

        r = i * x%d <= x%n
    end function I_le_Rational


    elemental function Rational_le_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        logical :: r

        r = x%n <= i * x%d
    end function Rational_le_I


    elemental function I_add_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        type(Rational) :: r

        r%n = i * x%d + x%n
        r%d = x%d
    end function I_add_Rational


    elemental function Rational_add_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        type(Rational) :: r

        r%n = x%n + i * x%d
        r%d = x%d
    end function Rational_add_I


    elemental function I_sub_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        type(Rational) :: r

        r%n = i * x%d - x%n
        r%d = x%d
    end function I_sub_Rational


    elemental function Rational_sub_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        type(Rational) :: r

        r%n = x%n - i * x%d
        r%d = x%d
    end function Rational_sub_I


    elemental function I_mul_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        type(Rational) :: r

        r%n = i * x%n
        r%d = x%d
        call r%reduce()
    end function I_mul_Rational


    elemental function Rational_mul_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        type(Rational) :: r

        r%n = x%n * i
        r%d = x%d
        call r%reduce()
    end function Rational_mul_I


    elemental function I_div_Rational(i, x) result(r)
        integer, intent(in) :: i
        class(Rational), intent(in) :: x
        type(Rational) :: r

        r%n = i * x%d
        r%d = x%n
        call r%normalize()
    end function I_div_Rational


    elemental function Rational_div_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        type(Rational) :: r

        r%n = x%n
        r%d = x%d * i
        call r%normalize()
    end function Rational_div_I


    elemental function Rational_pow_I(x, i) result(r)
        class(Rational), intent(in) :: x
        integer, intent(in) :: i
        type(Rational) :: r

        if (i > 0) then
            r%n = x%n ** i
            r%d = x%d ** i
        else if (i < 0) then
            r%n = x%d ** (-i)
            r%d = x%n ** (-i)
            call r%canonicalize()
        else
            r%n = 1
            r%d = 1
        end if
    end function Rational_pow_I


    elemental function I_cat_I(i, j) result(r)
        integer, intent(in) :: i, j
        type(Rational) :: r

        r = new_Rational(i, j)
    end function I_cat_I


    elemental function abs_Rational(a) result(r)
        class(Rational), intent(in) :: a
        type(Rational) :: r

        r%n = abs(a%n)
    end function abs_Rational
end module extlib_rational
