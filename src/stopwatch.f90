module extlib_stopwatch
    implicit none

    private
    public :: fmt_time

    type, public :: Stopwatch
        private
        real :: initial = -1, elapsed = 0
    contains
        procedure :: reset => reset_Stopwatch
        procedure :: is_running => is_running_Stopwatch
        procedure :: toggle => toggle_Stopwatch
        procedure :: read => read_Stopwatch
        procedure :: read_out => read_out_Stopwatch
    end type Stopwatch

contains
    elemental subroutine reset_Stopwatch(this)
        class(Stopwatch), intent(inout) :: this

        this%initial = -1
        this%elapsed = 0
    end subroutine reset_Stopwatch


    elemental function is_running_Stopwatch(this) result(running)
        class(Stopwatch), intent(in) :: this
        logical :: running

        running = this%initial >= 0
    end function is_running_Stopwatch


    subroutine toggle_Stopwatch(this)
        class(Stopwatch), intent(inout) :: this

        real :: time

        if (this%is_running()) then
            call cpu_time(time)
            this%elapsed = this%elapsed + time - this%initial
            this%initial = -1
        else
            call cpu_time(this%initial)
        end if
    end subroutine toggle_Stopwatch


    elemental function read_Stopwatch(this) result(elapsed)
        class(Stopwatch), intent(in) :: this
        real :: elapsed

        elapsed = this%elapsed
    end function read_Stopwatch


    function read_out_Stopwatch(this) result(elapsed)
        class(Stopwatch), intent(inout) :: this
        real :: elapsed

        if (this%is_running()) call this%toggle()
        elapsed = this%read()
        call this%reset()
    end function read_out_Stopwatch


    pure subroutine fmt_time(time, string)
        real, intent(in) :: time
        character(len=:), allocatable, intent(out) :: string

        integer, parameter :: &
            SECONDS_PER_MINUTE = 60, &
            SECONDS_PER_HOUR = 60 * SECONDS_PER_MINUTE, &
            SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR, &
            MILLISECONDS_PER_SECOND = 1000
        integer :: days, hours, minutes, seconds, milliseconds
        real :: remainder
        character(len=12) :: buffer

        remainder = time
        days = int(remainder / SECONDS_PER_DAY)
        remainder = remainder - days * SECONDS_PER_DAY
        hours = int(remainder / SECONDS_PER_HOUR)
        remainder = remainder - hours * SECONDS_PER_HOUR
        minutes = int(remainder / SECONDS_PER_MINUTE)
        remainder = remainder - minutes * SECONDS_PER_MINUTE
        seconds = int(remainder)
        remainder = remainder - seconds
        milliseconds = int(remainder * MILLISECONDS_PER_SECOND)
        write(buffer, *) days
        string = trim(adjustl(buffer))
        write(buffer, '(i2.2,":",i2.2,":",i2.2,".",i3.3)') &
            hours, minutes, seconds, milliseconds
        string = string // '-' // buffer
    end subroutine fmt_time
end module extlib_stopwatch
