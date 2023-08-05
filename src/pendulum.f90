module pendulum_mod
    use :: sdl2
    use :: circle_mod
implicit none

    real :: G = 1
    real(kind=8) :: PI = 4.D0*DATAN(1.D0)

    type pendulum_t
        integer :: r, m
        real(kind=8) :: a, v
    end type pendulum_t

    type double_pendulum_t
        type(sdl_point), dimension(500) :: history
        integer(kind=c_int8_t) :: r, g, b
        integer :: history_index
        integer :: ox, oy
        type(pendulum_t) :: p1, p2
    end type double_pendulum_t

    interface double_pendulum_t
        module procedure :: new_double_pendulum
    end interface double_pendulum_t

contains

    subroutine slide_array(arr, count)
        type(sdl_point), dimension(count) :: arr
        integer :: count

        integer :: i
        do i=1, count-1
            arr(i) = arr(i+1)
        end do
    end subroutine slide_array

    function new_double_pendulum(ox, oy, r1, a1, m1, r2, a2, m2, r, g, b)
        integer :: ox, oy, r1, m1, r2, m2
        real(kind=8) :: a1, a2 
        integer(kind=c_int8_t) :: r, g, b
        type(double_pendulum_t) :: new_double_pendulum

        type(pendulum_t) :: p1, p2
        type(sdl_point), dimension(500) :: history

        p1 = pendulum_t(r=r1, a=a1, m=m1, v=0)
        p2 = pendulum_t(r=r2, a=a2, m=m2, v=0)

        new_double_pendulum = double_pendulum_t(ox=ox, oy=oy, p1=p1, p2=p2, &
                                                history=history, history_index=1, &
                                                r=r, g=g, b=b)

    end function new_double_pendulum

    function sdl_render_double_pendulum(renderer, pendulum)
        integer(kind=c_int)      :: sdl_render_double_pendulum
        type(c_ptr), intent(out) :: renderer
        type(double_pendulum_t), intent(inout) :: pendulum

        integer :: rc, i
        type(sdl_circle) :: c

        integer :: x1, y1, x2, y2

        x1 = pendulum%ox + int(pendulum%p1%r * cos(pendulum%p1%a + PI/2))
        y1 = pendulum%oy + int(pendulum%p1%r * sin(pendulum%p1%a + PI/2))

        x2 = x1 + int(pendulum%p2%r * cos(pendulum%p2%a + PI/2))
        y2 = y1 + int(pendulum%p2%r * sin(pendulum%p2%a + PI/2))

        ! Render history.
        rc = sdl_set_render_draw_color(renderer, &
                                    uint8(200), &
                                    uint8(200), &
                                    uint8(200), &
                                    uint8(SDL_ALPHA_OPAQUE))

        if (pendulum%history_index .gt. 3) then
            do i=2, pendulum%history_index-1
                rc = sdl_render_draw_line(renderer, pendulum%history(i-1)%x, pendulum%history(i-1)%y,&
                                                    pendulum%history(i)%x, pendulum%history(i)%y)
            end do
        end if

        ! Set draw color.
        rc = sdl_set_render_draw_color(renderer, &
                                    pendulum%r, &
                                    pendulum%g, &
                                    pendulum%b, &
                                    uint8(SDL_ALPHA_OPAQUE))

        ! Render origin circle.
        c = sdl_circle(pendulum%ox, pendulum%oy, 5)
        rc = sdl_render_fill_circle(renderer, c)

        ! Render first pendulum line.
        rc = sdl_render_draw_line(renderer,   pendulum%ox, &
                                                    pendulum%oy, &
                                                    x1, y1)

        ! Render first pendulum circle.
        c = sdl_circle(x1, y1, 10)
        rc = sdl_render_fill_circle(renderer, c)

        ! Render second pendulum line.
        rc = sdl_render_draw_line(renderer, x1, y1, x2, y2)

        ! Render second pendulum circle.
        c = sdl_circle(x2, y2, 10)
        rc = sdl_render_fill_circle(renderer, c)

        sdl_render_double_pendulum = rc

    end function sdl_render_double_pendulum

    subroutine update_pendulum(pendulum)
        type(double_pendulum_t), intent(inout) :: pendulum

        real(kind=8) :: m1, m2, a1, a2, r1, r2, v1, v2

        real(kind=8) :: a1_a, a2_a
        real(kind=8) :: num, den, e1, e2, e3, e4
        integer :: x1, y1, x2, y2

        m1 = pendulum%p1%m
        a1 = pendulum%p1%a
        r1 = pendulum%p1%r
        v1 = pendulum%p1%v
        m2 = pendulum%p2%m
        a2 = pendulum%p2%a
        r2 = pendulum%p2%r
        v2 = pendulum%p2%v

        e1 = -G * (2.0*m1 + m2)*sin(a1)
        e2 = -m2 * G * sin(a1 - 2.0*a2)
        e3 = -2.0 * sin(a1 - a2)*m2
        e4 = v2*v2 * r2 + v1*v1 * r1 * cos(a1 - a2)

        num = e1 + e2 + e3*e4

        den = r1*(2.0*m1 + m2 - m2*cos(2.0*a1 - 2.0*a2))

        a1_a = num / den

        e1 = 2 * sin(a1 - a2)
        e2 = v1 * v1 * r1 * (m1 + m2)
        e3 = G * (m1 + m2) * cos(a1)
        e4 = v2 * v2 * r2 * m2 * cos(a1 - a2)

        num = e1 * (e2 + e3 + e4)

        den = r2 * (2*m1 + m2 - m2 * cos(2*a1 - 2*a2))

        a2_a = num / den

        pendulum%p1%v = pendulum%p1%v + a1_a
        pendulum%p2%v = pendulum%p2%v + a2_a

        pendulum%p1%a = pendulum%p1%a + pendulum%p1%v
        pendulum%p2%a = pendulum%p2%a + pendulum%p2%v

        ! Update history.

        x1 = pendulum%ox + int(pendulum%p1%r * cos(pendulum%p1%a + PI/2))
        y1 = pendulum%oy + int(pendulum%p1%r * sin(pendulum%p1%a + PI/2))

        x2 = x1 + int(pendulum%p2%r * cos(pendulum%p2%a + PI/2))
        y2 = y1 + int(pendulum%p2%r * sin(pendulum%p2%a + PI/2))

        pendulum%history(pendulum%history_index) = sdl_point(x2, y2)
        pendulum%history_index = pendulum%history_index + 1

        if (pendulum%history_index .gt. size(pendulum%history)) then
            pendulum%history_index = size(pendulum%history)
            call slide_array(pendulum%history, size(pendulum%history))
        end if

    end subroutine update_pendulum

end module pendulum_mod