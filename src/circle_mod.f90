module circle_mod
    use :: sdl2
    implicit none

    type :: sdl_circle
        integer :: x, y, r
    end type

    contains 

    function sdl_render_draw_circle(renderer, circle)
        type(c_ptr), intent(out)     :: renderer
        type(sdl_circle), intent(in) :: circle
        integer(kind=c_int)      :: sdl_render_draw_circle
        
        integer :: center_x, center_y, radius
        integer :: diameter, x, y, tx, ty, error, rc

        center_x = circle%x
        center_y = circle%y
        radius = circle%r

        diameter = radius * 2
        x = radius - 1
        y = 0
        tx = 1
        ty = 1
        error = tx - diameter

        do while (x .ge. y)
            rc = sdl_render_draw_point(renderer, center_x + x, center_y - y)
            rc = sdl_render_draw_point(renderer, center_x + x, center_y + y)
            rc = sdl_render_draw_point(renderer, center_x - x, center_y - y)
            rc = sdl_render_draw_point(renderer, center_x - x, center_y + y)
            rc = sdl_render_draw_point(renderer, center_x + y, center_y - x)
            rc = sdl_render_draw_point(renderer, center_x + y, center_y + x)
            rc = sdl_render_draw_point(renderer, center_x - y, center_y - x)
            rc = sdl_render_draw_point(renderer, center_x - y, center_y + x)

            if (error .le. 0) then
                y = y + 1
                error = error + ty
                ty = ty + 2
            end if

            if (error .gt. 0) then
                x = x - 1
                tx = tx + 2
                error = error + (tx - diameter)
            end if
        end do

        sdl_render_draw_circle = rc

    end function sdl_render_draw_circle

    function sdl_render_fill_circle(renderer, circle)
        type(c_ptr), intent(out) :: renderer
        type(sdl_circle), intent(in) :: circle
        integer(kind=c_int)      :: sdl_render_fill_circle

        integer :: center_x, center_y, radius
        integer :: offsetx, offsety, d, rc

        center_x = circle%x
        center_y = circle%y
        radius = circle%r

        offsetx = 0
        offsety = radius
        d = radius - 1

        do while (offsety .ge. offsetx)
            rc = sdl_render_draw_line(renderer, center_x - offsety, center_y + offsetx, &
                                                center_x + offsety, center_y + offsetx)
            rc = sdl_render_draw_line(renderer, center_x - offsetx, center_y + offsety, &
                                                center_x + offsetx, center_y + offsety)
            rc = sdl_render_draw_line(renderer, center_x - offsetx, center_y - offsety, &
                                                center_x + offsetx, center_y - offsety)
            rc = sdl_render_draw_line(renderer, center_x - offsety, center_y - offsetx, &
                                                center_x + offsety, center_y - offsetx)

            if (d .gt. 2 * offsetx) then
                d = d - (2 * offsetx + 1)
                offsetx = offsetx + 1

            else if (d .lt. 2 * (radius - offsety)) then
                d = d + (2 * offsety - 1)
                offsety = offsety - 1;
            else
                d = d + (2 * (offsety - offsetx - 1))
                offsety = offsety - 1;
                offsetx = offsetx + 1;
            end if

        end do

        sdl_render_fill_circle = rc

    end function sdl_render_fill_circle

end module circle_mod