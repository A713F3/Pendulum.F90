program main
    use, intrinsic :: iso_c_binding, only: c_associated, c_null_char, c_ptr
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    use :: sdl2
    use :: pendulum_mod
    implicit none

    integer, parameter :: SCREEN_WIDTH  = 800
    integer, parameter :: SCREEN_HEIGHT = 700

    type(c_ptr)     :: window
    type(c_ptr)     :: renderer
    type(sdl_event) :: event
    integer         :: rc, i, j, k
    logical         :: is_running, sim_running
    real            :: a_offset 

    type(double_pendulum_t) :: pendulum
    type(double_pendulum_t), dimension(400) :: pendulums

    ! Initialise SDL.
    if (sdl_init(SDL_INIT_VIDEO) < 0) then
        write (stderr, *) 'SDL Error: ', sdl_get_error()
        stop
    end if

    ! Create the SDL window.
    window = sdl_create_window('Double Pendulum Simulation' // c_null_char, &
                            SDL_WINDOWPOS_UNDEFINED, &
                            SDL_WINDOWPOS_UNDEFINED, &
                            SCREEN_WIDTH, &
                            SCREEN_HEIGHT, &
                            SDL_WINDOW_SHOWN)

    if (.not. c_associated(window)) then
        write (stderr, *) 'SDL Error: ', sdl_get_error()
        stop
    end if

    ! Create the renderer.
    renderer = sdl_create_renderer(window, -1, 0)

    pendulum = double_pendulum_t(ox=SCREEN_WIDTH/2, oy=200, &
                                r1=200, a1=-5*PI/6, m1=40, &
                                r2=200, a2=PI*0, m2=40, &
                                r=uint8(255), g=uint8(100), b=uint8(0))

    !do i=1, size(pendulums)
    !    call random_number(a_offset)
    !    a_offset = a_offset * 0.02
    !    pendulums(i) = double_pendulum_t(ox=SCREEN_WIDTH/2, oy=200, &
    !                                    r1=200, a1=-5*PI/6 + a_offset, m1=10,  &
    !                                    r2=200, a2=PI*0,    m2=10,  &
    !                                    r=uint8(int(255*(i/400.0))), g=uint8(0), b=uint8(int(255*(1 - i/400.0))))
    !end do

    ! Event loop.
    is_running = .true.
    sim_running = .false.

    do while (is_running)
        ! Catch events.
        do while (sdl_poll_event(event) > 0)
            select case (event%type)
                case (SDL_QUITEVENT)
                    is_running = .false.
                
                case (SDL_KEYDOWN)
                    if (event%key%key_sym%sym .eq. SDLK_SPACE) then
                        sim_running = .not. sim_running
                    end if
            end select
        end do

        ! Fill screen background.
        rc = sdl_set_render_draw_color(renderer, &
                                    uint8(30), &
                                    uint8(30), &
                                    uint8(30), &
                                    uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_clear(renderer)

        !do i=1, 255
        !    rc = sdl_render_double_pendulum(renderer, pendulums(i))
        !    if (sim_running) then
        !        call update_pendulum(pendulums(i))
        !    end if
        !end do

        rc = sdl_render_double_pendulum(renderer, pendulum)
        if (sim_running) then
            call update_pendulum(pendulum)
        end if

        ! Render to screen.
        call sdl_render_present(renderer)

        call sdl_delay(20)
    end do

    ! Quit.
    call sdl_destroy_renderer(renderer)
    call sdl_destroy_window(window)
    call sdl_quit()
end program main