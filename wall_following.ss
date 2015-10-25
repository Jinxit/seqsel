#define front_min 0.1f
#define side_min 0.1f
#define encoder_min 0.1f
#define normal_speed 0.5f

#var float delta_encoder_left
#var float delta_encoder_right
#var float ir_front

selector WallFollow:
    sequence Stuck:
        cond delta_encoder_left < #encoder_min
        cond delta_encoder_right < #encoder_min
        call publish(nord::action::backwards_distance, 0.1f)
    sequence FrontWall:
        cond ir_front < #front_min
        selector Turn:
            sequence TurnLeft:
                cond ir_right_avg < ir_left_avg
                call publish(nord::action::turn_left, 90.0f)
            sequence TurnRight:
                cond ir_left_avg < ir_right_avg
                call publish(nord::action::turn_right, 90.0f)
    call publish(nord::action::forwards_speed, #normal_speed)
