module [
    back_rank_mate_in_three,
    black_can_capture_ep,
    black_is_check_mated,
    crash_in_generate_black_ep,
    draw_by50_move_rule_in_one,
    draw_by_stale_mate,
    draw_by_stale_mate_in_one,
    draw_by_stale_mate_is_best,
    mate_in_two,
    fools_mate,
    fools_mate_in_one,
    initial_game,
    scholars_mate,
    scholars_mate_in_one,
    syntax_error,
    white_can_castle_qs,
    white_can_promote,
    white_cannot_castle,
]

black_can_capture_ep = "rnbqkbnr/1ppppppp/3P4/8/pP6/8/P1P1PPPP/RNBQKBNR b KQkq b3 0 4"
# FEN string from XBoard has half-move clock = 0
fools_mate = "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
fools_mate_in_one = "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq g3 0 2"
initial_game = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
mate_in_two = "rnR1k1r1/pp1pqp2/4p3/7p/6nQ/P3P3/3P1PPb/4KBNR b Kq - 0 20"
scholars_mate = "rnbqk2r/pppp1Qpp/5n2/2b1p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
scholars_mate_in_one = "rnbqk2r/pppp1ppp/5n2/2b1p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
syntax_error = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/ABCDEFGH w KQkq - 0 1"
white_can_castle_qs = "r1bqk2r/ppp2ppp/2np1n2/4p1B1/1b2P3/2NP1Q2/PPP2PPP/R3KBNR w KQkq - 2 6"
white_can_promote = "rnbqkbnr/1pppPppp/8/8/8/p7/PPP1PPPP/RNBQKBNR w KQkq - 0 5"
white_cannot_castle = "rnb1k3/pppp1p2/3b4/1N3n2/2B1p3/P3P1P1/1PPP1P2/R1B1K2r w KQq - 0 15"
draw_by50_move_rule_in_one = "4k3/8/4P3/8/8/8/8/3RK3 w - - 99 1"
draw_by_stale_mate = "k7/8/K7/8/8/8/8/1R6 b - - 0 1"
draw_by_stale_mate_in_one = "k7/8/K7/8/8/8/8/Rn6 w - - 0 1"
draw_by_stale_mate_is_best = "k7/8/K7/P7/8/8/Rp6/8 w - - 0 1"
back_rank_mate_in_three = "r7/1p3pkp/2p3p1/5n2/5B2/8/1P3PPP/6K1 b - - 0 17"
black_is_check_mated = "3r1N2/ppp5/2n1Q3/3k1p2/3P2B1/2P1P3/P1PK2PP/R7 b - - 4 23"
crash_in_generate_black_ep = "r1bqkbnr/pp5p/2p2pp1/4p3/1P6/4P3/p2PNPPP/1NB2K1R b kq b3 0 2"
