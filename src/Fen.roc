module [
    backRankMateInThree,
    blackCanCaptureEp,
    drawBy50MoveRuleInOne,
    drawByStaleMate,
    drawByStaleMateInOne,
    drawByStaleMateIsBest,
    foolsMate,
    foolsMateInOne,
    initialGame,
    scholarsMate,
    scholarsMateInOne,
    syntaxError,
    whiteCanCastleQs,
    whiteCanPromote,
]

blackCanCaptureEp = "rnbqkbnr/1ppppppp/3P4/8/pP6/8/P1P1PPPP/RNBQKBNR b KQkq b3 0 4"
# FEN string from XBoard has half-move clock = 0
foolsMate = "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
foolsMateInOne = "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq g3 0 2"
initialGame = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
scholarsMate = "rnbqk2r/pppp1Qpp/5n2/2b1p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
scholarsMateInOne = "rnbqk2r/pppp1ppp/5n2/2b1p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
syntaxError = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/ABCDEFGH w KQkq - 0 1"
whiteCanCastleQs = "r1bqk2r/ppp2ppp/2np1n2/4p1B1/1b2P3/2NP1Q2/PPP2PPP/R3KBNR w KQkq - 2 6"
whiteCanPromote = "rnbqkbnr/1pppPppp/8/8/8/p7/PPP1PPPP/RNBQKBNR w KQkq - 0 5"
drawBy50MoveRuleInOne = "4k3/8/4P3/8/8/8/8/3RK3 w - - 99 1"
drawByStaleMate = "k7/8/K7/8/8/8/8/1R6 b - - 0 1"
drawByStaleMateInOne = "k7/8/K7/8/8/8/8/Rn6 w - - 0 1"
drawByStaleMateIsBest = "k7/8/K7/P7/8/8/Rp6/8 w - - 0 1"
backRankMateInThree = "r7/1p3pkp/2p3p1/5n2/5B2/8/1P3PPP/6K1 b - - 0 17"
