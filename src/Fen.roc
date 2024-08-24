module [
    backRankMateInThree,
    blackCanCaptureEp,
    blackIsCheckMated,
    drawBy50MoveRuleInOne,
    drawByStaleMate,
    drawByStaleMateInOne,
    drawByStaleMateIsBest,
    mateInTwo,
    foolsMate,
    foolsMateInOne,
    initialGame,
    scholarsMate,
    scholarsMateInOne,
    syntaxError,
    whiteCanCastleQs,
    whiteCanPromote,
    whiteCannotCastle,
]

blackCanCaptureEp = "rnbqkbnr/1ppppppp/3P4/8/pP6/8/P1P1PPPP/RNBQKBNR b KQkq b3 0 4"
# FEN string from XBoard has half-move clock = 0
foolsMate = "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
foolsMateInOne = "rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq g3 0 2"
initialGame = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
mateInTwo = "rnR1k1r1/pp1pqp2/4p3/7p/6nQ/P3P3/3P1PPb/4KBNR b Kq - 0 20"
scholarsMate = "rnbqk2r/pppp1Qpp/5n2/2b1p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
scholarsMateInOne = "rnbqk2r/pppp1ppp/5n2/2b1p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
syntaxError = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/ABCDEFGH w KQkq - 0 1"
whiteCanCastleQs = "r1bqk2r/ppp2ppp/2np1n2/4p1B1/1b2P3/2NP1Q2/PPP2PPP/R3KBNR w KQkq - 2 6"
whiteCanPromote = "rnbqkbnr/1pppPppp/8/8/8/p7/PPP1PPPP/RNBQKBNR w KQkq - 0 5"
whiteCannotCastle = "rnb1k3/pppp1p2/3b4/1N3n2/2B1p3/P3P1P1/1PPP1P2/R1B1K2r w KQq - 0 15"
drawBy50MoveRuleInOne = "4k3/8/4P3/8/8/8/8/3RK3 w - - 99 1"
drawByStaleMate = "k7/8/K7/8/8/8/8/1R6 b - - 0 1"
drawByStaleMateInOne = "k7/8/K7/8/8/8/8/Rn6 w - - 0 1"
drawByStaleMateIsBest = "k7/8/K7/P7/8/8/Rp6/8 w - - 0 1"
backRankMateInThree = "r7/1p3pkp/2p3p1/5n2/5B2/8/1P3PPP/6K1 b - - 0 17"
blackIsCheckMated = "3r1N2/ppp5/2n1Q3/3k1p2/3P2B1/2P1P3/P1PK2PP/R7 b - - 4 23"
