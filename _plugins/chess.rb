
module Jekyll

    #
    # The Chessboard tag provides an straightforward way to represent
    # a chessboard and the standard chess pieces. To declare where each
    # piece should go the following notation is used:
    #
    # WBc1 WNb1 BPf7
    #
    # [W/B][P/K/Q/B/N/R][a1-h8]
    #
    # The first location means to place a White Bishop at C1.
    # The next a White Knight at B1.
    # And the last a Black Pawn at F7.
    #
    # Key:
    #
    # W - White
    # B - Black
    #
    # P - Pawn
    # K - King
    # Q - Queen
    # B - Bishop
    # N - Knight
    # R - Rook
    #
    # a1-h8: See Chess Algebraic Notation for understanding the
    #        chessboard grid.
    #
    class ChessBoard < Liquid::Tag
  
        def initialize(name, args, tokens)
            super
            @tokens = tokens
            @args = args

            # The eight letters that appear on a chess board.
            @alpha = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

            # The positions to be drawn on the chess board.
            @positions = args.split(" ")

        end
    
        # Checks if a piece should be placed at the passed in coord.
        # @param letter A letter a-h representing the column the piece is on.
        # @param number A number 1-8 representing the row the piece is on.
        def check_piece(letter, number)
            output = ""

            for position in @positions
                color = position[0]
                type = position[1]
                piece_letter = position[2]
                piece_number = position[3]

                if (piece_letter == letter)
                    pp "LETTER MATCH FOUND" + position
                end

                if (piece_letter == letter) && (piece_number == number.to_s())
                    if color == 'W'
                        output += "white-"
                        pp "WHITE FOUND WHITE FOUND WHITE FOUND"
                    elsif color == 'B'
                        output += "black-"

                    else
                        output += "Invalid color at " + posiiton
                        return output
                    end
                    
                    if type == "P"
                        output += "pawn"

                    elsif type == "K"
                        output += "king"
                        
                    elsif type == "Q"
                        output += "queen"
                        
                    elsif type == "R"
                        output += "rook"
                        
                    elsif type == "B"
                        output += "bishop"
                        
                    elsif type == "N"
                        output += "knight"

                    else
                        output += "Invalid type at " + posiiton
                        return output
                    end

                    return output
                end
            end

            return output
        end

        # Generates the row consisting of two corners and the letters
        # A, B, C, D, E, F, G, H.
        # @return A table row in HTML.
        def generate_letter_row()
            output = ""

            output += '<tr>'
            output += '<td class="corner"></td>'
            for letter in @alpha
                output += '<td class="letter">' + letter + '</td>'
            end
            output += '<td class="corner"></td>'
            output += '</tr>'

            return output
        end

        # Generates a numbered row on the chess board.
        # @param The number of the row (1-8).
        # @param is_odd True if the light tile should be on odd squares.
        # return A table row in HTML.
        def generate_row(number, is_odd)
            output = ""

            output += '<tr>'
            output += '<td class="number">' + number.to_s() + '</td>'

            for i in 1..8
                if is_odd
                    if i % 2 != 0
                        output += '<td class="light ' + check_piece(@alpha[i - 1], number) + '"></td>' 
                    else
                        output += '<td class="dark ' + check_piece(@alpha[i - 1], number) + '"></td>' 
                    end
                else 
                    if i % 2 != 0
                        output += '<td class="dark ' + check_piece(@alpha[i - 1], number) + '"></td>' 
                    else 
                        output += '<td class="light ' + check_piece(@alpha[i - 1], number) + '"></td>' 
                    end
                end
            end

            output += '<td class="number">' + number.to_s() + '</td>'
            output += '</tr>'

            return output
        end

        # Generates a complete chess board with pieces at the
        # correct passed in positions.
        # @return An HTML table representing the chess board.
        def generate_table()
            output = '<table class="chess">'
            output += generate_letter_row()
            output += generate_row(8,  true)
            output += generate_row(7, false)
            output += generate_row(6, true)
            output += generate_row(5, false)
            output += generate_row(4, true)
            output += generate_row(3, false)
            output += generate_row(2, true)
            output += generate_row(1, false)
            output += generate_letter_row();
            output += '</table>'

            return output
        end

        def render(context)
            super(context)
            return generate_table()
        end
  
    end

end

Liquid::Template.register_tag('chessboard', Jekyll::ChessBoard)