require 'json'

module Jekyll

  #
    # The BasicChessboard tag provides an straightforward way to represent
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
    class BasicChessboard < Liquid::Tag
  
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

                if (piece_letter == letter) && (piece_number == number.to_s())
                    if color == 'W'
                        output += "white-"
                        
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

                    elsif type == "M"
                        output += "marker"

                    else
                        output += "Invalid type at " + position
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

    #
    # The PGNParser class is capable of parsing PGN (Portable Game Notation)
    # files. A PGN file consists of two major components: Headers and Moves.
    # The parser divides these two components into a Hash. 
    #
    # Currently, the parser filters out annotations in the move set.
    #
    class PGNParser
        
        def initialize()
        end

        # Parses the PGN file at the passed in path and creates a Hash.
        # @param path A file system path to a PGN file.
        # @return A Hash the headers and moves from the PGN file.
        def parse(path)

            # Create Empty Hashes for storing PGN headers and moves
            headers = Hash.new
            moves = Hash.new

            # Open up the PGN file at the passed in path
            file = File.open(path, 'r')

            # Extract the PGN headers and store in the headers Hash
            data = file.read()
            extracted_headers = data.scan(/^[\[](.*?)]/)

            for line in extracted_headers
                line[0].gsub!(/[\"\[\]]/, "")
                divided = line[0].split(' ', 2)
                headers[divided[0]] = divided[1]
            end

            # Extract the PGN move directions and store in the moves Hash
            extracted_moves = data.gsub!(/^[\[](.*?)]/, "").strip() # Remove Headers
            extracted_moves = extracted_moves.gsub(/[0-9]*[.]/, "").strip() # Remove Move Numbers
            extracted_moves = extracted_moves.gsub(/{(.*?)}/, "").strip() # Remove Annotations
            extracted_moves = extracted_moves.split(' ') 

            # The turn the move was on.
            turn = 0

            # The color, White or Black, of the player that
            # made the move.
            player = 'W'

            counter = 1
            for move in extracted_moves
                counter += 1
                if (counter % 2) == 0
                    player = 'W'
                    turn += 1

                else
                    player = 'B'

                end

                key = player + turn.to_s()
                moves[key] = move
            end

            # Put Headers and moves Into JSON Object
            final = Hash.new

            final["headers"] = headers
            final["moves"] = moves

            return final
        end
    end

    #
    #
    #
    class Chessboard

        def initialize()
            @board = [["Rw", "Pw", "", "", "", "", "Pb", "Rb"],
                      ["Nw", "Pw", "", "", "", "", "Pb", "Nb"],
                      ["Bw", "Pw", "", "", "", "", "Pb", "Bb"],
                      ["Qw", "Pw", "", "", "", "", "Pb", "Qb"],
                      ["Kw", "Pw", "", "", "", "", "Pb", "Kb"],
                      ["Bw", "Pw", "", "", "", "", "Pb", "Bb"],
                      ["Bw", "Pw", "", "", "", "", "Pb", "Nb"],
                      ["Rw", "Pw", "", "", "", "", "Pb", "Rb"]]

        end

        # Converts a Chessboard file, i.e. a-h, into an index 0-7.
        # @param letter The file to convert into an index.
        # @return The index that correseponds to the passed in file.
        def get_letter_index(letter)
            case letter.upcase
            when 'A'
                return 0
            when 'B'
                return 1
            when 'C'
                return 2
            when 'D'
                return 3
            when 'E'
                return 4
            when 'F'
                return 5
            when 'G'
                return 6
            when 'H'
                return 7
            else
                return nil
            end
        end

        # Returns a String representing the piece at the passed in rank and file.
        # @param letter A-H
        # @param number 1-8
        # @return The piece at that location.
        def get_square(letter, number)
            return @board[get_letter_index(letter)][number - 1]
        end

        def set_square(piece, letter, number)
            @board[get_letter_index(letter)][number - 1] = piece
        end

        def make_move(key, move)
            
            # Kingside Castling
            if move == "O-O"
                kingside_castle(key)

            # Queenside Castling
            elsif move == "O-O-O"
                queenside_castle(key)

            # Pawn promotion
            elsif move.include? "="
                pawn_promotion(key, move)

            else
                # Pawn Moves

                # Captures

                # 

            end
        end

        def kingside_castle(key)
            if key.include? 'W'
                set_square('', 'e', 1)
                set_square('', 'h', 1)
                set_square('Rw', 'f', 1)
                set_square('Kw', 'g', 1)
            else 
                set_square('', 'e', 8)
                set_square('', 'h', 8)
                set_square('Rb', 'f', 8)
                set_square('Kb', 'g', 8)
            end
        end

        def queenside_castle(key)
            if key.include? 'W'
                set_square('', 'e', 1)
                set_square('', 'a', 1)
                set_square('', 'b', 1)
                set_square('Rw', 'd', 1)
                set_square('Kw', 'c', 1)
            else 
                set_square('', 'e', 8)
                set_square('', 'a', 8)
                set_square('', 'b', 8)
                set_square('Rb', 'd', 8)
                set_square('Kb', 'c', 8)
            end
        end

        def pawn_promotion(key, move)

        end
    end

    #
    # The ChessboardRenderer class provides functions for rendering
    # Chessboard objects in the form of an HTML table.
    #
    class ChessboardRenderer

        def initialize()
            @alpha = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

        end

        # Checks if a piece should be placed at the passed in coord.
        # @param letter A letter a-h representing the column the piece is on.
        # @param number A number 1-8 representing the row the piece is on.
        def check_piece(chessboard, letter, number)
            output = ""

            # Get the square from the chessboard
            square = chessboard.get_square(letter, number)

            # Get the square's piece type and the color of the piece
            piece = square[0]
            color = square[1]

            # Translate color
            if color == 'w'
                output += 'white-'
            elsif color =='b'
                output += 'black-'
            else 
                return ''
            end
            
            # Translate piece
            if piece == 'P'
                output += "pawn"
            elsif piece == 'N'
                output += "knight"
            elsif piece == 'B'
                output += "bishop"
            elsif piece == 'R'
                output += "rook"
            elsif piece == 'Q'
                output += "queen"
            elsif piece == 'M'
                output += "marker"
            elsif piece =='K'
                output += "king"
            else
                return ''
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
        def generate_row(chessboard, number, is_odd)
            output = ""

            output += '<tr>'
            output += '<td class="number">' + number.to_s() + '</td>'

            for i in 1..8
                if is_odd
                    if i % 2 != 0
                        output += '<td class="light ' + check_piece(chessboard, @alpha[i - 1], number) + '"></td>' 
                    else
                        output += '<td class="dark ' + check_piece(chessboard, @alpha[i - 1], number) + '"></td>' 
                    end
                else 
                    if i % 2 != 0
                        output += '<td class="dark ' + check_piece(chessboard, @alpha[i - 1], number) + '"></td>' 
                    else 
                        output += '<td class="light ' + check_piece(chessboard, @alpha[i - 1], number) + '"></td>' 
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
        def generate_table(chessboard)
            output = '<table class="chess">'
            output += generate_letter_row()
            output += generate_row(chessboard, 8,  true)
            output += generate_row(chessboard, 7, false)
            output += generate_row(chessboard, 6, true)
            output += generate_row(chessboard, 5, false)
            output += generate_row(chessboard, 4, true)
            output += generate_row(chessboard, 3, false)
            output += generate_row(chessboard, 2, true)
            output += generate_row(chessboard, 1, false)
            output += generate_letter_row();
            output += '</table>'

            return output
        end

        def render(chessboard)
            return generate_table(chessboard)
        end
    end

    #
    # The PGNHeader tag parse's a PGN (Portable Game Notation) file 
    # that is either passed in as an argument or set in the page.pgn
    # frontmatter variable and displays the desired header. Usage:
    #
    #   {% pgnheader [path] [header] %}
    # 
    # Optionally, to save time and make your markdown more readable
    # you can set page.pgn in the page's frontmatter. If page.pgn is
    # set you no longer have to include the [path] argument.
    #
    class PGNHeader < Liquid::Tag

        def initialize(name, args, tokens)
            super
            @tokens = tokens
            @args = args
            @tokens = args.split(" ")
        end

        def render(context)
            super(context)

            @path = context.registers[:page]['pgn']
            if @tokens.length() > 1
                @path = @tokens[0].strip()
                @header = @tokens[1].strip()
            else
                if @path != nil
                    if @tokens.length() > 0
                        @header = @tokens[0].strip()
                    else
                        return "<b>PGNHeader Error:</b> pgnheader [path] [header]<br>or set page.pgn and use pgnheader [header]!"
                    end
                else
                    return "<b>PGNHeader Error:</b> pgnheader [path] [header]<br>or set page.pgn and use pgnheader [header]!"
                end
            end

            parser = PGNParser.new()
            pgn = parser.parse(@path)

            return pgn['headers'][@header]
        end
    end

    # The PGNChessboard tag provides a way to represent chessboards and
    # games as represented in a Portable Game Notation (PGN) file.
    class PGNChessboard < Liquid::Tag

        def initialize(name, args, tokens)
            super
            @tokens = tokens
            @args = args
            @tokens = args.split(" ")
            @board = Chessboard.new()
            @boardr = ChessboardRenderer.new()
        end

        def render(context)
            super(context)

            # Handle tag argument errors and display error messages.
            @path = context.registers[:page]['pgn']
            if @tokens.length() > 1
                @path = @tokens[0].strip()
                @turn = @tokens[1].strip()
            else
                if @path != nil
                    if @tokens.length() > 0
                        @turn = @tokens[0].strip()
                    else
                        return "<b>PGNChessboard Error:</b> pgnchessboard [path] [turn]<br>or set page.pgn and use pgnchessboard [turn]!"
                    end
                else
                    return "<b>PGNChessboard Error:</b> pgnchessboard [path] [turn]<br>or set page.pgn and use pgnchessboard [turn]!"
                end
            end

            # Parse the PGN
            parser = PGNParser.new()
            pgn = parser.parse(@path)

            # Simulate the chess game up until the desired move.
            pgn['moves'].each do |key, value|
                @board.make_move(key, value)

                if key == @turn
                    break
                end
            end


            # Pass the simulated board to the ChessboardRenderer
            # and output 'rendered' HTML table.
            return @boardr.render(@board)
        end
    end
end

Liquid::Template.register_tag('chessboard', Jekyll::BasicChessboard)
Liquid::Template.register_tag('pgnchessboard', Jekyll::PGNChessboard)
Liquid::Template.register_tag('pgnheader', Jekyll::PGNHeader)