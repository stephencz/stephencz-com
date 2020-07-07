require 'nokogiri'

module Jekyll

  #
  # The BreakoutBlock class is a Liquid Block class which represents that 
  # most basic form of breakout block. The purpose of a breakout block is
  # to break its content out from its parent container, and act as if it
  # it has no parent. This is useful for breaking out of responsive design
  # elements while still maintaining document flow.
  #
  # Tag Usage:
  #   {% breakout [classes] %}
  #     ...
  #   {% endbreakout %}
  #
  # Params: 
  #   classes - Additional css classes to appended to the breakout container
  #
  class BreakoutBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @classes = args.split(" ")
    end

    def render(context)
      block_body = super(context)
      return '<div class="breakout ' + @classes.join(' ') + '">' + block_body + '</div>'
    end

  end
end

Liquid::Template.register_tag('breakout', Jekyll::BreakoutBlock)
