require 'nokogiri'

module Jekyll
  class BreakoutBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @url = args.split[0].downcase.strip
      @classes = args.split(" ")
      @classes.delete_at(0)
    end

    def render(context)
      block_body = super(context)
      return '<div class="breakout ' + @classes.join(' ') + '">' + block_body + '</div>'
    end

  end
end

Liquid::Template.register_tag('breakout', Jekyll::BreakoutBlock)