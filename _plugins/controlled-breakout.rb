require 'nokogiri'

module Jekyll
  class BreakoutBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @classes = args.split(" ")
      @type = @classes[0];
      @classes.delete_at(0)
    end

    def render(context)
      block_body = super(context)
      return '<div class="breakout ' + @classes.join(' ') + '"><div class="controlled-breakout-' + @type.to_s() + '">' + block_body + '</div></div>'
    end

  end
end

Liquid::Template.register_tag('controlledbreakout', Jekyll::BreakoutBlock)