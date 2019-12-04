require 'nokogiri'

module Jekyll
  class ContrasterStartTag < Liquid::Tag

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @classes = args.split(" ")
    end

    def render(context)
      return '<div class="contraster-start-marker ' + @classes.join(' ') + '"></div>'
    end

  end

  class ContrasterEndTag < Liquid::Tag

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
    end

    def render(context)
      return '<div class="contraster-end-marker"></div>'
    end

  end
end

Liquid::Template.register_tag('contraster', Jekyll::ContrasterStartTag)
Liquid::Template.register_tag('endcontraster', Jekyll::ContrasterEndTag)
