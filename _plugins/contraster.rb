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
      @classes = args.split(" ")
    end

    def render(context)
      return '<div class="contraster-end-marker ' + @classes.join(' ') + '"></div>'
    end

  end
end

Liquid::Template.register_tag('contraster', Jekyll::ContrasterStartTag)
Liquid::Template.register_tag('endcontraster', Jekyll::ContrasterEndTag)

module Jekyll
  class ContrasterLimitedStartTag < Liquid::Tag

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @color = args.split[0].downcase.strip
      @classes = args.split(" ")
      @classes.delete_at(0)
    end

    def render(context)
      return '<div class="contraster-start-marker limited-contraster ' + @classes.join(' ') + '" style="background-color: ' + @color.to_s() + ';"></div>'
    end

  end

  class ContrasterLimitedEndTag < Liquid::Tag

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @classes = args.split(" ")
    end

    def render(context)
      return '<div class="contraster-end-marker ' + @classes.join(' ') + '"></div>'
    end

  end
end

Liquid::Template.register_tag('contrasterlimited', Jekyll::ContrasterLimitedStartTag)
Liquid::Template.register_tag('endcontrasterlimited', Jekyll::ContrasterLimitedEndTag)
