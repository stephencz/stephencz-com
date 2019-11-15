require 'nokogiri'

module Jekyll
  class BannerTag < Liquid::Tag

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @url = args.split[0].downcase.strip
      @classes = args.split(" ")
      @classes.delete_at(0)
    end

    def render(context)
      return '<div class="breakout banner ' + @classes.join(' ') + '"><p>![](' + @url + ')</p></div>'
    end

  end
end

Liquid::Template.register_tag('banner', Jekyll::BannerTag)
