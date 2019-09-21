require 'nokogiri'

module Jekyll
  class SidenoteTag < Liquid::Tag

    def initialize(tag_name, text, tokens)
      super
      @tokens = tokens
    end

    def render(context)
      return '<div class="toc"><ul>' + generate_toc(html) + '</ul></div>'
    end
    
  end
end

Liquid::Template.register_tag('sidenote', Jekyll::TableOfContentsTag)
