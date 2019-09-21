require 'nokogiri'

module Jekyll
  class TableOfContentsTag < Liquid::Tag

    def initialize(tag_name, text, tokens)
      super
      @tokens = tokens
    end

    def render(context)
      html = Nokogiri::HTML.parse((context["content"]))
      return '<div class="toc"><ul>' + generate_toc(html) + '</ul></div>'
    end
           
    def generate_toc(html)
      headers = html.css('h1, h2, h3, h4, h5, h6')
      
      toc = ""
      prefix = 0.0

      if !headers.empty?
        headers.each do |header|
          depth = header.node_name[1].to_f()
          
          if depth == 1 || depth == 2
            prefix = prefix.floor()
            prefix += 1.0

            toc += create_li(header, prefix)

          elsif depth == 3
            prefix += 0.1
            toc += create_li(header, prefix.round(2))

          elsif depth == 4
            prefix += 0.01
            toc += create_li(header, prefix.round(3))

          elsif depth == 5
            toc += create_li(header, '+')

          elsif depth == 6
            toc += create_li(header, '+')

          end
        end
      end

      return toc
    end

    def create_li(header, prefix)
      return '<li class="depth-' + header.node_name + '"><a href="#' + header.attribute('id').value + '">'+ prefix.to_s + ' ' +  header.text + '</a></li>'
    end
    
  end
end

Liquid::Template.register_tag('table_of_contents', Jekyll::TableOfContentsTag)
