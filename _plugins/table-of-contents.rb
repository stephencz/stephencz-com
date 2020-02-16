require 'nokogiri'

module Jekyll
  class TableOfContents < Liquid::Tag
    def initialize(tag_name, text, tokens)
      super
      @text = text
    end

    def render(context)
      output = ""

      # Get header tags and create TOCHeader objects
      nokogiri_tags = get_header_tags(context)
      formatted_tags = get_formatted_header_tags(nokogiri_tags)

      # Open list
      output << "<ul>"

      #Join list items
      output << formatted_tags.join("\n")

      # Close list
      output << "</ul>"

      return output
    end

    # Retrieves the H1 - H6 tags from the page's content.
    # NOTE: This array will NOT contain the pages title.
    def get_header_tags(context)

      headers = []

      content =  context["content"]
      doc = Nokogiri::HTML(content)

      return doc.css("h1, h2, h3, h4, h5, h6")
    end

    # Takes in an array of Nokogiri header tags and returns
    # An array of properly formatted list items.
    def get_formatted_header_tags(tags)
      formatted_tags = []

      tags.each do |tag|

        formatted_tag = ''

        # Open list item tag
        formatted_tag << '<li '
        formatted_tag << 'class="'
        formatted_tag << 'depth-'
        formatted_tag << tag.name
        formatted_tag << '">'

        # Open attribute tag
        formatted_tag << '<a '
        formatted_tag << 'href="#'
        formatted_tag << tag.attributes["id"].value
        formatted_tag << '">'

        # Insert text in attribute tag
        formatted_tag << tag.children[0].text

        #Close attribute tag
        formatted_tag << "</a>"

        #Close list item tag
        formatted_tag << '</li>'


        formatted_tags.push(formatted_tag)
      end

      return formatted_tags
    end

  end
end

Liquid::Template.register_tag('table_of_contents', Jekyll::TableOfContents)