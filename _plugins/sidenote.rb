require 'nokogiri'

module Jekyll
  class SidenoteBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @direction = args.split[0].downcase.strip
    end

    def render(context)
      block_body = super(context)
      return build_marker(block_body)
    end

    # Places a sidenote marker element in the document.
    # This is used for mantaining vertical positioning.
    def build_marker(block_body)
      return '<div class="sn-marker-' +  @direction + '">' + block_body + '</div>'
    end 

  end
end

Liquid::Template.register_tag('sidenote', Jekyll::SidenoteBlock)
