require 'nokogiri'

module Jekyll
  class SidenoteBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @direction = args.split[0]
      @classes = args.split.drop(1)
      @counter = 0;
    end

    def render(context)
      body = super(context)
      
      #Increment the sidenote counter
      counter += 1


      return build_marker(counter)
    end

    def build_sidenote()

    end
    
    def build_marker(counter)
      return '<div class="sidenote-marker" id="sn-' + counter + '"></div>'
    end 
  end
end

Liquid::Template.register_tag('sidenote', Jekyll::SidenoteBlock)
