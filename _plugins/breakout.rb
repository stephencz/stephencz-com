require 'nokogiri'

module Jekyll

  #
  # The BreakoutBlock class is a Liquid Block class which represents that 
  # most basic form of breakout block. The purpose of a breakout block is
  # to break its content out from its parent container, and act as if it
  # it has no parent. This is useful for breaking out of responsive design
  # elements while still maintaining document flow.
  #
  # Tag Usage:
  #   {% breakout [classes] %}
  #     ...
  #   {% endbreakout %}
  #
  # Params: 
  #   classes - Additional css classes to appended to the breakout container
  #
  class BreakoutBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @classes = args.split(" ")
      @classes.delete_at(0)
    end

    def render(context)
      block_body = super(context)
      return '<div class="breakout ' + @classes.join(' ') + '">' + block_body + '</div>'
    end

  end
  
  #
  # The ControlledBreakoutBlock class is a Liquid Block class which represents
  # a more specialized version of the breakout block. A controlled breakout block doesn't
  # automatically grow to the browser window's width, but instead is centered and limited
  # to a max width.
  #
  # Tag Usage:
  #   {% controlledbreakout [width] [classes] %}
  #     ...
  #   {% endcontrolledbreakout %}
  #
  # Params: 
  #   classes - Additional css classes to appended to the breakout container
  #
  #   width -  The width is the max width of the controlled breakout. The width
  #            must be passed in as a CSS unit (Ex. 500px, 10%, 2em, etc).
  #
  class ControlledBreakoutBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @classes = args.split(" ")
      @breakout_size = @classes[0].to_s();
      @classes.delete_at(0)
    end

    def render(context)
      block_body = super(context)
      return '<div class="breakout ' + @classes.join(' ') + '"><div class="controlled-breakout" style="max-width: ' + @breakout_size + '; ">' + block_body + '</div></div>'
    end

  end

  #
  # The ContainerBreakoutBlock class is a Liquid Block class which represents
  # a normal breakout with a bootstrap container inside of it. Normally, you wouldn't
  # want or need to nest bootstrap containers. But because a breakout removes content
  # from the width constraints of its parent and takes advantage of the entire browser
  # width, nesting a container poses no issues and is useful for when you want a breakout
  # which respects the same dimensions as the outer most container.
  #
  # Tag Usage:
  #   {% containerbreakout [width] [classes] %}
  #     ...
  #   {% endcontainerbreakout %}
  #
  # Params: 
  #   classes - Additional css classes to appended to the breakout container
  #
  class ContainerBreakoutBlock < Liquid::Block

    def initialize(name, args, tokens)
      super
      @tokens = tokens
      @args = args
      @classes = args.split(" ")
      @classes.delete_at(0)
    end

    def render(context)
      block_body = super(context)
      return '<div class="breakout ' + @classes.join(' ') + '"><div class="container">' + block_body + '</div></div>'
    end

  end

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

Liquid::Template.register_tag('breakout', Jekyll::BreakoutBlock)
Liquid::Template.register_tag('controlledbreakout', Jekyll::ControlledBreakoutBlock)
Liquid::Template.register_tag('containerbreakout', Jekyll::ContainerBreakoutBlock)
Liquid::Template.register_tag('banner', Jekyll::BannerTag)