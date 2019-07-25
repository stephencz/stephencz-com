require 'jekyll/scholar'
require 'uri'

HREF_PATTERN = Regexp.compile(['\\\\href\\\\{([^\\\\}]+)\\\\}\\\\{([^\\\\}]+)\\\\}', URI.regexp(['http', 'https', 'ftp'])].join('|'))

module Jekyll
  class Scholar
    class HRef < BibTeX::Filter
      def apply(value)
        value.to_s.gsub(URL_PATTERN) {
          if $1
            "#{$1}"
          else
            "<a href=\"#{$&}\">#{$&}</a>"
          end
        }
      end
    end
  end
end
