require 'facets/string/titlecase'

module Jekyll
  class SeriesPageGenerator < Jekyll::Generator
      safe true

      def generate(site)
          if site.layouts.key? 'series'
              dir = site.config['series_dir'] || 'series'
              site.data['series'].each do |series|
                  site.pages << SeriesPage.new(site, site.source, dir, series['name'].downcase(), series)
              end
          end
      end
  end

  class SeriesPage < Page
      def initialize(site, base, dir, series, hash)
        @site = site
        @base = base
        @dir = dir
        @hash = hash
        @name =  series.gsub(" ", "-") + '.html'

        self.process(@name)
        self.read_yaml(File.join(base, '_layouts'), 'series.html')
        self.data['title'] = series.split(' ').map(&:capitalize).join(' ');
        self.data['series_name'] = "#{series}"
        self.data['series_title'] = "#{series}".titlecase
        self.data['series_description'] = @hash["description"]
      end
  end
end
