require 'facets/string/titlecase'

module Jekyll
  class SeriesPageGenerator < Jekyll::Generator
    safe true

    def generate(site)
      if site.layouts.key? 'series'
        dir = site.config['series_dir'] || 'series'
        if site.data['series']
          site.data['series'].each do |series|
            site.pages << SeriesPage.new(site, site.source, dir, series['series']['url'], series['series'])
          end
        end
      end
    end
  end

  class SeriesPage < Page
    def initialize(site, base, dir, url, hash)
      @site = site
      @base = base
      @dir = dir
      @url = url
      @hash = hash

      self.process(@url)
      self.read_yaml(File.join(base, '_layouts'), 'series.html')
      self.data['title'] = @url.split(' ').map(&:capitalize).join(' ');
      self.data['series_name'] = "#{@hash['name']}"
      self.data['series_title'] = "#{@hash['name']}".titlecase
      self.data['series_description'] = @hash["description"]
      self.data['series_posts'] = @hash["posts"]
    end
  end
end
