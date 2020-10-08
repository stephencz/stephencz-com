module Jekyll
  class CategoryPageGenerator < Jekyll::Generator
      safe true

      def generate(site)
          if site.layouts.key? 'category'
              dir = site.config['category_dir'] || 'categories'
              site.categories.each_key do |category|
                  site.pages << CategoryPage.new(site, site.source, dir, category)
              end
          end
      end
  end

  class CategoryPage < Page
      def initialize(site, base, dir, category)
          @site = site
          @base = base
          @dir = dir
          @name =  category.downcase.gsub(" ", "-") + '.html'

          self.process(@name)
          self.read_yaml(File.join(base, '_layouts'), 'category.html')
          self.data['title'] = category.split(' ').map(&:capitalize).join(' ');
          self.data['category'] = category
          self.data['category-name'] = "#{category}"
      end
  end
end
