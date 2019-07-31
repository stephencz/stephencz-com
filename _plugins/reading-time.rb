module Jekyll
  class ReadingTimeGenerator < Jekyll::Generator

    def generate(site)
      site.posts.docs.each { |post|
        post.data['word_count'] = get_word_count(post)
        post.data['reading_time'] = get_reading_time(post)
      }

    end

    def get_word_count(post)
      return post.content.split.size
    end

    def get_reading_time(post)
      return (get_word_count(post) / 256.to_f).ceil
    end
  
  end
end
