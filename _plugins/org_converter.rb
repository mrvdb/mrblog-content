require 'org-ruby'

module Jekyll

  # Define an orgmode converter
  class OrgConverter < Converter
    safe true

    def matches(ext)
      ext =~ /[.]org/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      content
    end    
  end

  # Like other converters, add a filter to convert orgmode strings to
  # html using the orgmode converter above using a liquid construction
  module Filters
    def orgify(input)
      site = @context.registers[:site]
      converter = site.getConverterImpl(Jekyll::OrgConverter)
      converter.convert(input)
    end
  end

  # Consider an org header to be a yaml_header too FIXME: Convenient,
  # but this should be changed upstream to be an array to which we can
  # add regular expressions.
  module Utils
    def has_yaml_header?(file)
      !!((File.open(file, 'rb') { |f| f.read(2) } =~ /^#\+/) or
         (File.open(file, 'rb') { |f| f.read(5) } =~ /\A---\r?\n/)) 
    end
  end

  # This overrides having to use YAML in the posts and pages
  module Convertible
    
    # Make sure we can still reach our parent
    alias :_orig_read_yaml :read_yaml

    # Override it for org files
    def read_yaml(base, name, opts = {})
      # We only process org files, call parent for others
      if name !~ /[.]org$/
        return _orig_read_yaml(base, name)
      end
      
      # Read in file and set defaults
      content = File.read(File.join(base, name), merged_file_read_opts(opts))
      self.data ||= {}
      liquid_enabled = true;

      # Read in all buffer settings in orgmode syntax and set them as value
      org_text = Orgmode::Parser.new(content, { markup_file: "html.tags.yml" })
      org_text.in_buffer_settings.each_pair do |key, value|
        # Remove #+TITLE from the buffer settings to avoid double exporting
        org_text.in_buffer_settings.delete(key) if key =~ /title/i
        buffer_setting = key.downcase

        #TODO: what to do when it is false?
        if buffer_setting == 'liquid' and value == 'false'
          liquid_enabled = false
        end

        self.data[buffer_setting] = value
      end

      # Convert the orgmode format via org-ruby to html
      self.content = org_text.to_html

      # Correct some entities so further output matching keeps working
      self.content = self.content.gsub("&#8216;","'")
      self.content = self.content.gsub("&#8217;","'")
      self.content = self.content.gsub("&#8220;",'"')
      self.content = self.content.gsub("&#8221;",'"')
      self.content = self.content.gsub("&#39;","'")

      # Make sure post excerpts are pointing to the right content
      if self.type == "post"
        self.extracted_excerpt = self.extract_excerpt
      end
       
    rescue => e
      puts "Error converting file #{File.join(base, name)}: #{e.message} #{e.backtrace}"
    end
  end

  # Document is somehow not a Convertible, so we have to do the whole thing again.
  class Document
    alias :_orig_read :read
    
    def read(opts = {})
      # We only process org files
      if path !~ /[.]org$/
        return _orig_read_yaml(base, name)
      end
      
      begin
        defaults = @site.frontmatter_defaults.all(url, collection.label.to_sym)
        unless defaults.empty?
          @data = defaults
        end
        content = File.read(path, merged_file_read_opts(opts))
        liquid_enabled = false
        org_text = Orgmode::Parser.new(content, { markup_file: "html.tags.yml" })
        org_text.in_buffer_settings.each_pair do |key, value|
          # Remove #+TITLE from the buffer settings to avoid double exporting
          org_text.in_buffer_settings.delete(key) if key =~ /title/i
          buffer_setting = key.downcase

          if buffer_setting == 'liquid'
            liquid_enabled = true
          end

          self.data[buffer_setting] = value
        end
        # Disable Liquid tags from the output by default or enabled with liquid_enabled tag
      if liquid_enabled
        self.content = org_text.to_html
        self.content = self.content.gsub("&#8216;","'")
        self.content = self.content.gsub("&#8217;", "'")
      else
        self.content = <<ORG
{% raw %}
#{org_text.to_html}
{% endraw %}
ORG
      end
        
      rescue SyntaxError => e
        puts "YAML Exception reading #{path}: #{e.message}"
      rescue Exception => e
        puts "Error reading file #{path}: #{e.message}"
      end
    end
  end
end
