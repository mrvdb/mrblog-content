require 'org-ruby'


# Augment String with a to_b method : converts sensibly to boolean
class String
  def to_b
    return true if self =~ (/^(true|t|yes|y|1)$/i)
    return false if self.empty? || self =~ (/^(false|f|no|n|0)$/i)

    raise ArgumentError.new "invalid value: #{self}"
  end
end

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

    def self.quote_replace(content)
      content = content.gsub("&#8216;","'") #&lsquo;
      content = content.gsub("&#8217;","'") #&rsquo;
      content = content.gsub("&#8220;",'"') #&ldquo;
      content = content.gsub("&#8221;",'"') #&rdquo;
      content = content.gsub("&#39;"  ,"'") #&apos;
    end

    def self.process_options(org_text, data)
      org_text.in_buffer_settings.each_pair do |key, value|
        # Remove #+TITLE from the buffer settings to avoid double rendering
        org_text.in_buffer_settings.delete(key) if key =~ /title/i
        # We need true/false as booleans, not string. 
        if key.downcase == 'published' #(any others?)
          value = value.to_b
        end
        data[key.downcase] = value
      end
      data
    end
  end

  # Like other converters, add a filter to convert orgmode strings to
  # html using the orgmode converter above using a liquid construction
  # NOTE: I have a hard time envisioning how this would be used.
  module Filters
    def orgify(input)
      site = @context.registers[:site]
      converter = site.getConverterImpl(Jekyll::OrgConverter)
      converter.convert(input)
    end
  end

  # Consider an org header to be a yaml_header too, so the files get processed by Jekyll
  # FIXME: This should be changed upstream to be a list or regexps to which we can add things.
  module Utils
    def has_yaml_header?(file)
      !!((File.open(file, 'rb') { |f| f.read(2) } =~ /^#\+/) or
         (File.open(file, 'rb') { |f| f.read(5) } =~ /\A---\r?\n/)) 
    end
  end
  
  # Handle included org files
  module Tags
    class IncludeTag
      alias :_orig_read_file :read_file
      
      def read_file(file, context)
        if file !~ /[.]org$/
          return _orig_read_file(file, context)
        end

        # Read contents of the included file        
        content = File.read(file, file_read_opts(context))

        # Parse it
        org_text = Orgmode::Parser.new(content, { markup_file: "html.tags.yml" })

        # FIXME: Do we want/need to process the buffer settings?
        # Convert the orgmode format via org-ruby to html
        OrgConverter::quote_replace(org_text.to_html)
      end
    end
  end

  # Handle Convertibles: Posts and Pages
  module Convertible
    # Override the read_yaml method to take org buffer settings instead
    alias :_orig_read_yaml :read_yaml

    def read_yaml(base, name, opts = {})
      # We only process org files, call parent for others
      if name !~ /[.]org$/
        return _orig_read_yaml(base, name)
      end
      
      # Read in file and set defaults
      content = File.read(File.join(base, name), merged_file_read_opts(opts))
      self.data ||= {}

      # Read in all buffer settings in orgmode syntax and set them as value
      org_text = Orgmode::Parser.new(content, { markup_file: "html.tags.yml" })
      self.data = OrgConverter::process_options(org_text, self.data)
    
      # Convert the orgmode format via org-ruby to html
      self.content = OrgConverter::quote_replace(org_text.to_html)

      # Make sure post excerpts are pointing to the right content
      # FIXME: this doesn't seem to work properly
      if self.type == "posts"
        puts self.extract_excerpt
        self.extracted_excerpt = self.extract_excerpt
      end
       
    rescue => e
      puts "Error converting file #{File.join(base, name)}: #{e.message} #{e.backtrace}"
    end
  end

  # Document is somehow not a Convertible, so we have to do the whole thing again.
  # The class is a bit different though
  class Document
    alias :_orig_read :read
    
    def read(opts = {})
      # We only process org files
      if path !~ /[.]org$/
        return _orig_read(base, name)
      end
      
      begin
        defaults = @site.frontmatter_defaults.all(url, collection.label.to_sym)
        unless defaults.empty?
          @data = defaults
        end
        content = File.read(path, merged_file_read_opts(opts))
        
        org_text = Orgmode::Parser.new(content, { markup_file: "html.tags.yml" })
        @data = OrgConverter::process_options(org_text, @data)
        
        # Convert the orgmode format via org-ruby to html
        self.content = OrgConverter::quote_replace(org_text.to_html)
      end
        
    rescue SyntaxError => e
      puts "YAML Exception reading #{path}: #{e.message}"
    rescue Exception => e
      puts "Error reading file #{path}: #{e.message}"
    end
  end
end
