require 'org-ruby'

# Augment String class with a to_b method : converts sensibly to boolean
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
      ext =~ /[.]org$/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      org_text = Orgmode::Parser.new(content, {markup_file: "html.tags.yml" })
      org_text.in_buffer_settings.delete("TITLE")  # We already captured the title  
      org_text.to_html
    end

    def self.process_options(content, data)
      org_text = Orgmode::Parser.new(content, {markup_file: "html.tags.yml" })
      
      org_text.in_buffer_settings.each_pair do |key, value|
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
  
  # Handle Convertibles: Posts and Pages
  module Convertible
    # Override the read_yaml method to take org buffer settings instead
    alias :_orig_read_yaml :read_yaml

    def read_yaml(base, name, opts = {})
      # We only process org files, call parent for other
      if name !~ /[.]org$/
        return _orig_read_yaml(base, name)
      end
      
      # Read in file and set defaults
      self.content = File.read(File.join(base, name), merged_file_read_opts(opts))
      self.data ||= {}

      # Read in all buffer settings in orgmode syntax and set them as value
      self.data = OrgConverter::process_options(content, self.data)
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
        return _orig_read(base, name)
      end
      
      begin
        defaults = @site.frontmatter_defaults.all(url, collection.label.to_sym)
        unless defaults.empty?
          @data = defaults
        end
        self.content = File.read(path, merged_file_read_opts(opts))
        @data = OrgConverter::process_options(content, @data)
      end
        
    rescue SyntaxError => e
      puts "YAML Exception reading #{path}: #{e.message}"
    rescue Exception => e
      puts "Error reading file #{path}: #{e.message}"
    end
  end
end
