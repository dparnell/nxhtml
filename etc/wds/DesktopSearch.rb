# -*- coding: iso-8859-1 -*-
require 'win32ole'
require 'pathname'


class WdsRecord
  # This class extracts record data
  def initialize(fields, rootpath)
    @fields = fields
    @tit_num = @fields.index("SYSTEM.TITLE")
    @auth_num = @fields.index("SYSTEM.AUTHOR")
    @itemauth_num = @fields.index("SYSTEM.ITEMAUTHORS")
    @filext_num = @fields.index("SYSTEM.FILEEXTENSION")
    @itemurl_num = @fields.index("SYSTEM.ITEMURL")
    @txt_ext = [".txt", ".org"]
    @root_path = Pathname.new(rootpath)
  end
  def fullurl (hit)
    url = hit[@itemurl_num]
    return url[5..-1]
  end
  def relurl (hit)
    url = hit[@itemurl_num]
    url = url[5..-1]
    this_path = Pathname.new(url)
    relurl = this_path.relative_path_from(@root_path)
    return relurl
  end
  def title (hit)
    return hit[@tit_num]
  end
  def authors (hit)
    return hit[@auth_num] || hit[@itemauth_num]
  end
  def istxt (hit)
    ext = hit[@filext_num]
    return ext && @txt_ext.index(ext)
  end
end

class WdsServer
  # This class manages database connection and queries
  attr_accessor :connection, :data, :fields

  def initialize
    @connection = nil
    @data = nil
  end

  def open
    # Open ADO connection to the WDS Server database
    connection_string =  "Provider=Search.CollatorDSO;"
    connection_string << "Extended Properties='Application=Windows';"
    @connection = WIN32OLE.new('ADODB.Connection')
    @connection.Open(connection_string)
  end

  # fix-me: Split:
  def query(sql)
    # Create an instance of an ADO Recordset
    recordset = WIN32OLE.new('ADODB.Recordset')
    # Open the recordset, using an SQL statement and the
    # existing ADO connection
    recordset.Open(sql, @connection)
    # Create and populate an array of field names
    @fields = []
    recordset.Fields.each do |field|
      @fields << field.Name
    end
    begin
      # Move to the first record/row, if any exist
      recordset.MoveFirst
      # Grab all records
      @data = recordset.GetRows
    rescue
      @data = []
    end
    recordset.Close
    # An ADO Recordset's GetRows method returns an array 
    # of columns, so we'll use the transpose method to 
    # convert it to an array of rows
    @data = @data.transpose
  end

  def close
    @connection.Close
  end
end

def search_textfile (filename, re, maxw)
  # See this blog posts about char encoding:
  #   http://blog.grayproductions.net/articles/ruby_19s_three_default_encodings
  file = File.open(filename, "r:UTF-8")
  row = 0
  maxw = maxw - 12
  # print "maxw=", maxw.to_s(), "\n"
  file.each_line do |line|
    row += 1
    line.chomp!
    matchdata = re.match(line)
    if matchdata
      col = matchdata.begin(0)
      cnd = matchdata.end(0)
      if line.length > maxw
        len = cnd - col
        if len > maxw
          show = "_"+line[col, maxw-3]+"_"
          part = "a"
        else
          pad = (maxw - len) / 2
          # print "col="+col.to_s()+", cnd="+cnd.to_s()+", pad="+pad.to_s()+"\n"
          start = col-pad
          start = 0 if start < 0
          show = "_"+line[start, maxw-3]+"_"
          part = "b"
        end
      else
        show = line
        part = "c"
      end
      #row_col = "L:"+row.to_s()+":"+col.to_s()+":"
      row_col = part+":"+row.to_s()+":"+col.to_s()+":"
      space = "     "[row_col.length-6..-1]
      print "", row_col, space, " ", show, "\n"
      raise "Too wide!: "+part+", "+ show.length.to_s() if show.length > maxw
    end
  end
end

class QueryWds
  # This class answers queries
  def initialize(rootpath)
    @rootpath = rootpath
    ### Field names. See for example this:
    # - System (Windows)
    #   http://msdn.microsoft.com/en-us/library/ff521735(VS.85).aspx
    # - Desktop Search
    #   http://technet.microsoft.com/en-us/library/ff402341.aspx
    # - Scripting Windows Desktop Search 3.0
    #   http://technet.microsoft.com/en-us/library/ff404224.aspx
    @used_fields = []
    @used_fields.push("System.Author")
    # @used_fields.push("System.ContentType")
    # @used_fields.push("System.CopyRight")
    @used_fields.push("System.DateCreated")
    @used_fields.push("System.DateCompleted")
    @used_fields.push("System.FileName")
    @used_fields.push("System.FileDescription")
    @used_fields.push("System.FileExtension")

    # Fix-me: fulltext gives error, maybe need to not fetch all at once?
    # The error happens on this line.
    #
    # recordset.Open(sql, @connection)
    #
    # @used_fields.push("System.FullText")

    @used_fields.push("System.ItemAuthors")
    @used_fields.push("System.ItemDate")
    # @used_fields.push("System.ItemFolderPathDisplay")
    # @used_fields.push("System.ItemName")
    @used_fields.push("System.ItemUrl")
    @used_fields.push("System.Keywords")
    @used_fields.push("System.MIMEType")
    @used_fields.push("System.Title")
    # print used_fields, "\n"
  end

  def query_wds(comma_sep_query)
    query_strings = comma_sep_query.split(",")

    query_contains = []
    query_strings.each { |i| query_contains.push("Contains('\""+i+"\"')") }

    fields = @used_fields.join(", ")
    filter = "WHERE "
    filter << query_contains.join(" AND ")
    query = "SELECT " + fields + " FROM SYSTEMINDEX " + filter
    # print "\nQuery: ", query, "\n\n"
    print filter, "\n\n"

    db = WdsServer.new
    db.open
    db.query(query)
    field_names = db.fields
    rec = WdsRecord.new(db.fields, @rootpath)
    hits = db.data
    db.close

    # print field_names, "\n"
    # print field_names.index("SYSTEM.FILEEXTENSION"), "\n"
    re_str = "("+query_strings.join("|")+")"
    re = Regexp.new(re_str, 1)
    maxw = 100 # fix-me
    emacs_w = ENV["EMACS-COMPILE-WINDOW-WIDTH"]
    maxw = emacs_w.to_i() if emacs_w
    hits.each {
      |hit|
      print "\n"
      istxt = rec.istxt(hit)
      relurl = rec.relurl(hit)
      if istxt
        print "Text file ", relurl, " matches:\n"
        fullurl = rec.fullurl(hit)
        search_textfile(fullurl, re, maxw)
      else
        print "Binary file ", relurl, " matches\n"
      end
      if nil
        for fn in 0..field_names.length-1
          val = hit[fn]
          if val
            print "  ", field_names[fn], "=", val, "\n"
          end
        end
      end
      title = rec.title(hit)
      if title
        print "  Title:   ", title, "\n"
      end
      authors = rec.authors(hit)
      if authors
        print "  Authors: ", authors.join(", "), "\n"
      end
    }
    print "----\n"
    print "Found ", hits.length.to_s(), " items"
  end
end

if __FILE__ == $0
  root = ARGV[0]
  query = ARGV[1]

  # print "root=", root, "\n"
  # print "query=", query, "\n"
  query_wds = QueryWds.new(root)
  query_wds.query_wds(query)
end

# Local variables:
# coding: utf-8
# End:
