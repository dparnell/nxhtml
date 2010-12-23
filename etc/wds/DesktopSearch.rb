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

def search_textfile (filename, re)
  file = File.open(filename)
  row = 0
  maxw = 100
  file.each_line do |line|
    row += 1
    line.chomp!
    matchdata = re.match(line)
    if matchdata
      col = matchdata.begin(0)
      if line.length > maxw
        cnd = matchdata.end(0)
        len = cnd - col
        if len > maxw
          show = "_"+line[col, maxw-3]+"_"
          part = "a"
        else
          pad = (maxw - len) / 2
          show = "_"+line[col-pad, maxw-3]+"_"
          part = "b"
        end
      else
        show = line
        part = "c"
      end
      row_col = ":"+row.to_s()+":"+col.to_s()+":"
      space = "     "[row_col.length-5..-1]
      print "  ", row_col, space, "'", show, "'\n"
      raise "Too wide!: "+part+", "+ show.length.to_s() if show.length > maxw
    end
  end
end

### Field names. See for example this:
# - System (Windows)
#   http://msdn.microsoft.com/en-us/library/ff521735(VS.85).aspx
# - Desktop Search
#   http://technet.microsoft.com/en-us/library/ff402341.aspx
# - Scripting Windows Desktop Search 3.0
#   http://technet.microsoft.com/en-us/library/ff404224.aspx
used_fields = []
used_fields.push("System.Author")
# used_fields.push("System.ContentType")
# used_fields.push("System.CopyRight")
used_fields.push("System.DateCreated")
used_fields.push("System.DateCompleted")
used_fields.push("System.FileName")
used_fields.push("System.FileDescription")
used_fields.push("System.FileExtension")
used_fields.push("System.ItemAuthors")
used_fields.push("System.ItemDate")
# used_fields.push("System.ItemFolderPathDisplay")
# used_fields.push("System.ItemName")
used_fields.push("System.ItemUrl")
used_fields.push("System.Keywords")
used_fields.push("System.MIMEType")
used_fields.push("System.Title")
# print used_fields, "\n"

query_strings = [ "cullberg", "lewis" ]
query_contains = []
query_strings.each { |i| query_contains.push("Contains('\""+i+"\"')") }

fields = used_fields.join(", ")
filter = "WHERE "
filter << query_contains.join(" AND ")
query = "SELECT " + fields + " FROM SYSTEMINDEX " + filter
# print "\nQuery: ", query, "\n\n"
print filter, "\n\n"

db = WdsServer.new
db.open
db.query(query)
field_names = db.fields
rec = WdsRecord.new(db.fields, "c:/dropbox3719833/my dropbox/psych/")
hits = db.data
db.close

# print field_names, "\n"
# print field_names.index("SYSTEM.FILEEXTENSION"), "\n"
re = Regexp.new(/schizophrenia/)
hits.each {
  |hit|
  print "--------\n"
  istxt = rec.istxt(hit)
  relurl = rec.relurl(hit)
  if istxt
    print "Text file ", relurl, " matches:\n"
    fullurl = rec.fullurl(hit)
    search_textfile(fullurl, re)
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
    print "  Title: ", title, "\n"
  end
  authors = rec.authors(hit)
  if authors
    print "  Authors: ", authors.join(", "), "\n"
  end
}

# Local variables:
# coding: utf-8
# End:
