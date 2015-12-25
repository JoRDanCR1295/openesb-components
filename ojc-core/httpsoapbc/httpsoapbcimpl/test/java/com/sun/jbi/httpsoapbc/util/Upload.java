/* *************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/

package com.sun.jbi.httpsoapbc.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Uploads a TESTsuites.xml file to the JUnitTrack server
 *
 * The format upload is as follows:
 * PROPERTIES
 * <empty line>
 * <classname>|<name>|<time>|Q
 * <classname>|<name>|<time>|Q
 * <classname>|<name>|<time>|Q
 * ...
 * Q can be an empty string or 1 (failure), 2 (failure)
 *
 * @author Frank Kieviet
 */
public class Upload extends Task {
    private String m_URL = "http://hercules.stc.com:15001/unittracksrv-WebModule/upload";
    private String m_file;
    private String m_user;
    private String m_computer;
    private String m_module;
    private String m_branch;
    private String m_comments;
    private String m_env = "env";
    private String m_prefix;
    private String m_allowDuplicates;
    private String m_isXTest = "false";
        
    
    private String getEnv(String name) {
        String ret = getProject().getProperty(m_env + "." + name);
        if (ret == null) {
            ret = "Unavailable";
        }
        return ret;
    }
    
    private void required(String toTest, String name) {
        if (toTest == null) {
            throw new RuntimeException("Required property [" + name + "] not set");
        }
    }
    
    private Properties getProperties() {
        Properties ret = new Properties();
        
        // Adjust attributes
        m_user = m_user == null ?  getEnv("USERDOMAIN") + "\\" + getEnv("USERNAME") : m_user;
        if (m_module == null) {
            m_module = IOTools.readLine(getProject().resolveFile("./CVS/Repository"));
        }
        if (m_branch == null) {
            File f = getProject().resolveFile("./CVS/Tag");
            if (f == null || !f.exists() || (f.length() == 0)) {
                m_branch = "main";
            } else {
                m_branch = IOTools.readLine(f);
                if (m_branch != null && m_branch.startsWith("T")) {
                    m_branch = m_branch.substring(1);
                }
            }
        }
        m_computer = m_computer == null ? getEnv("COMPUTERNAME") : m_computer;
        
        // Verify
        required(m_module, "module");
        required(m_computer, "computer");
        required(m_user, "user");
        
        // Set
        ret.setProperty("user", m_user);
        ret.setProperty("module", m_module);
        ret.setProperty("computer", m_computer);
        if (m_branch != null) {
            ret.setProperty("branch", m_branch);
        }
        if (m_comments != null) {
            ret.setProperty("comments", m_comments);
        }
        if (m_prefix != null) {
            ret.setProperty("prefix", m_prefix);
        }
        ret.setProperty("version", "1.0");
        ret.setProperty("TZ-id", TimeZone.getDefault().getID());
        ret.setProperty("TZ-offset", "" + TimeZone.getDefault().getOffset(System.currentTimeMillis()));
        
        return ret;
    }
    
    private class SaxL extends DefaultHandler {
        private ArrayList mStack = new ArrayList();
        private StringBuffer mCurrent;
        private List mList;
        private int mTotalLen;
        private Map mFound = new HashMap();
        private boolean mAllowDuplicates;

        public SaxL(List list) {
            mList = list;
            mAllowDuplicates = "true".equals(m_allowDuplicates);
        }
        
        public void endElement(String uri, String localName, String qName) throws org.xml.sax.SAXException {
            if (localName.equals("testcase")) {
                mCurrent.append("\r\n");
                String line = mCurrent.toString();
                byte[] buf = line.getBytes();
                mList.add(buf);
                mTotalLen += buf.length;
                mCurrent = null;
            }
            super.endElement(uri, localName, qName);
        }

        public void startElement(String uri, String localName, String qName, org.xml.sax.Attributes attributes) throws org.xml.sax.SAXException {
            if (localName.equals("testcase")) {
                String classname = attributes.getValue("classname");
                String name = attributes.getValue("name");

                // Deal with duplicates by suffixing the number of found instances
                if (mAllowDuplicates) {
                    String compoundname = classname + "-" + name;
                    int[] found = (int[]) mFound.get(compoundname);
                    if (found == null) {
                        mFound.put(compoundname, new int[] { 1 });
                    } else {
                        name = name + "-" + found[0];
                        found[0]++;
                    }
                }
                
                mCurrent = new StringBuffer();
                mCurrent.append(classname).append("|");
                mCurrent.append(name).append("|");
                mCurrent.append((int) (Float.parseFloat(attributes.getValue("time")) * 1000)).append("|");
            } else if (localName.equals("failure")) {
                mCurrent.append("1");
            } else if (localName.equals("error")) {
                mCurrent.append("2");
            }
            super.startElement(uri, localName, qName, attributes);
        }

        public void warning(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException {
            getProject().log("SAX warning: " + e, getProject().MSG_WARN);
            super.warning(e);
        }

        public void fatalError(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException {
            getProject().log("SAX fatal: " + e, getProject().MSG_ERR);
            throw e;
        }

        public void error(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException {
            getProject().log("SAX error: " + e, getProject().MSG_ERR);
            throw e;
        }
    }
    
    private class XTestParser extends DefaultHandler {
        private ArrayList mStack = new ArrayList();
        private StringBuffer mCurrent;
        private List mList;
        private int mTotalLen;
        private Map mFound = new HashMap();
        private boolean mAllowDuplicates;

        public XTestParser(List list) {
            mList = list;
            mAllowDuplicates = "true".equals(m_allowDuplicates);
        }
        
        public void endElement(String uri, String localName, String qName) throws org.xml.sax.SAXException {
            if (localName.equals("TestBag")) {
                mCurrent.append("\r\n");
                String line = mCurrent.toString();
                byte[] buf = line.getBytes();
                mList.add(buf);
                mTotalLen += buf.length;
                mCurrent = null;
            }
            super.endElement(uri, localName, qName);
        }

        public void startElement(String uri, String localName, String qName, org.xml.sax.Attributes attributes) throws org.xml.sax.SAXException {
            if (localName.equals("TestBag")) {
                String classname = attributes.getValue("module");
                String name = attributes.getValue("name");

                // Deal with duplicates by suffixing the number of found instances
                if (mAllowDuplicates) {
                    String compoundname = classname + "-" + name;
                    int[] found = (int[]) mFound.get(compoundname);
                    if (found == null) {
                        mFound.put(compoundname, new int[] { 1 });
                    } else {
                        name = name + "-" + found[0];
                        found[0]++;
                    }
                }
                
                mCurrent = new StringBuffer();
                mCurrent.append(classname).append("|");
                mCurrent.append(name).append("|");
                mCurrent.append((int) (Float.parseFloat(attributes.getValue("time")) * 1000)).append("|");
                int fail = Integer.parseInt(attributes.getValue("testsFail"));
                int error = Integer.parseInt(attributes.getValue("testsError"));
                if (fail > 0) {
                    mCurrent.append("1");
                } else if (error > 0) {
                    mCurrent.append("2");
                }
            } else if (localName.equals("failure")) {
                mCurrent.append("1");
            } else if (localName.equals("error")) {
                mCurrent.append("2");
            }
            super.startElement(uri, localName, qName, attributes);
        }

        public void warning(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException {
            getProject().log("SAX warning: " + e, getProject().MSG_WARN);
            super.warning(e);
        }

        public void fatalError(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException {
            getProject().log("SAX fatal: " + e, getProject().MSG_ERR);
            throw e;
        }

        public void error(org.xml.sax.SAXParseException e) throws org.xml.sax.SAXException {
            getProject().log("SAX error: " + e, getProject().MSG_ERR);
            throw e;
        }
    }

    public void execute() {
        InputStream inp = null;
        OutputStream out = null;
        try {
            // Part 2: file
            required(m_file, "File");
            File f = new File(m_file);
            if (!f.exists()) {
                throw new Exception(m_file + " does not exist");
            }
            inp = new FileInputStream(f);
            
            // Read part 2: xml
            XMLReader x = XMLReaderFactory.createXMLReader();
            List records = new ArrayList();
            int totalLen = 0;
            if ("true".equalsIgnoreCase(m_isXTest)) {
                XTestParser xTestParser = new XTestParser(records);
                x.setContentHandler(xTestParser);
                x.setErrorHandler(xTestParser);
                x.parse(new InputSource(inp));
                totalLen = xTestParser.mTotalLen;
            } else {
                SaxL listener = new SaxL(records);
                x.setContentHandler(listener);
                x.setErrorHandler(listener);
                x.parse(new InputSource(inp));
                totalLen = listener.mTotalLen;
            }
            inp.close();
            inp = null;
            
            // Part 1: meta data
            Properties p = getProperties();
            p.setProperty("filedate", "" + f.lastModified());
            p.setProperty("filepath" , f.getAbsolutePath());
            p.setProperty("records", "" + records.size());
            byte[] props = (IOTools.serialize(p) + "\r\n").getBytes();

            // Total length
            long len = props.length + totalLen;

            getProject().log(this, "Dump: " + this + new String(props), getProject().MSG_VERBOSE);
            
            // Setup connection
            required(m_URL, "URL");
            URL url = new URL(m_URL);
            HttpURLConnection c = (HttpURLConnection) url.openConnection();
            c.setRequestMethod("POST");
            c.setDoOutput(true);
            c.setDoInput(true);
            c.setRequestProperty("Content-Length", "" + len);
            c.setUseCaches(false);
            
            // Send data
            out = c.getOutputStream();
            out.write(props);
            for (Iterator it = records.iterator(); it.hasNext(); ) {
                byte[] line = (byte[]) it.next();
                out.write(line);
            }
            out.flush();
            c.connect();
            
            // Read response
            if (c.getResponseCode() != 200) {
                throw new Exception("Server reply: " + c.getResponseCode()
                + "; msg = " + c.getResponseMessage());
            }
            
            c.disconnect();
            
            getProject().log(this, "Uploaded " + records.size() + " testcase results", getProject().MSG_INFO);
        } catch (Exception e) {
            throw new BuildException("Upload failed: " + e, e);
        } finally {
            IOTools.safeClose(inp);
            IOTools.safeClose(out);
        }
    }
    
  // The following is code automatically generated by GASE

  // Constructors
  /**
   * Default constructor
   */
  public Upload() {
  }

  /**
   * Parameter constructor
   */
  public Upload(String URL, String file, String user, String computer, String module, String branch, String comments, String env, String prefix, String allowDuplicates) {
    this.m_URL = URL;
    this.m_file = file;
    this.m_user = user;
    this.m_computer = computer;
    this.m_module = module;
    this.m_branch = branch;
    this.m_comments = comments;
    this.m_env = env;
    this.m_prefix = prefix;
    this.m_allowDuplicates = allowDuplicates;
  }


  // Getters and setters
  public final String getURL() {
    return m_URL;
  }
  public final void setURL(String URL) {
    this.m_URL = URL;
  }
  public final String getFile() {
    return m_file;
  }
  public final void setFile(String file) {
    this.m_file = file;
  }
  public final String getUser() {
    return m_user;
  }
  public final void setUser(String user) {
    this.m_user = user;
  }
  public final String getComputer() {
    return m_computer;
  }
  public final void setComputer(String computer) {
    this.m_computer = computer;
  }
  public final String getModule() {
    return m_module;
  }
  public final void setModule(String module) {
    this.m_module = module;
  }
  public final String getBranch() {
    return m_branch;
  }
  public final void setBranch(String branch) {
    this.m_branch = branch;
  }
  public final String getComments() {
    return m_comments;
  }
  public final void setComments(String comments) {
    this.m_comments = comments;
  }
  public final String getEnv() {
    return m_env;
  }
  public final void setEnv(String env) {
    this.m_env = env;
  }
  public final String getPrefix() {
    return m_prefix;
  }
  public final void setPrefix(String prefix) {
    this.m_prefix = prefix;
  }
  public final String getAllowDuplicates() {
    return m_allowDuplicates;
  }
  public final void setAllowDuplicates(String allowDuplicates) {
    this.m_allowDuplicates = allowDuplicates;
  }
  
  public final String getIsXTest() {
      return m_isXTest;
  }
  
  public final void setIsXTest(String isXTest) {
      this.m_isXTest = isXTest;
  }
  


  // toString
  public String toString() {
    StringBuffer ret = new StringBuffer();
    ret.append("Upload: ");
    ret.append("URL=").append(m_URL).append("; ");
    ret.append("file=").append(m_file).append("; ");
    ret.append("user=").append(m_user).append("; ");
    ret.append("computer=").append(m_computer).append("; ");
    ret.append("module=").append(m_module).append("; ");
    ret.append("branch=").append(m_branch).append("; ");
    ret.append("comments=").append(m_comments).append("; ");
    ret.append("env=").append(m_env).append("; ");
    ret.append("prefix=").append(m_prefix).append("; ");
    ret.append("allowDuplicates=").append(m_allowDuplicates);
    return ret.toString();
  }
}
