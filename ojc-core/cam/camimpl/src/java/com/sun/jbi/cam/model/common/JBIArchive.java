/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)JBIArchive.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

/**
 * @author ylee
 * @author Graj
 *
 */

/**
 * This class reads the jar file and identifies the jbi artifacts
 * like jbi descriptors.
 */
public class JBIArchive {

    /**
     * jar file
     */
    private JarFile mJarFile;

    /**
     * jbi descriptor
     */
    private JBIDescriptor mJbiDescriptor;

    /**
     * Creates a new instance of JBIArchive
     * @param pathName path name
     * @throws IOException on error
     */
    public JBIArchive(String pathName) throws Exception {
        this(new File(pathName));
    }

    /**
     * Creates a new instance of JBIArchive
     * @param file file
     * @throws IOException on error
     */
    public JBIArchive(File file) throws Exception {
        this(new JarFile(file));
    }

    /**
     * Creates a new instance of JBIArchive
     * @param jarFile jar file
     * @throws IOException on error
     */
    public JBIArchive(JarFile jarFile) throws Exception {
        this.mJarFile = jarFile;
        validate();
    }

    /**
     * fix it
     * @throws IOException on error
     */
    private void validate() throws Exception {
        // TODO schema validate the jbi.xml on tools side in JbiDescriptor
        // throws exception if it does not find the jbi.xml in /META-INF or not
        // valid according to JBIDescriptor.
        try {
            JBIDescriptor jbiDD = getJbiDescriptor();
        } catch (Exception ioEx) {
            // rethrow
            throw ioEx;
        }/* catch (Exception ex) {
            throw new IOException(ex.getMessage());
        }*/
    }

    /**
     * fix it
     * @throws IOException on error
     * @return jar file
     */
    public JarFile getJarFile() throws IOException {
        return this.mJarFile;
    }

    /**
     * fix it
     * @return jbi
     * @throws Exception on error
     */
    public JBIDescriptor getJbiDescriptor() throws Exception {
        if (this.mJbiDescriptor == null) {
            JarEntry jbiXmlEntry = this.mJarFile.getJarEntry("META-INF/jbi.xml");
            if (jbiXmlEntry != null) {
                InputStreamReader reader = null;
                try {
                    reader = new InputStreamReader(this.mJarFile.getInputStream(jbiXmlEntry));
                    this.mJbiDescriptor = JBIDescriptor.createJBIDescriptor(reader);
                } finally {
                    if (reader != null) {
                        try {
                            reader.close();
                        } catch (Exception ex) {
                            // ignore
                        }
                    }
                    try {
                        this.mJarFile.close();
                    } catch (Exception ex) {
                        // ignore
                    }
                }
            } else {
                throw new IOException("jbi.xml not found in META-INF directory.");
            }
        }
        return this.mJbiDescriptor;
    }

    public Properties getFactoryConfigProperties(File file) throws Exception {
        StringBuffer buffer = new StringBuffer("");
        ByteArrayOutputStream stream = null;
        InputStreamReader reader = null;
        Properties properties = null;
        if((file != null) && file.exists() && file.isFile()) {
            ZipFile zip = new ZipFile(file);
            //System.out.println("Opening Stream File: "+file.getAbsolutePath());
            try {
                // Open the ZIP file
                ZipInputStream in = new ZipInputStream(new FileInputStream(file.getAbsolutePath()));

                // Get the first entry
                ZipEntry entry = in.getNextEntry();
                if(entry != null) {
                    //System.out.println("Entry: "+entry.getName()+"; Size: "+entry.getSize());
//                    while((entry != null) && (entry.getName().endsWith("jbi2.xml") == false)) {
                    while((entry != null) && (entry.getName().endsWith("jbi.xml") == false)) {
                        entry  = in.getNextEntry();
                        if(entry != null) {
                            //System.out.println("Entry: "+entry.getName()+"; Size: "+entry.getSize());
                        }
                    }
                }
//                if((entry != null) && (entry.getName().endsWith("jbi2.xml") == true)) {
                if((entry != null) && (entry.getName().endsWith("jbi.xml") == true)) {
                    // Transfer bytes from the ZIP file to the output file
                    int len = 0;
                    int size = (int)entry.getSize();
                    if(size == -1) {
                        size = 10240;
                    }
                    byte[] buf = new byte[size];
                    // Open the output buffer
                    stream = new ByteArrayOutputStream(size);
                    while((len = in.read(buf)) > 0) {
                        stream.write(buf, 0, len);
                    }
                    buffer.append(stream.toString("UTF-8"));
                }
                // Close the stream
                if(stream != null) {
                    stream.close();
                }
                if(in != null) {
                    in.close();
                }
                String xmlString = buffer.toString();
                if(xmlString.length() > 5) {
                    properties = JBIDescriptor.getFactoryConfigProperties(xmlString);
                }
            } catch (IOException e) {
            }

          try {
          } finally {
              if (reader != null) {
                  try {
                      reader.close();
                  } catch (Exception ex) {
                      // ignore
                  }
              }
          }
        }
        return properties;
    }

    public Properties getIdentificationProperties(File file) throws Exception {
        StringBuffer buffer = new StringBuffer("");
        ByteArrayOutputStream stream = null;
        InputStreamReader reader = null;
        Properties properties = null;
        if((file != null) && file.exists() && file.isFile()) {
            ZipFile zip = new ZipFile(file);
            //System.out.println("Opening Stream File: "+file.getAbsolutePath());
            try {
                // Open the ZIP file
                ZipInputStream in = new ZipInputStream(new FileInputStream(file.getAbsolutePath()));

                // Get the first entry
                ZipEntry entry = in.getNextEntry();
                if(entry != null) {
                    //System.out.println("Entry: "+entry.getName()+"; Size: "+entry.getSize());
                    while((entry != null) && (entry.getName().endsWith("jbi.xml") == false)) {
                        entry  = in.getNextEntry();
                        //if(entry != null) {
                            //System.out.println("Entry: "+entry.getName()+"; Size: "+entry.getSize());
                        //}
                    }
                }
                if((entry != null) && (entry.getName().endsWith("jbi.xml") == true)) {
                    buffer = new StringBuffer("");
                    // Transfer bytes from the ZIP file to the output file
                    int len = 0;
                    int size = (int)entry.getSize();
                    if(size == -1) {
                        size = 10240;
                    }
                    byte[] buf = new byte[size];
                    // Open the output buffer
                    stream = new ByteArrayOutputStream(size);
                    while((len = in.read(buf)) > 0) {
                        stream.write(buf, 0, len);
                    }
                    buffer.append(stream.toString("UTF-8"));
                }
                // Close the stream
                if(stream != null) {
                    stream.close();
                }
                if(in != null) {
                    in.close();
                }
                String xmlString = buffer.toString();
                if(xmlString.length() > 5) {
                    properties = JBIDescriptor.getIdentificationProperties(xmlString);
                }
            } catch (IOException e) {
            }

          try {
          } finally {
              if (reader != null) {
                  try {
                      reader.close();
                  } catch (Exception ex) {
                      // ignore
                  }
              }
          }
        }
        return properties;
    }

    /**
     * fix it
     * @return true if it is namespace
     */
    public boolean isSharedLibraryArchive() {
        try {
            JBIDescriptor jbiDesc = getJbiDescriptor();
            return (jbiDesc != null && jbiDesc.isSharedLibraryDescriptor());
        } catch (Exception ex) {
            return false;
        }
    }

    /**
     * fix it
     * @return true if it is namespace
     */
    public boolean isServiceAssemblyArchive() {
        try {
            JBIDescriptor jbiDesc = getJbiDescriptor();
            return (jbiDesc != null && jbiDesc.isServiceAssemblyDescriptor());
        } catch (Exception ex) {
            return false;
        }
    }

    /**
     * fix it
     * @return true if it is namespace
     */
    public boolean isServiceEngineArchive() {
        try {
            JBIDescriptor jbiDesc = getJbiDescriptor();
            return (jbiDesc != null && jbiDesc.isServiceEngineDescriptor());
        } catch (Exception ex) {
            return false;
        }
    }

    /**
     * fix it
     * @return true if it is namespace
     */
    public boolean isBindingComponentArchive() {
        try {
            JBIDescriptor jbiDesc = getJbiDescriptor();
            return (jbiDesc != null && jbiDesc.isBindingComponentDescriptor());
        } catch (Exception ex) {
            return false;
        }
    }

    public static void main(String[] args) {
        StringBuffer buffer = new StringBuffer("<?xml version=\"1.0\"?><jbi version=\"1.0\" xmlns=\"http://java.sun.com/xml/ns/jbi\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://java.sun.com/xml/ns/jbi ./jbi.xsd\"><!-- identification information about this engine --><component type=\"service-engine\"><identification><name>bpelserviceengine-9bfbff60-467d-11d9-9669-0800200c9a66</name><description>This is a bpel service engine.</description></identification><component-class-name description=\"LifeCycleMBean\">com.sun.bpms.jbiadapter.BPELSELifeCycle</component-class-name><component-class-path><path-element>lib/jbiadapter.jar</path-element><path-element>lib/bpelcore.jar</path-element><path-element>lib/jxpath.jar</path-element><path-element>lib/wsdl4j.jar</path-element><path-element>lib/log4j-1.2.8.jar</path-element><path-element>lib/resolver.jar</path-element></component-class-path><bootstrap-class-name>com.sun.bpms.jbiadapter.BPELSEBootstrap</bootstrap-class-name><bootstrap-class-path><path-element>lib/jbiadapter.jar</path-element></bootstrap-class-path></component></jbi>");
        Properties properties = null;

        try {
            properties = JBIDescriptor.getFactoryConfigProperties(buffer.toString());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
