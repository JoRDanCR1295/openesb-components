 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.utils;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.Reader;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.logging.Level;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
   Utility Class for File manipulation
 * @author <a href="mailto:gvaleri@imolinfo.it">Giancarlo Valeri</a>
 */
public class HelperFileUtil {

    
     /**
     * The system dependent line separator string.
     */
    private static final String LINE_SEPARATOR = System.getProperty("line.separator");

    
     /**
     * Logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(HelperFileUtil.class);
    
    public static final int ENABLE=0;
    public static final int DISABLE=1;
    
    
    /**
     * Import to exclude
     */
    private static final String ORBFNAME = "orb.idl";
    private static final String IRFNAME = "ir.idl";

    
    /**
     * Reads file content as a <code>String</code>.
     *
     * @param   f  the file to read. Must be not <code>null</code>.
     * @return  a <code>String</code> representing <code>f</code> content, with
     *          lines separated by the system dependent terminator string. The
     *          returned <code>String</code> is never <code>null</code>, but it
     *          may be empty (<code>""</code>).
     * @throws  IOException  if the file does not exist, is a directory rather
     *                       than a regular file, for some other reason cannot
     *                       be opened for reading or an I/O error occurs.
     */
    public  static String readFileAsString(final File f)
            throws IOException {
        BufferedReader file = null;

        try {
            StringBuilder buf = new StringBuilder();
            file = new BufferedReader(new FileReader(f));

            buf.append(LINE_SEPARATOR);
            for (String s = file.readLine(); s != null; s = file.readLine()) {
                buf.append(s).append(LINE_SEPARATOR);
            }
            return buf.toString();
        } finally {
            if (file != null) {
                file.close();
            }
        }
    }
    
    
    /**
     * Reads WSDL file and return IDL as a <code>String</code>.
     *
     * @param   f  the WSDL file to read. Must be not <code>null</code>.
     * @return  a <code>String</code> representing <code>IDL</code> content. The
     *          returned <code>String</code> is never <code>null</code>, but it
     *          may be empty (<code>""</code>).
     * @throws  IOException  if the file does not exist, is a directory rather
     *                       than a regular file, for some other reason cannot
     *                       be opened for reading or an I/O error occurs.
     */
    public  static String readIdlFromFile(final File wsdlFile)
            throws IOException {
            File file = wsdlFile;
            String idl="";
        try {
            
            
           DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
           DocumentBuilder mBuilder;        
        
           mBuilder = factory.newDocumentBuilder();                        
           Document document = mBuilder.parse(wsdlFile);
            
           NodeList ndl = document.getElementsByTagName( "imolacorba:idl" );
            
           if (ndl.getLength() !=0 )
           {
        	   idl=ndl.item(0).getTextContent();
           }
           
            
            
        } catch (SAXException ex) {
            java.util.logging.Logger.getLogger(HelperFileUtil.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ParserConfigurationException ex) {
            java.util.logging.Logger.getLogger(HelperFileUtil.class.getName()).log(Level.SEVERE, null, ex);
        } 
        
        return idl;
    }
    
   /**
     * Write Inpustream on File
     *
     * @param   InputStream Must be not <code>null</code>.
     * @return  a <code>String</code> representing <code>IDL</code> content. The
     *          returned <code>String</code> is never <code>null</code>, but it
     *          may be empty (<code>""</code>).
     * @throws  IOException  if the file does not exist, is a directory rather
     *                       than a regular file, for some other reason cannot
     *                       be opened for reading or an I/O error occurs.
     */
    public  static void writeToFile(InputStream in, File file) throws FileNotFoundException, IOException {
        
        OutputStream out = new FileOutputStream(file);
    
        // Transfer bytes from in to out
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
		
    }
    
    /**
     * Copy a file. 
     * @param in
     * @param out
     * @throws Exception
     */
    public static void copyFile(File in, File out) throws IOException {
        FileInputStream fis  = new FileInputStream(in);
        FileOutputStream fos = new FileOutputStream(out);
        try {
            byte[] buf = new byte[1024];
            int i = 0;
            while ((i = fis.read(buf)) != -1) {
                fos.write(buf, 0, i);
            }
        }       
        finally {
            if (fis != null) fis.close();
            if (fos != null) fos.close();
        }
      } 
    
    /**
     * Copy a file from an URL 
     * @param in
     * @param out
     * @throws Exception
     */
    public static void copyFile(URL in, File out) throws IOException {
        InputStream fis  = in.openStream();
        FileOutputStream fos = new FileOutputStream(out);
        try {
            byte[] buf = new byte[1024];
            int i = 0;
            while ((i = fis.read(buf)) != -1) {
                fos.write(buf, 0, i);
            }
        }       
        finally {
            if (fis != null) fis.close();
            if (fos != null) fos.close();
        }
      }  
    
    
    /**
     * Remove same specified include from the Idl File passed as argument
     * Process Idl starting from file URL and comments all #include orb.idl or ir.idl
     * @param idlUrl
     */
    public static void changeIDLIncludeStaus(final URL idlUrl,final int type) throws FileNotFoundException, URISyntaxException, IOException{
        File idlFile=new File(idlUrl.toURI());
        Reader r=new FileReader(idlFile);
        LineNumberReader reader = new LineNumberReader(r);
        StringBuilder buffer=new StringBuilder();
        String line;
            while((line=reader.readLine())!=null){
        
			if(type==DISABLE){
                            if (line.startsWith("#include") && (line.contains(ORBFNAME) || line.contains(IRFNAME))) {
                                buffer.append("//"+line+LINE_SEPARATOR);
                            } else{
                                buffer.append(line+LINE_SEPARATOR);
                            }      
                        }else{
                             if (line.startsWith("//#include") && (line.contains(ORBFNAME) || line.contains(IRFNAME))) {
                                buffer.append(line.substring(2)+LINE_SEPARATOR);
                            } else{
                                buffer.append(line+LINE_SEPARATOR);
                            }   
                        }
                        
            }
         reader.close();
         r.close();
         ByteArrayInputStream bais=new ByteArrayInputStream(buffer.toString().getBytes());
         writeToFile(bais, idlFile);
     }
			
   
}

